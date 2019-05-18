{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Persist.MPTree
  ( MPTreeDB(..)
  , initMPTreeDB
  , persister
  ) where

import           Prelude hiding (log)

import           Control.Monad.Reader ()
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Pact.Persist hiding (compileQuery)
import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Sealchain.Mpt.MerklePatriciaMixMem
import           Pact.Types.Logger hiding (Logging (..))

data MPTreeDB = MPTreeDB 
  { _mpdb         :: !MPDB
  , _modifier     :: !MMModifier
  , _workMpdb     :: !MPDB
  , _workModifier :: !MMModifier -- | modifier used in transaction
  , _logger       :: Logger
  }

initMPTreeDB :: MPDB -> Loggers -> MPTreeDB
initMPTreeDB mpdb loggers = 
  let logger = newLogger loggers "Persist-MPTree"
  in MPTreeDB mpdb Map.empty mpdb Map.empty logger

log :: MPTreeDB -> String -> String -> IO ()
log e = logLog (_logger e)

data RowKey k = RowKey 
    { _rkTableId :: Text
    , _rkPactKey :: k
    }

instance (PactKey k) => Bi (RowKey k) where
  encode RowKey{..} = Bi.encodeListLen 2
                   <> Bi.encode (encodeUtf8 _rkTableId)
                   <> Bi.encode (toByteString _rkPactKey)

  decode = do
      Bi.enforceSize "RowKey" 2
      _rkTableId <- decodeUtf8 <$> Bi.decode
      _rkPactKey <- fromByteString <$> Bi.decode
      return RowKey{..} 

persister :: Persister MPTreeDB
persister = Persister 
  { createTable = \t s -> createTable_ t s
  , beginTx = \_ s -> beginTx_ s
  , commitTx = \s -> commitTx_ s
  , rollbackTx = \s -> rollbackTx_ s
  , queryKeys = \t kq s -> queryKeys_ t kq s
  , query = \t kq s -> query_ t kq s
  , readValue = \t k s -> readValue_ t k s
  , writeValue = \t wt k v s -> writeValue_ t wt k v s
  , refreshConn = return . (,())
  }

createTable_ :: Table k -> MPTreeDB -> IO (MPTreeDB, ())
createTable_ table mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  log mptDB "DDL" $ "createTable: " ++ show table
  let tableKey = getTableKey table
  tableRootM <- getKeyValMixMem workMpdb workModifier tableKey 
  case tableRootM of
    Just _  -> throwDbError $ "Table already exists: " ++ show table 
    Nothing -> do
      (newStateRoot, modifier') <- putKeyValMixMem workMpdb workModifier tableKey $ 
                                 unboxStateRoot emptyTriePtr
      return (mptDB {_workMpdb = workMpdb{stateRoot=newStateRoot}, _workModifier=modifier'}, ())

unsafeGetTableRoot :: Table k -> MPTreeDB -> IO B.ByteString
unsafeGetTableRoot table (MPTreeDB _ _ workMpdb workModifier _) = do
  let tableKey = getTableKey table
  tableRootM <- getKeyValMixMem workMpdb workModifier tableKey 
  case tableRootM of
    Just tableRoot -> return tableRoot
    Nothing        -> throwDbError $ "Table does not exists: " ++ show table

beginTx_ :: MPTreeDB -> IO (MPTreeDB, ())
beginTx_ mptDB@(MPTreeDB mpdb modifier _ _ _) = 
  return (mptDB {_workMpdb = mpdb, _workModifier = modifier}, ())

commitTx_ :: MPTreeDB -> IO (MPTreeDB, ())
commitTx_ mptDB@(MPTreeDB _ _ workMpdb workModifier _) =
  return (mptDB {_mpdb = workMpdb, _modifier = workModifier}, ()) 

rollbackTx_ :: MPTreeDB -> IO (MPTreeDB,())
rollbackTx_ mptDB@(MPTreeDB mpdb modifier _ _ _) = 
  return (mptDB {_workMpdb = mpdb, _workModifier = modifier}, ()) 

readValue_ :: (PactKey k, PactValue v) => Table k -> k -> MPTreeDB -> IO (MPTreeDB, (Maybe v))
readValue_ table packKey mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  tableRoot <- unsafeGetTableRoot table mptDB
  let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
  let rowKey = newRowKey table packKey 
  let bsKey = Bi.serialize' rowKey
  valM <- getKeyValMixMem tableMpdb workModifier bsKey
  case valM of
    Nothing  -> return (mptDB, Nothing)
    Just val -> do
      let pactValue = decodePactVal val 
      return (mptDB, Just pactValue)

writeValue_ :: (PactKey k, PactValue v) => Table k -> WriteType -> k -> v -> MPTreeDB -> IO (MPTreeDB,())
writeValue_ table wt pactKey pactValue mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  let tableKey = getTableKey table
  tableRoot <- unsafeGetTableRoot table mptDB

  let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
  let rowKey = newRowKey table pactKey 
  let bsKey = Bi.serialize' rowKey

  rowValM <- getKeyValMixMem tableMpdb workModifier bsKey
  case (wt, rowValM) of
    (Insert, Just _)  -> throwDbError $ "Key already exists: " ++ show pactKey
    (Update, Nothing) -> throwDbError $ "Key does not exists: " ++ show pactKey
    _                 -> do
      (newTableRoot, modifier') <- putKeyValMixMem tableMpdb workModifier bsKey $ encodePactVal pactValue
      (newStateRoot, modifier'') <- putKeyValMixMem workMpdb modifier' tableKey $ unboxStateRoot newTableRoot
      return (mptDB {_workMpdb = workMpdb{stateRoot=newStateRoot}, _workModifier=modifier''}, ())

queryKeys_ :: (PactKey k) => Table k -> Maybe (KeyQuery k) -> MPTreeDB -> IO (MPTreeDB,[k])
queryKeys_ table kq mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  tableRoot <- unsafeGetTableRoot table mptDB
  let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
  keyVals <- getAllKeyValsMixMem tableMpdb workModifier
  let keys = map (_rkPactKey . justRight . Bi.decodeFull' . fst) keyVals
  let filterFunc = compileQuery kq
  return (mptDB, filter filterFunc keys)

query_ :: (PactKey k, PactValue v) => Table k -> Maybe (KeyQuery k) -> MPTreeDB -> IO (MPTreeDB,[(k,v)])
query_ table kq mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
    tableRoot <- unsafeGetTableRoot table mptDB
    let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
    keyVals <- getAllKeyValsMixMem tableMpdb workModifier
    let pactKeyVals = map keyVals2PactKeyVals keyVals
    let filterFunc = compileQuery kq
    return (mptDB, filter (filterFunc . fst) pactKeyVals)
  where
    keyVals2PactKeyVals (mpKey, mpVal) = 
      (_rkPactKey . justRight . Bi.decodeFull' $ mpKey, decodePactVal mpVal) 

newRowKey :: Table k -> k -> RowKey k
newRowKey (DataTable (TableId tid)) k = RowKey tid k
newRowKey (TxTable (TableId tid)) k = RowKey tid k
{-# INLINE newRowKey #-}

getTableKey :: Table k -> B.ByteString
getTableKey table = 
  let (TableId tid) = tableId table 
  in encodeUtf8 tid
{-# INLINE getTableKey #-}

encodePactVal :: PactValue a => a -> B.ByteString
encodePactVal = BSL.toStrict . encode
{-# INLINE encodePactVal #-}

decodePactVal :: (PactValue v) => B.ByteString -> v
decodePactVal bs = 
  case (decode $ BSL.fromStrict bs) of
    Nothing  -> error "Data courrpution!"  
    Just val -> val
{-# INLINE decodePactVal #-}

compileQuery :: PactKey k => Maybe (KeyQuery k) -> (k -> Bool)
compileQuery Nothing = const True
compileQuery (Just kq) = compile kq
  where
    compile (KQKey cmp k) = (`op` k)
      where op = case cmp of
              KGT -> (>)
              KGTE -> (>=)
              KEQ -> (==)
              KNEQ -> (/=)
              KLT -> (<)
              KLTE -> (<=)
    compile (KQConj l o r) = conj o <$> compile l <*> compile r
    conj AND = (&&)
    conj OR = (||)
{-# INLINE compileQuery #-}
