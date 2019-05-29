{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Persist.MPTree
  ( MPTreeDB (..)
  , newMPTreeDB
  , getStateRoot
  , getModifier
  , persister
  ) where

import           Prelude hiding (log)

import           Control.Monad.Reader ()
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Pact.Persist hiding (compileQuery)
import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Sealchain.Mpt.MerklePatriciaMixMem
import           Pact.Types.Logger hiding (Logging (..))
import           Pact.Types.Pretty (viaShow)

data MPTreeDB p = MPTreeDB
  { _mpdb         :: !(MPDB p)
  , _modifier     :: !MMModifier
  , _workMpdb     :: !(MPDB p)
  , _workModifier :: !MMModifier -- | modifier used in transaction
  , _logger       :: Logger
  }

newMPTreeDB :: p -> StateRoot -> MMModifier -> Loggers -> MPTreeDB p
newMPTreeDB p sr modifier loggers = 
  let mpdb = MPDB p sr
  in MPTreeDB {
      _mpdb = mpdb,
      _modifier = modifier,
      _workMpdb = mpdb,
      _workModifier = modifier,
      _logger = newLogger loggers "Persist-MPTree"
     }

getStateRoot :: MPTreeDB p -> StateRoot
getStateRoot = stateRoot . _mpdb

getModifier :: MPTreeDB p -> MMModifier 
getModifier = _modifier

log :: MPTreeDB p -> String -> String -> IO ()
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

persister :: KVPersister p => Persister (MPTreeDB p)
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

createTable_ :: KVPersister p => Table k -> MPTreeDB p -> IO (MPTreeDB p, ())
createTable_ table mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  log mptDB "DDL" $ "createTable: " ++ show table
  let tableKey = getTableKey table
  tableRootM <- getKeyValMixMem workMpdb workModifier tableKey 
  case tableRootM of
    Just _  -> throwDbError $ "Table already exists: " <> viaShow table 
    Nothing -> do
      (newStateRoot, modifier') <- putKeyValMixMem workMpdb workModifier tableKey $ 
                                 unboxStateRoot emptyTriePtr
      return (mptDB {_workMpdb = workMpdb{stateRoot=newStateRoot}, _workModifier=modifier'}, ())

unsafeGetTableRoot :: KVPersister p => Table k -> MPTreeDB p -> IO B.ByteString
unsafeGetTableRoot table (MPTreeDB _ _ workMpdb workModifier _) = do
  let tableKey = getTableKey table
  tableRootM <- getKeyValMixMem workMpdb workModifier tableKey 
  case tableRootM of
    Just tableRoot -> return tableRoot
    Nothing        -> throwDbError $ "Table does not exists: " <> viaShow table

beginTx_ :: MPTreeDB p -> IO (MPTreeDB p, ())
beginTx_ mptDB@(MPTreeDB mpdb modifier _ _ _) = 
  return (mptDB {_workMpdb = mpdb, _workModifier = modifier}, ())

commitTx_ :: MPTreeDB p -> IO (MPTreeDB p, ())
commitTx_ mptDB@(MPTreeDB _ _ workMpdb workModifier _) =
  return (mptDB {_mpdb = workMpdb, _modifier = workModifier}, ()) 

rollbackTx_ :: MPTreeDB p -> IO (MPTreeDB p,())
rollbackTx_ mptDB@(MPTreeDB mpdb modifier _ _ _) = 
  return (mptDB {_workMpdb = mpdb, _workModifier = modifier}, ()) 

readValue_ :: (PactKey k, PactValue v, KVPersister p) => Table k -> k -> MPTreeDB p -> IO (MPTreeDB p, (Maybe v))
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

writeValue_ :: (PactKey k, PactValue v, KVPersister p) => Table k -> WriteType -> k -> v -> MPTreeDB p -> IO (MPTreeDB p,())
writeValue_ table wt pactKey pactValue mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  let tableKey = getTableKey table
  tableRoot <- unsafeGetTableRoot table mptDB

  let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
  let rowKey = newRowKey table pactKey 
  let bsKey = Bi.serialize' rowKey

  rowValM <- getKeyValMixMem tableMpdb workModifier bsKey
  case (wt, rowValM) of
    (Insert, Just _)  -> throwDbError $ "Key already exists: " <> viaShow pactKey
    (Update, Nothing) -> throwDbError $ "Key does not exists: " <> viaShow pactKey
    _                 -> do
      (newTableRoot, modifier') <- putKeyValMixMem tableMpdb workModifier bsKey $ encodePactVal pactValue
      (newStateRoot, modifier'') <- putKeyValMixMem workMpdb modifier' tableKey $ unboxStateRoot newTableRoot
      return (mptDB {_workMpdb = workMpdb{stateRoot=newStateRoot}, _workModifier=modifier''}, ())

queryKeys_ :: (PactKey k, KVPersister p) => Table k -> Maybe (KeyQuery k) -> MPTreeDB p -> IO (MPTreeDB p,[k])
queryKeys_ table kq mptDB@(MPTreeDB _ _ workMpdb workModifier _) = do
  tableRoot <- unsafeGetTableRoot table mptDB
  let tableMpdb = workMpdb {stateRoot = StateRoot tableRoot}
  keyVals <- getAllKeyValsMixMem tableMpdb workModifier
  let keys = map (_rkPactKey . justRight . Bi.decodeFull' . fst) keyVals
  let filterFunc = compileQuery kq
  return (mptDB, filter filterFunc keys)

query_ :: (PactKey k, PactValue v, KVPersister p) => Table k -> Maybe (KeyQuery k) -> MPTreeDB p -> IO (MPTreeDB p,[(k,v)])
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
