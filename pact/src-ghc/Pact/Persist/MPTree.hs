{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Persist.MPTree
  (
    Tbl(..),tbl,
    Tables(..),tbls,
    Db(..),dataTables,txTables,
    rootMPDB,tableStateRoot,
    MPtreeDb(..),temp,commitMpdb,
    initMPtree,
    persister,
    usersMap,ketsetsMap,modulesMap
  ) where

import Control.Lens hiding (op)
import Data.Aeson
import Control.Monad.Reader ()
import Control.Monad.State
import Data.Default
-- import Data.Typeable
import Data.Semigroup (Semigroup)

import Seal.Contract.Persist hiding (compileQuery)

import           Seal.Mpt.MerklePatricia
import           Seal.Mpt.MerklePatricia.Utils
import           Seal.RocksDB (DB (..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL

import qualified Seal.Util.Modifier as MM
import qualified Data.Map.Strict as M
import Data.Text.Encoding
import Prelude


--MM.Modifyer 保存某张表的修改  
data Tbl k = Tbl {
  _tbl :: MM.MapModifier k B.ByteString,
  _tableStateRoot :: StateRoot
  } deriving (Show)
makeLenses ''Tbl

newtype Tables k = Tables {
  _tbls :: MM.MapModifier (Table k) (Tbl k)
  } deriving (Show,Semigroup,Monoid)
makeLenses ''Tables

-- 多余？
data Db = Db {
  _dataTables :: !(Tables DataKey),
  _txTables :: !(Tables TxKey),
  _usersMap   :: M.Map Text B.ByteString,
  _ketsetsMap :: M.Map Text B.ByteString,
  _modulesMap :: M.Map Text B.ByteString
  } deriving (Show)
makeLenses ''Db
instance Default Db where def = Db mempty mempty mempty mempty mempty

tblType :: Table k -> Lens' Db (Tables k)
tblType DataTable {} = dataTables
tblType TxTable {} = txTables --todo 是否会有影响

data MPtreeDb = MPtreeDb {
  _temp :: !Db,
  _rootMPDB :: MPDB           
  }
makeLenses ''MPtreeDb

initMPtree :: MPDB -> MPtreeDb
initMPtree mpdb = MPtreeDb def mpdb

persister :: Persister MPtreeDb
persister = Persister {
  -- 判断mptree中是否有table,没有则创建，同时创建一个modifyer，如果有，则直接创建modifyer
  createTable = \t s -> createTable_ t s,
  beginTx = \_ s -> beginTx_ s
  ,
  -- modifyer 值 提交到mptree 
  commitTx = \s -> commitTx_ s
  ,
  rollbackTx = \s -> rollbackTx_ s
  ,
  -- queryKeys = \t kq s -> (s,) . map fst <$> qry t kq s
  queryKeys = \t kq s -> queryKeys_ t kq s
  ,
  -- query = \t kq s -> fmap (s,) $ qry t kq s >>= mapM (\(k,v) -> (k,) <$> conv v)
  query = \t kq s -> query_ t kq s
  ,
  readValue = \t k s -> readValue_ t k s
  ,
  writeValue = \t wt k v s -> writeValue_ t wt k v s
  ,
  refreshConn = return . (,())
  }

-- 判断mptree中是否有table,没有则创建，同时创建一个modifyer，如果有，则直接创建modifyer
createTable_ :: PactKey k => Table k -> MPtreeDb -> IO (MPtreeDb,())
createTable_ t s = do
  let tables = view (temp . tblType t . tbls) s
  let  baseGetter :: PactKey k => Table k ->  IO (Maybe (Tbl k))
       baseGetter tb = do
        --table k 转换成mpkey
        let mk = tableKey2MPKey tb 
        -- 根据table k 查询对应的table stateroot
        tid <- getKeyVal (_rootMPDB s) mk 
        -- mpVal to  tal k
        return $ mpValToTbl tid
  mtbl <- MM.lookupM baseGetter t tables
  tbs  <- case mtbl of
            Nothing -> return $ MM.insert t (Tbl mempty emptyTriePtr) tables
            Just _ -> return tables
            -- Just _ ->  throwDbError $ "createTable: already exists: " ++ show t
  let ns = set (temp . tblType t . tbls) tbs s
  return $ (ns,())

--初始化mptree,创建tables树
beginTx_ :: MPtreeDb -> IO (MPtreeDb,())
beginTx_ s = return $ (,()) $ s

commitTx_ :: MPtreeDb -> IO (MPtreeDb,())
commitTx_ s = do
  let tables = _tbls . _dataTables . _temp $ s
  let    ls  = MM.insertions tables
  root <- setMptreeTables ls (_rootMPDB s)
  -- 更新最外层StateRoot
  let ss = set rootMPDB root s
  -- 清空内存DB
  let ns = set temp def ss
  return $ (ns,())

commitMpdb :: MPtreeDb -> IO MPtreeDb
commitMpdb s = do
  (mp,_) <- commitTx_ s
  return mp

-- reset db to def
rollbackTx_ :: MPtreeDb -> IO (MPtreeDb,())
rollbackTx_ s = 
  return $ (MPtreeDb def (_rootMPDB s),()) 

--先从modifyer中读取，没有，则从mptree中读取
readValue_ :: (PactKey k, PactValue v) => Table k -> k -> MPtreeDb -> IO (MPtreeDb,(Maybe v))
readValue_ t k s = do
  --获取tables
  let tables = view (temp . tblType t . tbls) s
  --通过key，获取对应的value
  --mptree-tables中获取具体的table
  let  baseGetter :: PactKey k => Table k ->  IO (Maybe (Tbl k))
       baseGetter tb = do
        --table k 转换成mpkey
        let mk = tableKey2MPKey tb 
        -- 根据table k 查询对应的table stateroot
        tid <- getKeyVal (_rootMPDB s) mk 
        -- mpVal to  tal k
        return $ mpValToTbl tid
  mtbl <- MM.lookupM baseGetter t tables
  mpv <- case mtbl of 
          --从mptree中读取 
          Nothing     -> throwDbError $ "readValue: no such table: " ++ show t
          Just table  -> do
            let valueGetter :: PactKey k => k -> IO (Maybe B.ByteString)
                valueGetter key = do
                  --将k转换成mpkey
                  let mpkey = pactKey2MPKey key
                  let tMpdb = _rootMPDB s
                  -- tal k 转换成mpdb
                  let mpdb = MPDB {rdb=(rdb tMpdb),stateRoot=(_tableStateRoot table)}
                  --获取对应value
                  val <- getKeyVal mpdb mpkey
                  return val
            MM.lookupM valueGetter k (_tbl table)
  -- ByteString to PactValue 
  let v = mpValToPv mpv          
  return $ (s, v)

--writeValue MM中没有表，判断mptree中是否有，有的话创建一个MM,再写入MM
writeValue_ :: (PactKey k, PactValue v) => Table k -> WriteType -> k -> v -> MPtreeDb -> IO (MPtreeDb,())
writeValue_ t wt k v s = do
  let tables = view (temp . tblType t . tbls) s
  let  baseGetter :: PactKey k => Table k ->  IO (Maybe (Tbl k))
       baseGetter tb = do
        --table k 转换成mpkey
        let mk = tableKey2MPKey tb 
        -- 根据table k 查询对应的table stateroot
        tid <- getKeyVal (_rootMPDB s) mk 
        -- mpVal to  tal k
        return $ mpValToTbl tid
  mtbl <- MM.lookupM baseGetter t tables
  tb <- case mtbl of 
    --从mptree中读取 
    Nothing     -> throwDbError $ "readValue: no such table: " ++ show t
    Just table  -> do
      let valueGetter :: PactKey k => k -> IO (Maybe B.ByteString)
          valueGetter key = do
            --将k转换成mpkey
            let mpkey = pactKey2MPKey key
            let tMpdb = _rootMPDB s
            -- tal k 转换成mpdb
            let mpdb = MPDB {rdb=(rdb tMpdb),stateRoot=(_tableStateRoot table)}
            --获取对应value
            val <- getKeyVal mpdb mpkey
            return val
      mpv <- MM.lookupM valueGetter k (_tbl table)
      ntbl <- case (mpv,wt) of 
              --  (Just _,Insert) -> throwDbError $ "Insert: value already at key: " ++ show k
               (Just _,Insert) -> return $ _tbl table
               (Nothing,Update) -> throwDbError $ "Update: no value at key: " ++ show k
               _ -> return $ MM.insert k (asByteString v) (_tbl table)
      return $ set tbl ntbl table
  --更新MM中tables
  let newTables = MM.insert t tb tables
  let ns = set (temp . tblType t . tbls) newTables s
  return $ (ns,())   

-- 目前先查询全部keys
queryKeys_ :: PactKey k => Table k -> Maybe (KeyQuery k) -> MPtreeDb -> IO (MPtreeDb,[k])
queryKeys_ t kq s = do
  let tables = view (temp . tblType t . tbls) s
  let  baseGetter :: PactKey k => Table k ->  IO (Maybe (Tbl k))
       baseGetter tb = do
        --table k 转换成mpkey
        let mk = tableKey2MPKey tb 
        -- 根据table k 查询对应的table stateroot
        tid <- getKeyVal (_rootMPDB s) mk 
        -- mpVal to  tal k
        return $ mpValToTbl tid
  mtbl <- MM.lookupM baseGetter t tables
  mkeys <- case mtbl of 
    --从mptree中读取 
    Nothing     -> throwDbError $ "readValue: no such table: " ++ show t
    Just table  -> do
      let keysGetter :: PactKey k => IO [k]
          keysGetter = do
            let tMpdb = _rootMPDB s
            -- tal k 转换成mpdb
            let mpdb = MPDB {rdb=(rdb tMpdb),stateRoot=(_tableStateRoot table)}
            --获取对应value
            val <- getAllKeyVals mpdb
            let keys = map fst val
            let dataKeys = map mpKey2PackKey keys
            return $ dataKeys
      MM.keysM keysGetter (_tbl table)
  let fKey = filter (compileQuery kq) mkeys
  return $ (s,fKey)

-- 先查询全部(key,value)
query_ :: (PactKey k, PactValue v) => Table k -> Maybe (KeyQuery k) -> MPtreeDb -> IO (MPtreeDb,[(k,v)])
query_ t kq s = do
  let tables = view (temp . tblType t . tbls) s
  let  baseGetter :: PactKey k => Table k ->  IO (Maybe (Tbl k))
       baseGetter tb = do
        --table k 转换成mpkey
        let mk = tableKey2MPKey tb 
        -- 根据table k 查询对应的table stateroot
        tid <- getKeyVal (_rootMPDB s) mk 
        -- mpVal to  tal k
        return $ mpValToTbl tid
  mtbl <- MM.lookupM baseGetter t tables
  mkeys <- case mtbl of 
    --从mptree中读取 
    Nothing     -> throwDbError $ "readValue: no such table: " ++ show t
    Just table  -> do
      let getter :: PactKey k => IO [(k,B.ByteString)]
          getter = do
            let tMpdb = _rootMPDB s
            -- tal k 转换成mpdb
            let mpdb = MPDB {rdb=(rdb tMpdb),stateRoot=(_tableStateRoot table)}
            --获取对应value
            val <- getAllKeyVals mpdb
            kv  <- forM val $ \(k,v) -> do
              let k1 = mpKey2PackKey k
              return (k1,v)
            return $ kv
      MM.toListM getter (_tbl table)
  ks <- forM mkeys $ \(k,v) -> do
    let v1 = decode . BSL.fromStrict $ v
    case v1 of 
      Nothing -> throwDbError $ "decode value failed,value : " ++ show v
      Just vv -> return (k,vv)
  let fKs = filter (compileQuery kq . fst) ks
  return $ (s,fKs)


mpValToPv :: (PactValue v) => Maybe B.ByteString -> Maybe v
mpValToPv (Just v)  = decode . BSL.fromStrict $ v
mpValToPv Nothing = Nothing

mpValToTbl :: PactKey k => Maybe MPVal -> Maybe (Tbl k)
mpValToTbl = fmap(\p -> Tbl {_tbl=mempty, _tableStateRoot=StateRoot p})

--B.tail 去掉第一个空格
mpKey2PackKey :: PactKey k => MPKey -> k
mpKey2PackKey key = fromByteString bs
  where bs = B.tail $ termNibbleString2String True key



pactKey2MPKey :: PactKey k => k -> MPKey
pactKey2MPKey k = bytesToNibbleString $ toByteString k

tableKey2MPKey :: Table k -> MPKey
tableKey2MPKey (DataTable t) = bytesToNibbleString $ sanitize t
tableKey2MPKey (TxTable t) = bytesToNibbleString $ sanitize t

sanitize :: TableId -> B.ByteString
sanitize (TableId t) = encodeUtf8 t

--将tables插入mptree
setMptreeTables :: [(Table DataKey,Tbl DataKey)] -> MPDB -> IO MPDB
setMptreeTables ((k,v):xs) mpdb = do
  let mpKey = tableKey2MPKey k
  let values = MM.insertions (_tbl v)
  tid <- getKeyVal mpdb mpKey
  tsr <- case tid of 
    Nothing -> setMPtreeValues values (MPDB {rdb=(rdb mpdb),stateRoot=emptyTriePtr})
    Just tv -> setMPtreeValues values (mpVal2MPDB (rdb mpdb) tv)
  nst <- putKeyVal mpdb mpKey (mpdb2MPval tsr)
  setMptreeTables xs nst
setMptreeTables [] mpdb = return mpdb


-- 将modifyer中的值插入table mptree
setMPtreeValues :: [(DataKey,B.ByteString)] -> MPDB -> IO MPDB
setMPtreeValues ((k,v):xs) mpdb = do
  let mpKey = dataKey2MPKey k
  rs <- putKeyVal mpdb mpKey v
  --更新stateroot
  setMPtreeValues xs rs
setMPtreeValues [] mpdb = return mpdb  

--Text -ByteString
dataKey2MPKey :: DataKey -> MPKey
dataKey2MPKey (DataKey k) = bytesToNibbleString $ encodeUtf8 k


asByteString :: ToJSON v => v -> B.ByteString
asByteString = BSL.toStrict . encode

mpdb2MPval :: MPDB -> MPVal
mpdb2MPval (MPDB _ (StateRoot sr)) = sr

mpVal2MPDB :: DB -> MPVal -> MPDB
mpVal2MPDB db pval  = MPDB {rdb=db,stateRoot=(StateRoot pval)}

--queryKeys 先从map里面查，再从mptree里面查？

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

-- qry :: Table k -> Maybe (KeyQuery k) -> MPtreeDb -> IO [(k,PValue)]
-- qry t kq s = case firstOf (temp . tblType t . tbls . ix t . tbl) s of
--   Nothing -> throwDbError $ "query: no such table: " ++ show t
--   Just m -> return $ filter (compileQuery kq . fst) $ M.toList m
-- qry _ _ _ = undefined

-- {-# INLINE qry #-}

-- _test :: IO ()
-- _test = do
--   e <- initMPtreeDb "/tmp/contract"
--   let p = persister
--       dt = DataTable "22222"
--       -- tt = TxTable "tx"
--       run f = do
--         s <- get
--         (s',r) <- liftIO (f s)
--         put s'
--         return r
--   (`evalStateT` e) $ do
--     run $ beginTx p True
--     run $ createTable p dt

    -- run $ createTable p tt
    -- run $ commitTx p
    -- run $ beginTx p True
    -- run $ writeValue p dt Insert "stuff1" (String "hello")
    -- run $ writeValue p dt Insert "tough" (String "goodbye")

    -- run $ writeValue p tt Write 1 (String "txy goodness")
    -- run $ writeValue p tt Insert 2 (String "txalicious")
    -- run $ commitTx p
    -- run $ writeValue p dt Insert "stuff1" (String "hello")
    -- run $ writeValue p dt Insert "stuff1" (String "hello")

    -- run $ createTable p dt

    -- run (readValue p dt "tough") >>= (liftIO . (print :: Maybe Text -> IO ()))
    -- run (readValue p dt "stuff") >>= (liftIO . (print :: Maybe Value -> IO ()))

    -- run (query p dt (Just (KQKey KEQ "stuff"))) >>=
    --   (liftIO . (print :: [(DataKey,Value)] -> IO ()))
    -- run (queryKeys p dt (Just (KQKey KGTE "stuff"))) >>= liftIO . print
    -- run (query p tt (Just (KQKey KGT 0 `kAnd` KQKey KLT 2))) >>=
    --   (liftIO . (print :: [(TxKey,Value)] -> IO ()))
    -- run $ beginTx p True
    -- run $ writeValue p tt Update 2 (String "txalicious-2!")
    -- run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
    -- run $ rollbackTx p
    -- run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
