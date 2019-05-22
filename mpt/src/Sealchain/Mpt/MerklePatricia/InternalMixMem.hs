{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.InternalMixMem (
  MMModifier,
  MixMemMode,
  runMixMemMode,
  unsafePutKeyValMixMem,
  unsafeGetKeyValsMixMem,
  unsafeGetAllKeyValsMixMem,
  unsafeDeleteKeyMixMem,
  ) where

import           Universum
import           Universum.Unsafe ((!!))

import           Control.Monad.Trans (MonadIO)
import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.NibbleString as N

import qualified Pos.Binary.Class as Bi

import           Sealchain.Mpt.MerklePatricia.MPDB
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.StateRoot
import           Sealchain.Mpt.MerklePatricia.Utils

type MMModifier = Map.Map B.ByteString B.ByteString

type MixMemMode p m = ReaderT (MPDB p) (StateT MMModifier m)

runMixMemMode::MPDB p->MMModifier->MixMemMode p m a->m (a,MMModifier)
runMixMemMode db modifier action = runStateT (runReaderT action db) modifier

unsafePutKeyValMixMem::(MonadIO m,KVPersister p)=>MPKey->MPVal->MixMemMode p m StateRoot
unsafePutKeyValMixMem key val = do
  db <- ask
  dbNodeData <- getNodeData (PtrRef $ unboxStateRoot $ stateRoot db)
  dbPutNodeData <- putKV_NodeData key val dbNodeData
  StateRoot <$> putNodeData dbPutNodeData

unsafeGetKeyValsMixMem::(MonadIO m,KVPersister p)=>MPKey->MixMemMode p m [(MPKey, MPVal)]
unsafeGetKeyValsMixMem key = do
  db <- ask
  let dbNodeRef = PtrRef $ unboxStateRoot $ stateRoot db
  getKeyVals_NodeRef dbNodeRef key 

unsafeGetAllKeyValsMixMem::(MonadIO m,KVPersister p)=>MixMemMode p m [(MPKey, MPVal)]
unsafeGetAllKeyValsMixMem = unsafeGetKeyValsMixMem N.empty

unsafeDeleteKeyMixMem::(MonadIO m,KVPersister p)=>MPKey->MixMemMode p m StateRoot
unsafeDeleteKeyMixMem key = do
  db <- ask
  dbNodeData <- getNodeData (PtrRef $ unboxStateRoot $ stateRoot db)
  dbDeleteNodeData <- deleteKey_NodeData key dbNodeData
  StateRoot <$> putNodeData dbDeleteNodeData

putKV_NodeData::(MonadIO m,KVPersister p)=>MPKey->MPVal->NodeData->MixMemMode p m NodeData

putKV_NodeData key val EmptyNodeData =
  return $ ShortcutNodeData key (Right val)

putKV_NodeData key val (FullNodeData options nodeValue)
  | options `slotIsEmpty` N.head key =
    do
      tailNode <- newShortcut (N.tail key) $ Right val
      return $ FullNodeData (replace options (N.head key) tailNode) nodeValue

  | otherwise =
      do
        let conflictingNodeRef = options!!fromIntegral (N.head key)
        newNode <- putKV_NodeRef (N.tail key) val conflictingNodeRef
        return $ FullNodeData (replace options (N.head key) newNode) nodeValue

putKV_NodeData key1 val1 (ShortcutNodeData key2 val2)
  | key1 == key2 =
    case val2 of
      Right _  -> return $ ShortcutNodeData key1 $ Right val1
      Left ref -> do
        newNodeRef <- putKV_NodeRef key1 val1 ref
        return $ ShortcutNodeData key2 (Left newNodeRef)

  | N.null key1 = do
      newNodeRef <- newShortcut (N.tail key2) val2
      return $ FullNodeData (list2Options 0 [(N.head key2, newNodeRef)]) $ Just val1

  | key1 `N.isPrefixOf` key2 = do
      tailNode <- newShortcut (N.drop (N.length key1) key2) val2
      modifiedTailNode <- putKV_NodeRef "" val1 tailNode
      return $ ShortcutNodeData key1 $ Left modifiedTailNode

  | key2 `N.isPrefixOf` key1 =
    case val2 of
      Right val -> putKV_NodeData key2 val (ShortcutNodeData key1 $ Right val1)
      Left ref  -> do
        newNode <- putKV_NodeRef (N.drop (N.length key2) key1) val1 ref
        return $ ShortcutNodeData key2 $ Left newNode

  | N.head key1 == N.head key2 =
    let (commonPrefix, suffix1, suffix2) =
          getCommonPrefix (N.unpack key1) (N.unpack key2)
    in do
      nodeAfterCommonBeforePut <- newShortcut (N.pack suffix2) val2
      nodeAfterCommon <- putKV_NodeRef (N.pack suffix1) val1 nodeAfterCommonBeforePut
      return $ ShortcutNodeData (N.pack commonPrefix) $ Left nodeAfterCommon

  | otherwise = do
      tailNode1 <- newShortcut (N.tail key1) $ Right val1
      tailNode2 <- newShortcut (N.tail key2) val2
      return $ FullNodeData
        (list2Options 0 $ sortBy (compare `on` fst)
         [(N.head key1, tailNode1), (N.head key2, tailNode2)])
        Nothing

-----

getKeyVals_NodeData::(MonadIO m,KVPersister p)=>NodeData->MPKey->MixMemMode p m [(MPKey, MPVal)]

getKeyVals_NodeData EmptyNodeData _ = return []

getKeyVals_NodeData (FullNodeData {choices=cs}) "" = do
  partialKVs <- sequence $ (\ref -> getKeyVals_NodeRef ref "") <$> cs
  return $ concatMap
    (uncurry $ map . (prependToKey . N.singleton)) (zip [0..] partialKVs)

getKeyVals_NodeData (FullNodeData {choices=cs}) key
  | ref == emptyRef = return []
  | otherwise = fmap (prependToKey $ N.singleton $ N.head key) <$>
                getKeyVals_NodeRef ref (N.tail key)
  where ref = cs !! fromIntegral (N.head key)

getKeyVals_NodeData ShortcutNodeData{nextNibbleString=s, nextVal=Left ref} key
  | key `N.isPrefixOf` s = prependNext ""
  | s `N.isPrefixOf` key = prependNext $ N.drop (N.length s) key
  | otherwise = return []
  where prependNext key' = fmap (prependToKey s) <$> getKeyVals_NodeRef ref key'

getKeyVals_NodeData ShortcutNodeData{nextNibbleString=s, nextVal=Right val} key =
  return $
    if key `N.isPrefixOf` s
    then [(s,val)]
    else []

-----

deleteKey_NodeData::(MonadIO m,KVPersister p)=>MPKey->NodeData->MixMemMode p m NodeData

deleteKey_NodeData _ EmptyNodeData = return EmptyNodeData

deleteKey_NodeData key nd@(FullNodeData options val)
  | N.null key = return $ FullNodeData options Nothing

  | options `slotIsEmpty` N.head key = return nd

  | otherwise = do
    let nodeRef = options!!fromIntegral (N.head key)
    newNodeRef <- deleteKey_NodeRef (N.tail key) nodeRef
    let newOptions = replace options (N.head key) newNodeRef
    simplify_NodeData $ FullNodeData newOptions val

deleteKey_NodeData key1 nd@(ShortcutNodeData key2 (Right _)) =
  return $
    if key1 == key2
    then EmptyNodeData
    else nd

deleteKey_NodeData key1 nd@(ShortcutNodeData key2 (Left ref))
  | key2 `N.isPrefixOf` key1 = do
    newNodeRef <- deleteKey_NodeRef (N.drop (N.length key2) key1) ref
    simplify_NodeData $ ShortcutNodeData key2 $ Left newNodeRef

  | otherwise = return nd

-----

putKV_NodeRef::(MonadIO m,KVPersister p)=>MPKey->MPVal->NodeRef->MixMemMode p m NodeRef
putKV_NodeRef key val nodeRef = do
  nodeData <- getNodeData nodeRef
  newNodeData <- putKV_NodeData key val nodeData
  nodeData2NodeRef newNodeData


getKeyVals_NodeRef::(MonadIO m,KVPersister p)=>NodeRef->MPKey->MixMemMode p m [(MPKey, MPVal)]
getKeyVals_NodeRef ref key = do
  nodeData <- getNodeData ref
  getKeyVals_NodeData nodeData key

--TODO- This is looking like a lift, I probably should make NodeRef some sort of Monad....

deleteKey_NodeRef::(MonadIO m,KVPersister p)=>MPKey->NodeRef->MixMemMode p m NodeRef
deleteKey_NodeRef key nodeRef =
  nodeData2NodeRef =<< deleteKey_NodeData key =<< getNodeData nodeRef

-----

getNodeData::(MonadIO m, KVPersister p)=>NodeRef->MixMemMode p m NodeData
getNodeData (SmallRef x) = return $ justRight $ Bi.decodeFull' x
getNodeData ptr@(PtrRef p) = do
    modifier <- lift $ get
    bytes <- case (Map.lookup p modifier) of
      Just bs -> return bs 
      Nothing -> do
        db <- ask
        fromMaybe
          (error $ "Missing StateRoot in call to getNodeData: " <> formatNodeRef ptr) <$>
          getKV' db p
    return $ bytes2NodeData bytes
  where
    bytes2NodeData::B.ByteString->NodeData
    bytes2NodeData bytes | B.null bytes = EmptyNodeData
    bytes2NodeData bytes = justRight $ Bi.decodeFull' bytes

putNodeData::MonadIO m=>NodeData->MixMemMode p m B.ByteString
putNodeData nd = do
  let bytes = Bi.serialize' nd
      ptr = convert $ (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)

  lift $ modify' (Map.insert ptr bytes) 
  return ptr

-----

-- Only used to canonicalize the DB after a
-- delete.  We need to concatinate ShortcutNodeData links, convert
-- FullNodeData to ShortcutNodeData when possible, etc.

-- Important note- this function should only apply to immediate items,
-- and not recurse deep into the database (ie- by simplifying all options
-- in a FullNodeData, etc).  Failure to adhere will result in a
-- performance nightmare!  Any delete could result in a full read through
-- the whole database.  The delete function only will "break" the
-- canonical structure locally, so deep recursion isn't required.

simplify_NodeData::(MonadIO m,KVPersister p)=>NodeData->MixMemMode p m NodeData
simplify_NodeData EmptyNodeData = return EmptyNodeData
simplify_NodeData nd@(ShortcutNodeData key (Left ref)) = do
  refNodeData <- getNodeData ref
  case refNodeData of
    (ShortcutNodeData key2 v2) -> return $ ShortcutNodeData (key `N.append` key2) v2
    _                          -> return nd
simplify_NodeData (FullNodeData options Nothing) = do
    case options2List options of
      [(n, nodeRef)] ->
          simplify_NodeData $ ShortcutNodeData (N.singleton n) $ Left nodeRef
      _ -> return $ FullNodeData options Nothing
simplify_NodeData x = return x

-----

newShortcut::MonadIO m=>MPKey->Either NodeRef MPVal-> MixMemMode p m NodeRef
newShortcut "" (Left ref) = return ref
newShortcut key val       = nodeData2NodeRef $ ShortcutNodeData key val

nodeData2NodeRef::MonadIO m=>NodeData->MixMemMode p m NodeRef
nodeData2NodeRef nodeData =
  case Bi.serialize' nodeData of
    bytes | B.length bytes < 32 -> return $ SmallRef bytes
    _                           -> PtrRef <$> putNodeData nodeData
