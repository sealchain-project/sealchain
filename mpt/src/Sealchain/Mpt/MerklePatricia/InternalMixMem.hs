{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.InternalMixMem (
  MPMixMem(..),
  unsafePutKeyValMixMem,
  unsafeGetKeyValsMixMem,
  unsafeGetAllKeyValsMixMem,
  unsafeDeleteKeyMixMem,
  getNodeDataMixMem,
  putNodeDataMixMem,
  ) where

import           Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as B
import           Data.ByteArray (convert)
import           Crypto.Hash as Crypto
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import qualified Data.NibbleString as N
import qualified Database.RocksDB as DB

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.MPDB
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.StateRoot
import           Sealchain.Mpt.MerklePatricia.Utils

type MPMixMap = Map.Map B.ByteString B.ByteString

data MPMixMem = MPMixMem {
    mpmStateRoot :: StateRoot,
    mpmDB        :: MPDB,
    mpmMap       :: MPMixMap
  } 

unsafePutKeyValMixMem::MonadIO m=>MPMixMem->Key->Val->m MPMixMem
unsafePutKeyValMixMem mpmm key val = do
  dbNodeData <- getNodeDataMixMem mpmm (PtrRef $ mpmStateRoot mpmm)
  dbPutNodeData <- putKV_NodeDataMixMem mpmm key val dbNodeData
  putNodeDataMixMem (fst dbPutNodeData) (snd dbPutNodeData)

unsafeGetKeyValsMixMem::MonadIO m=>MPMixMem->Key->m [(Key,Val)]
unsafeGetKeyValsMixMem mpmm =
  let dbNodeRef = PtrRef $ mpmStateRoot mpmm
  in getKeyVals_NodeRefMixMem mpmm dbNodeRef

unsafeGetAllKeyValsMixMem::MonadIO m=>MPMixMem->m [(Key,Val)]
unsafeGetAllKeyValsMixMem mpmm = unsafeGetKeyValsMixMem mpmm N.empty

unsafeDeleteKeyMixMem::MonadIO m=>MPMixMem->Key->m MPMixMem
unsafeDeleteKeyMixMem mpmm key = do
  dbNodeData <- getNodeDataMixMem mpmm (PtrRef $ mpmStateRoot mpmm)
  dbDeleteNodeData <- deleteKey_NodeDataMixMem mpmm key dbNodeData
  putNodeDataMixMem (fst dbDeleteNodeData) (snd dbDeleteNodeData)

-----

putKV_NodeDataMixMem::MonadIO m=>MPMixMem->Key->Val->NodeData-> m (MPMixMem,NodeData)

putKV_NodeDataMixMem mpmm key val EmptyNodeData =
  return $ (mpmm,ShortcutNodeData key (Right val))

putKV_NodeDataMixMem mpmm key val (FullNodeData options nodeValue)
  | options `slotIsEmpty` N.head key =
    do
      tailNode <- newShortcutMem mpmm (N.tail key) $ Right val
      return $ (fst tailNode, FullNodeData (replace options (N.head key) (snd tailNode)) nodeValue)

  | otherwise =
      do
        let conflictingNodeRef = options!!fromIntegral (N.head key)
        newNode <- putKV_NodeRefMem mpmm (N.tail key) val conflictingNodeRef
        return $ (fst newNode, FullNodeData (replace options (N.head key) (snd newNode)) nodeValue)

putKV_NodeDataMixMem mpmm key1 val1 (ShortcutNodeData key2 val2)
  | key1 == key2 =
    case val2 of
      Right _  -> return $ (mpmm, ShortcutNodeData key1 $ Right val1)
      Left ref -> do
        newNodeRef <- putKV_NodeRefMem mpmm key1 val1 ref
        return $ (fst newNodeRef, ShortcutNodeData key2 (Left . snd $ newNodeRef))

  | N.null key1 = do
      newNodeRef <- newShortcutMem mpmm (N.tail key2) val2
      return $ (fst newNodeRef, FullNodeData (list2Options 0 [(N.head key2, snd newNodeRef)]) $ Just val1)

  | key1 `N.isPrefixOf` key2 = do
      tailNode <- newShortcutMem mpmm (N.drop (N.length key1) key2) val2
      modifiedTailNode <- putKV_NodeRefMem (fst tailNode) "" val1 (snd tailNode)
      return $ (fst modifiedTailNode, ShortcutNodeData key1 $ Left (snd modifiedTailNode))

  | key2 `N.isPrefixOf` key1 =
    case val2 of
      Right val -> putKV_NodeDataMixMem mpmm key2 val (ShortcutNodeData key1 $ Right val1)
      Left ref  -> do
        newNode <- putKV_NodeRefMem mpmm (N.drop (N.length key2) key1) val1 ref
        return $ (fst newNode, ShortcutNodeData key2 $ Left (snd newNode))

  | N.head key1 == N.head key2 =
    let (commonPrefix, suffix1, suffix2) =
          getCommonPrefix (N.unpack key1) (N.unpack key2)
    in do
      nodeAfterCommonBeforePut <- newShortcutMem mpmm (N.pack suffix2) val2
      nodeAfterCommon <- putKV_NodeRefMem (fst nodeAfterCommonBeforePut)
                                          (N.pack suffix1)
                                          val1
                                          (snd nodeAfterCommonBeforePut)

      return $ (fst nodeAfterCommon,
                ShortcutNodeData (N.pack commonPrefix) $ Left (snd nodeAfterCommon))

  | otherwise = do
      tailNode1 <- newShortcutMem mpmm (N.tail key1) $ Right val1
      tailNode2 <- newShortcutMem (fst tailNode1) (N.tail key2) val2
      return $ (fst tailNode2, FullNodeData
          (list2Options 0 $ sortBy (compare `on` fst) [(N.head key1, snd tailNode1),
                                                       (N.head key2, snd tailNode2)])
           Nothing)

-----

getKeyVals_NodeDataMixMem::MonadIO m=>MPMixMem->NodeData->Key->m [(Key, Val)]

getKeyVals_NodeDataMixMem _ EmptyNodeData _ = return []

getKeyVals_NodeDataMixMem mpmm (FullNodeData {choices=cs}) "" = do
  partialKVs <- sequence $ (\ref -> getKeyVals_NodeRefMixMem mpmm ref "") <$> cs
  return $ concatMap
    (uncurry $ map . (prependToKey . N.singleton)) (zip [0..] partialKVs)

getKeyVals_NodeDataMixMem mpmm (FullNodeData {choices=cs}) key
  | ref == emptyRef = return []
  | otherwise = fmap (prependToKey $ N.singleton $ N.head key) <$>
                getKeyVals_NodeRefMixMem mpmm ref (N.tail key)
  where ref = cs !! fromIntegral (N.head key)

getKeyVals_NodeDataMixMem mpmm ShortcutNodeData{nextNibbleString=s, nextVal=Left ref} key
  | key `N.isPrefixOf` s = prependNext ""
  | s `N.isPrefixOf` key = prependNext $ N.drop (N.length s) key
  | otherwise = return []
  where prependNext key' = fmap (prependToKey s) <$> getKeyVals_NodeRefMixMem mpmm ref key'

getKeyVals_NodeDataMixMem _ ShortcutNodeData{nextNibbleString=s, nextVal=Right val} key =
  return $
    if key `N.isPrefixOf` s
    then [(s,val)]
    else []

-----

deleteKey_NodeDataMixMem::MonadIO m=>MPMixMem->Key->NodeData-> m (MPMixMem,NodeData)

deleteKey_NodeDataMixMem mpmm _ EmptyNodeData = return (mpmm,EmptyNodeData)

deleteKey_NodeDataMixMem mpmm key nd@(FullNodeData options val)
  | N.null key = return $ (mpmm,FullNodeData options Nothing)

  | options `slotIsEmpty` N.head key = return (mpmm,nd)

  | otherwise = do
    let nodeRef = options!!fromIntegral (N.head key)
    newNodeRef <- deleteKey_NodeRefMixMem mpmm (N.tail key) nodeRef
    let newOptions = replace options (N.head key) (snd newNodeRef)
    simplify_NodeDataMixMem mpmm $ FullNodeData newOptions val

deleteKey_NodeDataMixMem mpmm key1 nd@(ShortcutNodeData key2 (Right _)) =
  return $
    if key1 == key2
    then (mpmm,EmptyNodeData)
    else (mpmm,nd)

deleteKey_NodeDataMixMem mpmm key1 nd@(ShortcutNodeData key2 (Left ref))
  | key2 `N.isPrefixOf` key1 = do
    newNodeRef <- deleteKey_NodeRefMixMem mpmm (N.drop (N.length key2) key1) ref
    simplify_NodeDataMixMem (fst newNodeRef) $ ShortcutNodeData key2 $ Left (snd newNodeRef)

  | otherwise = return (mpmm, nd)

-----

putKV_NodeRefMem::MonadIO m=>MPMixMem->Key->Val->NodeRef->m (MPMixMem,NodeRef)
putKV_NodeRefMem mpmm key val nodeRef = do
  nodeData <- getNodeDataMixMem mpmm nodeRef
  mpmm' <- putKV_NodeDataMixMem mpmm key val nodeData
  nodeData2NodeRefMem (fst mpmm') (snd mpmm')


getKeyVals_NodeRefMixMem::MonadIO m=>MPMixMem->NodeRef->Key->m [(Key, Val)]
getKeyVals_NodeRefMixMem mpmm ref key = do
  nodeData <- getNodeDataMixMem mpmm ref
  getKeyVals_NodeDataMixMem mpmm nodeData key

--TODO- This is looking like a lift, I probably should make NodeRef some sort of Monad....

deleteKey_NodeRefMixMem::MonadIO m=>MPMixMem->Key->NodeRef->m (MPMixMem,NodeRef)
deleteKey_NodeRefMixMem mpmm key nodeRef = do
  ref <- getNodeDataMixMem mpmm nodeRef
  mpmm'<- deleteKey_NodeDataMixMem mpmm key ref

  nodeData2NodeRefMem (fst mpmm') ref

-----

getNodeDataMixMem::MonadIO m=>MPMixMem->NodeRef->m NodeData
getNodeDataMixMem _ (SmallRef x) = return $ rlpDecode $ rlpDeserialize x
getNodeDataMixMem mpmm (PtrRef ptr@(StateRoot p)) = do
    bytesM <- lookupVal p
    case bytesM of
      Nothing    -> error $ "Missing StateRoot in call to getNodeData: " ++ formatStateRoot ptr
      Just bytes ->
        return $ bytes2NodeData bytes
  where
    bytes2NodeData::B.ByteString->NodeData
    bytes2NodeData bytes | B.null bytes = EmptyNodeData
    bytes2NodeData bytes = rlpDecode $ rlpDeserialize bytes

    lookupVal k = case (Map.lookup k (mpmMap mpmm)) of
      Just bytes -> pure $ Just bytes
      Nothing    -> DB.get (rdb $ mpmDB mpmm) DB.defaultReadOptions k
         

putNodeDataMixMem::MonadIO m=>MPMixMem->NodeData->m MPMixMem
putNodeDataMixMem mpmm nd = do
  let bytes = rlpSerialize $ rlpEncode nd
      ptr = convert (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)
      map' = Map.insert ptr bytes (mpmMap mpmm)
  return $ mpmm { mpmMap = map', mpmStateRoot = StateRoot ptr }


simplify_NodeDataMixMem::MonadIO m=>MPMixMem->NodeData->m (MPMixMem,NodeData)
simplify_NodeDataMixMem mpmm EmptyNodeData = return (mpmm,EmptyNodeData)
simplify_NodeDataMixMem mpmm nd@(ShortcutNodeData key (Left ref)) = do
  refNodeData <- getNodeDataMixMem mpmm ref
  case refNodeData of
    (ShortcutNodeData key2 v2) -> return $ (mpmm,ShortcutNodeData (key `N.append` key2) v2)
    _                          -> return (mpmm,nd)
simplify_NodeDataMixMem mpmm (FullNodeData options Nothing) = do
    case options2List options of
      [(n, nodeRef)] ->
          simplify_NodeDataMixMem mpmm $ ShortcutNodeData (N.singleton n) $ Left nodeRef
      _ -> return $ (mpmm,FullNodeData options Nothing)
simplify_NodeDataMixMem mpmm x = return (mpmm,x)

newShortcutMem::MonadIO m=>MPMixMem->Key->Either NodeRef Val->m (MPMixMem,NodeRef)
newShortcutMem mpmm "" (Left ref) = return (mpmm,ref)
newShortcutMem mpmm key val       = nodeData2NodeRefMem mpmm $ ShortcutNodeData key val

nodeData2NodeRefMem::MonadIO m=>MPMixMem->NodeData->m (MPMixMem,NodeRef)
nodeData2NodeRefMem mpmm nodeData =
  case rlpSerialize $ rlpEncode nodeData of
    bytes | B.length bytes < 32 -> return $ (mpmm,SmallRef bytes)
    _ -> do
      new <- putNodeDataMixMem mpmm nodeData
      return (new, PtrRef . mpmStateRoot $ new)
