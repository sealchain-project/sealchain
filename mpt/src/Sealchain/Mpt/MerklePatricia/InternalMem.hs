{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.InternalMem (
  MPMem(..),
  unsafePutKeyValMem,
  unsafeGetKeyValsMem,
  unsafeGetAllKeyValsMem,
  unsafeDeleteKeyMem,
  getNodeDataMem,
  putNodeDataMem,
  ) where

import qualified Data.ByteString as B
import           Data.ByteArray (convert)
import           Crypto.Hash as Crypto
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.NibbleString as N

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.Utils

type MPMap = Map.Map B.ByteString B.ByteString

data MPMem = MPMem {
    mpMap       :: MPMap,
    mpRoot      :: B.ByteString
  } deriving Show


unsafePutKeyValMem::Monad m=>MPMem->Key->Val->m MPMem
unsafePutKeyValMem db key val = do
  dbNodeData <- getNodeDataMem db (PtrRef $ mpRoot db)
  dbPutNodeData <- putKV_NodeDataMem db key val dbNodeData
  putNodeDataMem (fst dbPutNodeData) (snd dbPutNodeData)

unsafeGetKeyValsMem::Monad m=>MPMem->Key->m [(Key,Val)]
unsafeGetKeyValsMem db =
  let dbNodeRef = PtrRef $ mpRoot db
  in getKeyVals_NodeRefMem db dbNodeRef

unsafeGetAllKeyValsMem::Monad m=>MPMem->m [(Key,Val)]
unsafeGetAllKeyValsMem db = unsafeGetKeyValsMem db N.empty

unsafeDeleteKeyMem::Monad m=>MPMem->Key->m MPMem
unsafeDeleteKeyMem db key = do
  dbNodeData <- getNodeDataMem db (PtrRef $ mpRoot db)
  dbDeleteNodeData <- deleteKey_NodeDataMem db key dbNodeData
  putNodeDataMem (fst dbDeleteNodeData) (snd dbDeleteNodeData)

-----

putKV_NodeDataMem::Monad m=>MPMem->Key->Val->NodeData-> m (MPMem,NodeData)

putKV_NodeDataMem db key val EmptyNodeData =
  return $ (db,ShortcutNodeData key (Right val))

putKV_NodeDataMem db key val (FullNodeData options nodeValue)
  | options `slotIsEmpty` N.head key =
    do
      tailNode <- newShortcutMem db (N.tail key) $ Right val
      return $ (fst tailNode, FullNodeData (replace options (N.head key) (snd tailNode)) nodeValue)

  | otherwise =
      do
        let conflictingNodeRef = options!!fromIntegral (N.head key)
        newNode <- putKV_NodeRefMem db (N.tail key) val conflictingNodeRef
        return $ (fst newNode, FullNodeData (replace options (N.head key) (snd newNode)) nodeValue)

putKV_NodeDataMem db key1 val1 (ShortcutNodeData key2 val2)
  | key1 == key2 =
    case val2 of
      Right _  -> return $ (db, ShortcutNodeData key1 $ Right val1)
      Left ref -> do
        newNodeRef <- putKV_NodeRefMem db key1 val1 ref
        return $ (fst newNodeRef, ShortcutNodeData key2 (Left . snd $ newNodeRef))

  | N.null key1 = do
      newNodeRef <- newShortcutMem db (N.tail key2) val2
      return $ (fst newNodeRef, FullNodeData (list2Options 0 [(N.head key2, snd newNodeRef)]) $ Just val1)

  | key1 `N.isPrefixOf` key2 = do
      tailNode <- newShortcutMem db (N.drop (N.length key1) key2) val2
      modifiedTailNode <- putKV_NodeRefMem (fst tailNode) "" val1 (snd tailNode)
      return $ (fst modifiedTailNode, ShortcutNodeData key1 $ Left (snd modifiedTailNode))

  | key2 `N.isPrefixOf` key1 =
    case val2 of
      Right val -> putKV_NodeDataMem db key2 val (ShortcutNodeData key1 $ Right val1)
      Left ref  -> do
        newNode <- putKV_NodeRefMem db (N.drop (N.length key2) key1) val1 ref
        return $ (fst newNode, ShortcutNodeData key2 $ Left (snd newNode))

  | N.head key1 == N.head key2 =
    let (commonPrefix, suffix1, suffix2) =
          getCommonPrefix (N.unpack key1) (N.unpack key2)
    in do
      nodeAfterCommonBeforePut <- newShortcutMem db (N.pack suffix2) val2
      nodeAfterCommon <- putKV_NodeRefMem (fst nodeAfterCommonBeforePut)
                                          (N.pack suffix1)
                                          val1
                                          (snd nodeAfterCommonBeforePut)

      return $ (fst nodeAfterCommon,
                ShortcutNodeData (N.pack commonPrefix) $ Left (snd nodeAfterCommon))

  | otherwise = do
      tailNode1 <- newShortcutMem db (N.tail key1) $ Right val1
      tailNode2 <- newShortcutMem (fst tailNode1) (N.tail key2) val2
      return $ (fst tailNode2, FullNodeData
          (list2Options 0 $ sortBy (compare `on` fst) [(N.head key1, snd tailNode1),
                                                       (N.head key2, snd tailNode2)])
           Nothing)

-----

getKeyVals_NodeDataMem::Monad m=>MPMem->NodeData->Key->m [(Key, Val)]

getKeyVals_NodeDataMem _ EmptyNodeData _ = return []

getKeyVals_NodeDataMem db (FullNodeData {choices=cs}) "" = do
  partialKVs <- sequence $ (\ref -> getKeyVals_NodeRefMem db ref "") <$> cs
  return $ concatMap
    (uncurry $ map . (prependToKey . N.singleton)) (zip [0..] partialKVs)

getKeyVals_NodeDataMem db (FullNodeData {choices=cs}) key
  | ref == emptyRef = return []
  | otherwise = fmap (prependToKey $ N.singleton $ N.head key) <$>
                getKeyVals_NodeRefMem db ref (N.tail key)
  where ref = cs !! fromIntegral (N.head key)

getKeyVals_NodeDataMem db ShortcutNodeData{nextNibbleString=s, nextVal=Left ref} key
  | key `N.isPrefixOf` s = prependNext ""
  | s `N.isPrefixOf` key = prependNext $ N.drop (N.length s) key
  | otherwise = return []
  where prependNext key' = fmap (prependToKey s) <$> getKeyVals_NodeRefMem db ref key'

getKeyVals_NodeDataMem _ ShortcutNodeData{nextNibbleString=s, nextVal=Right val} key =
  return $
    if key `N.isPrefixOf` s
    then [(s,val)]
    else []

-----

deleteKey_NodeDataMem::Monad m=>MPMem->Key->NodeData-> m (MPMem,NodeData)

deleteKey_NodeDataMem db _ EmptyNodeData = return (db,EmptyNodeData)

deleteKey_NodeDataMem db key nd@(FullNodeData options val)
  | N.null key = return $ (db,FullNodeData options Nothing)

  | options `slotIsEmpty` N.head key = return (db,nd)

  | otherwise = do
    let nodeRef = options!!fromIntegral (N.head key)
    newNodeRef <- deleteKey_NodeRefMem db (N.tail key) nodeRef
    let newOptions = replace options (N.head key) (snd newNodeRef)
    simplify_NodeDataMem db $ FullNodeData newOptions val

deleteKey_NodeDataMem db key1 nd@(ShortcutNodeData key2 (Right _)) =
  return $
    if key1 == key2
    then (db,EmptyNodeData)
    else (db,nd)

deleteKey_NodeDataMem db key1 nd@(ShortcutNodeData key2 (Left ref))
  | key2 `N.isPrefixOf` key1 = do
    newNodeRef <- deleteKey_NodeRefMem db (N.drop (N.length key2) key1) ref
    simplify_NodeDataMem (fst newNodeRef) $ ShortcutNodeData key2 $ Left (snd newNodeRef)

  | otherwise = return (db, nd)

-----

putKV_NodeRefMem::Monad m=>MPMem->Key->Val->NodeRef->m (MPMem,NodeRef)
putKV_NodeRefMem db key val nodeRef = do
  nodeData <- getNodeDataMem db nodeRef
  db' <- putKV_NodeDataMem db key val nodeData
  nodeData2NodeRefMem (fst db') (snd db')


getKeyVals_NodeRefMem::Monad m=>MPMem->NodeRef->Key->m [(Key, Val)]
getKeyVals_NodeRefMem db ref key = do
  nodeData <- getNodeDataMem db ref
  getKeyVals_NodeDataMem db nodeData key

--TODO- This is looking like a lift, I probably should make NodeRef some sort of Monad....

deleteKey_NodeRefMem::Monad m=>MPMem->Key->NodeRef->m (MPMem,NodeRef)
deleteKey_NodeRefMem db key nodeRef = do
  ref <- getNodeDataMem db nodeRef
  db'<- deleteKey_NodeDataMem db key ref

  nodeData2NodeRefMem (fst db') ref

-----

getNodeDataMem::Monad m=>MPMem->NodeRef->m NodeData
getNodeDataMem _ (SmallRef x) = return $ rlpDecode $ rlpDeserialize x
getNodeDataMem db ptr@(PtrRef bs) = do
  let bytes = fromMaybe (error $ "Missing StateRoot in call to getNodeData: " ++ formatNodeRef ptr)
                        (Map.lookup bs (mpMap db))

  return $ bytes2NodeData bytes
    where
      bytes2NodeData::B.ByteString->NodeData
      bytes2NodeData bytes | B.null bytes = EmptyNodeData
      bytes2NodeData bytes = rlpDecode $ rlpDeserialize bytes

putNodeDataMem::Monad m=>MPMem->NodeData->m MPMem
putNodeDataMem db nd = do
  let bytes = rlpSerialize $ rlpEncode nd
      ptr = convert (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)
      map' = Map.insert ptr bytes (mpMap db)
  return $ MPMem { mpMap = map', mpRoot = ptr }


simplify_NodeDataMem::Monad m=>MPMem->NodeData->m (MPMem,NodeData)
simplify_NodeDataMem db EmptyNodeData = return (db,EmptyNodeData)
simplify_NodeDataMem db nd@(ShortcutNodeData key (Left ref)) = do
  refNodeData <- getNodeDataMem db ref
  case refNodeData of
    (ShortcutNodeData key2 v2) -> return $ (db,ShortcutNodeData (key `N.append` key2) v2)
    _                          -> return (db,nd)
simplify_NodeDataMem db (FullNodeData options Nothing) = do
    case options2List options of
      [(n, nodeRef)] ->
          simplify_NodeDataMem db $ ShortcutNodeData (N.singleton n) $ Left nodeRef
      _ -> return $ (db,FullNodeData options Nothing)
simplify_NodeDataMem db x = return (db,x)

newShortcutMem::Monad m=>MPMem->Key->Either NodeRef Val->m (MPMem,NodeRef)
newShortcutMem db "" (Left ref) = return (db,ref)
newShortcutMem db key val       = nodeData2NodeRefMem db $ ShortcutNodeData key val

nodeData2NodeRefMem::Monad m=>MPMem->NodeData->m (MPMem,NodeRef)
nodeData2NodeRefMem db nodeData =
  case rlpSerialize $ rlpEncode nodeData of
    bytes | B.length bytes < 32 -> return $ (db,SmallRef bytes)
    _ -> do
      new <- putNodeDataMem db nodeData
      return (new, PtrRef . mpRoot $ new)
