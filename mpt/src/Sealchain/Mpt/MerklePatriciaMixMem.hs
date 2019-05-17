{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatriciaMixMem (
  MPKey, 
  MPVal, 
  MPPtr,
  MPDB(..), 
  StateRoot(..),
  MMModifier,
  putKeyValMixMem, 
  getKeyValMixMem, 
  getAllKeyValsMixMem,
  deleteKeyMixMem, 
  keyExistsMixMem,
  initializeBlankMixMem,
  justRight,
  emptyTriePtr,
  unboxStateRoot
  ) where

import           Universum

import           Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Database.RocksDB as DB

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi

import           Sealchain.Mpt.Utils
import           Sealchain.Mpt.MerklePatricia.MPDB
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.InternalMixMem
import           Sealchain.Mpt.MerklePatricia.StateRoot
import           Sealchain.Mpt.MerklePatricia.Utils

data RichValue = RichValue 
  { _rvKey :: B.ByteString
  , _rvVal :: B.ByteString
  }

instance Bi RichValue where
    encode rv = Bi.encodeListLen 2
                <> Bi.encode (_rvKey rv)
                <> Bi.encode (_rvVal rv)
    decode = do
        Bi.enforceSize "RichValue" 2
        RichValue <$> Bi.decode <*> Bi.decode

richValueToPair :: RichValue -> (B.ByteString, B.ByteString)
richValueToPair RichValue{..} = (_rvKey, _rvVal)

putKeyValMixMem::MonadIO m=>MPDB
           ->MMModifier
           ->B.ByteString
           ->B.ByteString
           ->m (MPPtr, MMModifier)
putKeyValMixMem db mmm key val = do
  runMixMemMode db mmm $ unsafePutKeyValMixMem (byteStringToSafeKey key) (Bi.serialize' $ RichValue key val)

getKeyValMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->B.ByteString
         ->m (Maybe B.ByteString)
getKeyValMixMem db mmm key = do
  (keyVals, _) <- runMixMemMode db mmm $ unsafeGetKeyValsMixMem (byteStringToSafeKey key)
  case keyVals of
    []     -> return Nothing
    (x:_)  -> do
      let RichValue _ val = justRight $ Bi.decodeFull' $ snd x
      return $ Just val

getAllKeyValsMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->m [(B.ByteString, B.ByteString)]
getAllKeyValsMixMem db mmm = do
  (keyVals, _) <- runMixMemMode db mmm $ unsafeGetAllKeyValsMixMem
  let actualKeyVals = map (richValueToPair . justRight . Bi.decodeFull' . snd) keyVals 
  return actualKeyVals

deleteKeyMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->B.ByteString
         ->m (MPPtr, MMModifier)
deleteKeyMixMem db mmm key = do
  runMixMemMode db mmm $ unsafeDeleteKeyMixMem (byteStringToSafeKey key)

keyExistsMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->B.ByteString
         ->m Bool
keyExistsMixMem db mmm key = isJust <$> getKeyValMixMem db mmm key

initializeBlankMixMem::MonadIO m=>MPDB -- ^ The object containing the current stateRoot.
               ->m MMModifier
initializeBlankMixMem db = do
    let bytes = Bi.serialize' (0::Integer)
        StateRoot key = emptyTriePtr

    DB.put (rdb db) DB.defaultWriteOptions key bytes
    return Map.empty


