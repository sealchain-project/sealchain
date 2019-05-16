
module Sealchain.Mpt.MerklePatriciaMixMem (
  Key, 
  Val, 
  MPDB(..), 
  StateRoot(..),
  MMModifier,
  putKeyValMixMem, 
  getKeyValMixMem, 
  deleteKeyMixMem, 
  keyExistsMixMem,
  initializeBlankMixMem
  ) where

import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map as Map
import           Data.Maybe(isJust)
import qualified Database.RocksDB as DB

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.MPDB
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.InternalMixMem
import           Sealchain.Mpt.MerklePatricia.StateRoot
import           Sealchain.Mpt.MerklePatricia.Utils

putKeyValMixMem::MonadIO m=>MPDB
           ->MMModifier
           ->Key
           ->Val
           ->m MMModifier
putKeyValMixMem db mmm key val = do
  (_, mmm') <- runMixMemMode db mmm $ unsafePutKeyValMixMem (keyToSafeKey key) val
  return mmm'

getKeyValMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->Key
         ->m (Maybe Val)
getKeyValMixMem db mmm key = do
  (vals, _) <- runMixMemMode db mmm $ unsafeGetKeyValsMixMem (keyToSafeKey key)
  return $
    if not (null vals)
    then Just $ snd (head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

deleteKeyMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->Key
         ->m MMModifier
deleteKeyMixMem db mmm key = do
  (_, mmm') <- runMixMemMode db mmm $ unsafeDeleteKeyMixMem (keyToSafeKey key)
  return mmm'

keyExistsMixMem::MonadIO m=>MPDB
         ->MMModifier
         ->Key
         ->m Bool
keyExistsMixMem db mmm key = isJust <$> getKeyValMixMem db mmm key

initializeBlankMixMem::MonadResource m=>MPDB -- ^ The object containing the current stateRoot.
               ->m MMModifier
initializeBlankMixMem db = do
    let bytes = rlpSerialize $ rlpEncode (0::Integer)
        StateRoot key = emptyTriePtr

    DB.put (rdb db) DB.defaultWriteOptions key bytes
    return Map.empty


