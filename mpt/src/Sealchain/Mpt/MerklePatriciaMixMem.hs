
module Sealchain.Mpt.MerklePatriciaMixMem (
  putKeyValMixMem, getKeyValMixMem, deleteKeyMixMem, keyExistsMixMem,
  initializeBlankMixMem,
  MPMixMem(..)
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

putKeyValMixMem::MonadIO m=>MPMixMem
           ->Key
           ->Val
           ->m MPMixMem
putKeyValMixMem db = unsafePutKeyValMixMem db . keyToSafeKey


getKeyValMixMem::MonadIO m=>MPMixMem
         -> Key
         -> m (Maybe Val)
getKeyValMixMem db key = do
  vals <- unsafeGetKeyValsMixMem db (keyToSafeKey key)
  return $
    if not (null vals)
    then Just $ snd (head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

deleteKeyMixMem::MonadIO m=>MPMixMem
         ->Key
         ->m MPMixMem
deleteKeyMixMem db = unsafeDeleteKeyMixMem db . keyToSafeKey

keyExistsMixMem::MonadIO m=>MPMixMem
         ->Key
         ->m Bool
keyExistsMixMem db key = isJust <$> getKeyValMixMem db key

initializeBlankMixMem::MonadResource m=>MPDB -- ^ The object containing the current stateRoot.
               ->m MPMixMem
initializeBlankMixMem db = do
    let bytes = rlpSerialize $ rlpEncode (0::Integer)
        StateRoot key = emptyTriePtr

    DB.put (rdb db) DB.defaultWriteOptions key bytes
    return $ MPMixMem {
             mpmStateRoot = StateRoot bytes,
             mpmDB = db,
             mpmMap = Map.empty
           }


