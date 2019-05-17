-- | This is an implementation of the modified Merkle Patricia database
-- described in the Ethereum Yellowpaper
-- (<http://gavwood.com/paper.pdf>).  This modified version works like a
-- canonical Merkle Patricia database, but includes certain
-- optimizations.  In particular, a new type of "shortcut node" has been
-- added to represent multiple traditional nodes that fall in a linear
-- string (ie- a stretch of parent child nodes where no branch choices
-- exist).
--
-- A Merkle Patricia Database effeciently retains its full history, and a
-- snapshot of all key-value pairs at a given time can be looked up using
-- a "stateRoot" (a pointer to the root of the tree representing that
-- data).  Many of the functions in this module work by updating this
-- object, so for anything more complicated than a single update, use of
-- the state monad is recommended.
--
-- The underlying data is actually stored in RocksDB.  This module
-- provides the logic to organize the key-value pairs in the appropriate
-- Patricia Merkle Tree.

module Sealchain.Mpt.MerklePatricia (
  MPKey, MPVal, MPDB(..), StateRoot(..),
  openMPDB, emptyTriePtr, sha2StateRoot, unboxStateRoot,
  putKeyVal, getKeyVal, deleteKey, keyExists,
  initializeBlank
  ) where

import           Universum
import qualified Universum.Unsafe as Unsafe

import           Control.Monad.Trans(MonadIO)
import           Data.Maybe(isJust)
import qualified Database.RocksDB as DB

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.MPDB
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.Internal
import           Sealchain.Mpt.MerklePatricia.StateRoot
import           Sealchain.Mpt.MerklePatricia.Utils


-- | Adds a new key/value pair.
putKeyVal::MonadIO m=>MPDB -- ^ The object containing the current stateRoot.
           ->MPKey -- ^ Key of the data to be inserted.
           ->MPVal -- ^ Value of the new data
           ->m MPDB -- ^ The object containing the stateRoot to the data after the insert.
putKeyVal db = unsafePutKeyVal db . keyToSafeKey

-- | Retrieves all key/value pairs whose key starts with the given parameter.
getKeyVal::MonadIO m=>MPDB -- ^ Object containing the current stateRoot.
         -> MPKey -- ^ Key of the data to be inserted.
         -> m (Maybe MPVal) -- ^ The requested value.
getKeyVal db key = do
  vals <- unsafeGetKeyVals db (keyToSafeKey key)
  return $
    if not (null vals)
    then Just $ snd (Unsafe.head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

-- | Deletes a key (and its corresponding data) from the database.
--
-- Note that the key/value pair will still be present in the history, and
-- can be accessed by using an older 'MPDB' object.
deleteKey::MonadIO m=>MPDB -- ^ The object containing the current stateRoot.
         ->MPKey -- ^ The key to be deleted.
         ->m MPDB -- ^ The object containing the stateRoot to the data after the delete.
deleteKey db = unsafeDeleteKey db . keyToSafeKey

-- | Returns True is a key exists.
keyExists::MonadIO m=>MPDB -- ^ The object containing the current stateRoot.
         ->MPKey -- ^ The key to be deleted.
         ->m Bool -- ^ True if the key exists
keyExists db key = isJust <$> getKeyVal db key

-- | Initialize the DB by adding a blank stateroot.
initializeBlank::MonadIO m=>MPDB -- ^ The object containing the current stateRoot.
               ->m ()
initializeBlank db =
    let bytes = rlpSerialize $ rlpEncode (0::Integer)
        StateRoot key = emptyTriePtr
    in DB.put (rdb db) DB.defaultWriteOptions key bytes

