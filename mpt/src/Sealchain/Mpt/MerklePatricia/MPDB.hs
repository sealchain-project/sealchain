{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.MPDB (
  KVPersister (..),
  MPDB(..),
  getKV',
  putKV'
  ) where

import           Universum

import qualified Data.ByteString as B

import           Sealchain.Mpt.MerklePatricia.StateRoot

-- | The interface represents KV persister
class KVPersister k where
    getKV :: MonadIO m => k -> B.ByteString -> m (Maybe B.ByteString)
    putKV :: MonadIO m => k -> B.ByteString -> B.ByteString -> m () 

-- | This is the database reference type, contianing both the handle to the underlying database, as well
-- as the stateRoot to the current tree holding the data.
--
-- The MPDB acts a bit like a traditional database handle, although because it contains the stateRoot,
-- many functions act by updating its value.  Because of this, it is recommended that this item be
-- stored and modified within the state monad.
data MPDB p = MPDB {
    kvdb      :: p,
    stateRoot :: StateRoot
}

getKV' :: (KVPersister p, MonadIO m) => MPDB p -> B.ByteString -> m (Maybe B.ByteString)
getKV' mpdb k = getKV (kvdb mpdb) k

putKV' :: (KVPersister p, MonadIO m) => MPDB p -> B.ByteString -> B.ByteString -> m ()
putKV' mpdb k v = putKV (kvdb mpdb) k v