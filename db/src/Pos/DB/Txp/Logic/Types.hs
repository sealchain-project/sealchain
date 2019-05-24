{-# LANGUAGE InstanceSigs  #-}

module Pos.DB.Txp.Logic.Types
       ( GStateDB (..)
       , newGStateDB
       ) where

import           Universum

import           Sealchain.Mpt.MerklePatricia.MPDB (KVPersister (..))

import           Pos.DB.Rocks.Functions (rocksGetBytes, rocksPutBytes)
import           Pos.DB.Rocks.Types (DB, MonadRealDB, getGStateDB)


newtype GStateDB = GStateDB DB

instance KVPersister GStateDB where
    getKV (GStateDB db) k = rocksGetBytes k db   
    putKV (GStateDB db) k v = rocksPutBytes k v db

newGStateDB :: MonadRealDB ctx m => m GStateDB
newGStateDB = GStateDB <$> getGStateDB
