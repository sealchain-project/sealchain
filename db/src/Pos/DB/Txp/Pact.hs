{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores pact datas.

module Pos.DB.Txp.Pact
       ( PactOp (..)
       , GStateDb (..)
       , newGStateDb
       ) where


import           Universum

import qualified Database.RocksDB as Rocks
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Base16 (base16F)

import           Pos.DB (RocksBatchOp (..))
import           Pos.DB.Rocks.Functions (rocksGetBytes, rocksPutBytes)
import           Pos.DB.Rocks.Types (DB, MonadRealDB, getGStateDB)

import           Sealchain.Mpt.MerklePatriciaMixMem (KVPersister (..))

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data PactOp = PactOp ByteString ByteString

instance Buildable PactOp where
    build (PactOp key val)           =
        bprint ("PactOp ("%base16F%", "%base16F%")") key val

instance RocksBatchOp PactOp where
    toBatchOp (PactOp key val) = [Rocks.Put key val]

----------------------------------------------------------------------------
-- GStateDb
----------------------------------------------------------------------------

newtype GStateDb = GStateDb DB

instance KVPersister GStateDb where
    getKV (GStateDb db) k = rocksGetBytes k db   
    putKV (GStateDb db) k v = rocksPutBytes k v db

newGStateDb :: MonadRealDB ctx m => m GStateDb
newGStateDb = GStateDb <$> getGStateDB