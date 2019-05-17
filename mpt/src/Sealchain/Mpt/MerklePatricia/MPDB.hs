{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.MPDB (
  MPDB(..),
  openMPDB
  ) where

import           Universum

import           Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Database.RocksDB as Rocks

import           Sealchain.Mpt.MerklePatricia.StateRoot
import qualified Pos.Binary.Class as Bi

-- | This is the database reference type, contianing both the handle to the underlying database, as well
-- as the stateRoot to the current tree holding the data.
--
-- The MPDB acts a bit like a traditional database handle, although because it contains the stateRoot,
-- many functions act by updating its value.  Because of this, it is recommended that this item be
-- stored and modified within the state monad.
data MPDB = MPDB {
    rdb       :: Rocks.DB,
    stateRoot :: StateRoot
}

-- | This function is used to create an MPDB object corresponding to the blank database.
-- After creation, the stateRoot can be changed to a previously saved version.
openMPDB::String -- ^ The filepath with the location of the underlying database.
        ->ResourceT IO MPDB
openMPDB path = do
    rdb' <- Rocks.open options
    Rocks.put rdb' Rocks.defaultWriteOptions (Bi.serialize' emptyTriePtr) B.empty
    return MPDB{ rdb=rdb', stateRoot=emptyTriePtr }
  where options = (Rocks.defaultOptions path)
          { Rocks.optionsCreateIfMissing = True
          , Rocks.optionsCompression     = Rocks.NoCompression
          }
