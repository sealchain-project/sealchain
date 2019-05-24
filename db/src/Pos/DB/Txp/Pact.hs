{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores pact datas.

module Pos.DB.Txp.Pact
       ( PactOp (..)
       ) where


import           Universum

import qualified Database.RocksDB as Rocks
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Base16 (base16F)

import           Pos.DB (RocksBatchOp (..))

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data PactOp = PactOp ByteString ByteString

instance Buildable PactOp where
    build (PactOp key val)           =
        bprint ("PactOp ("%base16F%", "%base16F%")") key val

instance RocksBatchOp PactOp where
    toBatchOp (PactOp key val) = [Rocks.Put key val]
