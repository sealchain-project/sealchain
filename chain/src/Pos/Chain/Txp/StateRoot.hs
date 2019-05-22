{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric #-}

module Pos.Chain.Txp.StateRoot 
  ( StateRoot(..)
  , emptyStateRoot
  ) where

import           Universum

import           Data.Data (Data)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint)
import qualified Formatting.Buildable
import           Serokell.Util.Base16 (base16F)

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi

-- | Represents the root state of Sealchain 
data StateRoot = StateRoot 
    { getStatRoot :: ByteString
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable StateRoot where
    build = bprint base16F . getStatRoot

instance Bi StateRoot where
  encode = Bi.encode . getStatRoot 
  decode = StateRoot <$> Bi.decode

emptyStateRoot :: StateRoot
emptyStateRoot = StateRoot mempty

deriveSafeCopySimple 0 'base ''StateRoot