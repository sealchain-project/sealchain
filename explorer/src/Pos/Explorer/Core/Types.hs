-- | Module containing explorer-specific datatypes
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Explorer.Core.Types
       ( TxExtra (..)
       , AddrHistory
       ) where

import           Universum

import           Formatting ((%), int, bprint)
import qualified Formatting.Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Txp (TxId, TxUndo)
import           Pos.Core (Timestamp, Coin (..), GoldDollar (..))
import           Pos.Core.Chrono (NewestFirst)

type AddrHistory = NewestFirst [] TxId

data TxExtra = TxExtra
    { teBlockchainPlace :: !(Maybe (HeaderHash, Word32))
    , teReceivedTime    :: !(Maybe Timestamp)
    -- non-strict on purpose, see comment in `processTxDo` in Pos.Explorer.Txp.Local
    , teInputOutputs    :: TxUndo
    } deriving (Show, Generic, Eq)

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Field [| teReceivedTime    :: Maybe Timestamp            |],
        Field [| teInputOutputs    :: TxUndo                     |]
    ]]

instance Buildable (Coin, GoldDollar) where
    build (Coin c, GoldDollar gd) = bprint ("("%int%" coins, "%int%" gds)") c gd
