{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.TxAux
       ( TxAux (..)
       , txaF
       , checkTxAux
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting (Format, bprint, build, later, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Txp.Tx
import           Pos.Chain.Txp.TxWitness

-- | Transaction + auxiliary data
data TxAux = TxAux
    { taTx      :: !Tx
    , taWitness :: !TxWitness
    } deriving (Generic, Show, Eq)

instance NFData TxAux

-- | Specialized formatter for 'TxAux'.
txaF :: Format r (TxAux -> r)
txaF = later $ \(TxAux tx w) ->
    bprint (build%"\n"%"witnesses: "%build) tx w

instance Buildable TxAux where
    build = bprint txaF

-- | Check that a 'TxAux' is internally valid (checks that its 'Tx' is valid
-- via 'checkTx'). Does not check the witness.
checkTxAux
    :: MonadError Text m
    => TxValidationRules
    -> TxAux
    -> m ()
checkTxAux txValRules TxAux{..} = checkTx txValRules taTx

deriveSimpleBi ''TxAux [
    Cons 'TxAux [
        Field [| taTx       :: Tx        |],
        Field [| taWitness  :: TxWitness |]
    ]]

deriveJSON defaultOptions ''TxAux
