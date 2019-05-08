{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
-- | Utility functions on core types
--
-- Intended for qualified import
--
-- > import qualified Cardano.Wallet.Kernel.Util.Core as Core
module Cardano.Wallet.Kernel.Util.Core (
    -- * General utility functions
    absCoin
  , absGoldDollar
  , derefIn
  , getCurrentTimestamp
  , fromUtxo
  , nothingToZero
  , sumCoinPairsUnsafe
  , toOutPair
  , toOutPair'
    -- * UTxO
  , utxoBalance
  , unsafeUtxoBalance
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoUnions
  , toCoinPair
  , toCoinPair'
  , toAddress
    -- * Transactions
  , paymentAmount
  , txOuts
  , txIns
  , txAuxId
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Serokell.Util (enumerate)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hash)

import           UTxO.Util

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- | Get current timestamp
--
-- NOTE: we are abandoning the 'Mockable time' strategy of core.
getCurrentTimestamp :: IO Core.Timestamp
getCurrentTimestamp = Core.Timestamp . round . (* 1000000) <$> getPOSIXTime

{-------------------------------------------------------------------------------
  UTxO
-------------------------------------------------------------------------------}

-- | Computes the balance for this UTxO
--
-- This returns an 'Integer' rather than a 'Coin' because the outputs of a
-- block may sum to more than 'maxCoinVal' (if some outputs of the transactions
-- in the block are used as inputs by other transactions in that block).
utxoBalance :: Core.Utxo -> (Integer, Integer)
utxoBalance = foldl' updateFn (0, 0) . Map.elems
  where
    updateFn :: (Integer, Integer) -> Core.TxOutAux -> (Integer, Integer)
    updateFn (coins, gds) (Core.TxOutAux Core.TxOut{..}) = (coins + Core.coinToInteger txOutValue, gds)
    updateFn (coins, gds) (Core.TxOutAux Core.TxOutGD{..}) = (coins, gds + Core.goldDollarToInteger txOutGD)
    updateFn (coins, gds) (Core.TxOutAux _) = (coins, gds)

unsafeUtxoBalance :: Core.Utxo -> (Core.Coin, Core.GoldDollar)
unsafeUtxoBalance = Core.unsafeIntegerPairToCoinPair . utxoBalance

-- | Restricts the 'Utxo' to only the selected set of inputs.
utxoRestrictToInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRestrictToInputs = restrictKeys

utxoRemoveInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRemoveInputs = withoutKeys

utxoUnions :: [Core.Utxo] -> Core.Utxo
utxoUnions = Map.unions

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Calculates the amount of a requested payment.
paymentAmount :: [Core.TxOut] -> Core.CoinPair
paymentAmount = Core.unsafeIntegerPairToCoinPair
              . Core.sumCoinPairs
              . map toCoinPair'

txOuts :: Core.Tx -> Core.Utxo
txOuts tx = Map.fromList $ map (toTxInOut (hash tx)) (outs tx)

txIns :: Core.TxAux -> Set Core.TxIn
txIns = Set.fromList . NE.toList . Core._txInputs . Core.taTx

txAuxId :: Core.TxAux -> Core.TxId
txAuxId = hash . Core.taTx

{-------------------------------------------------------------------------------
  External auxiliary
-------------------------------------------------------------------------------}

sumCoinPairsUnsafe :: (Container coins, Element coins ~ Core.CoinPair)
         => coins -> Core.CoinPair
sumCoinPairsUnsafe ls = 
  let (coinSum, gdSum) = Core.sumCoinPairs ls
  in (Core.unsafeIntegerToCoin coinSum, Core.unsafeIntegerToGoldDollar gdSum)

-- | This is not unsafe although we use unsafeGetCoin, because
-- this is not actually unsafe either.
absCoin :: Core.Coin -> Core.Coin -> Core.Coin
absCoin ca cb
    | a >= b = Core.Coin (a-b)
    | otherwise = Core.Coin (b-a)
    where
      a = Core.unsafeGetCoin ca
      b = Core.unsafeGetCoin cb

absGoldDollar :: Core.GoldDollar -> Core.GoldDollar -> Core.GoldDollar
absGoldDollar ca cb
    | a >= b = Core.GoldDollar (a - b)
    | otherwise = Core.GoldDollar (b - a)
    where
      a = Core.unsafeGetGoldDollar ca
      b = Core.unsafeGetGoldDollar cb

nothingToZero :: Ord a => a -> Map a Core.CoinPair -> Core.CoinPair
nothingToZero acc mp = case Map.lookup acc mp of
    Nothing -> Core.zeroCoinPair
    Just n  -> n

toOutPair :: Core.TxOutAux -> (Core.Address, Core.CoinPair)
toOutPair = toOutPair' . Core.toaOut

toOutPair' :: Core.TxOut -> (Core.Address, Core.CoinPair)
toOutPair' Core.TxOut{..} = (txOutAddress, (txOutValue, Core.mkGoldDollar 0))
toOutPair' Core.TxOutGD{..} = (txOutAddress, (Core.mkCoin 0, txOutGD))
toOutPair' Core.TxOutState{..} = (txOutAddress, (Core.mkCoin 0, Core.mkGoldDollar 0))

fromUtxo :: Core.Utxo -> [(Core.Address, Core.CoinPair)]
fromUtxo utxo = toOutPair <$> Map.elems utxo

derefIn :: Core.TxIn -> (Core.TxId, Word32)
derefIn (Core.TxInUtxo txId ix) = (txId, ix)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}
toCoinPair :: Core.TxOutAux -> Core.CoinPair
toCoinPair = toCoinPair' . Core.toaOut

toCoinPair' :: Core.TxOut -> Core.CoinPair
toCoinPair' = snd . toOutPair'

toAddress :: Core.TxOutAux -> Core.Address
toAddress = Core.txOutAddress . Core.toaOut

outs :: Core.Tx -> [(Word32, Core.TxOut)]
outs tx = enumerate $ toList $ tx ^. Core.txOutputs

toTxInOut :: Core.TxId -> (Word32, Core.TxOut) -> (Core.TxIn, Core.TxOutAux)
toTxInOut txId (idx, out) = (Core.TxInUtxo txId idx, Core.TxOutAux out)
