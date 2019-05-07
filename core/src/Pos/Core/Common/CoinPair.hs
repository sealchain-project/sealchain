{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Common.CoinPair
       ( CoinPair
       , zeroCoinPair
       , addCoinPair
       , subCoinPair
       , unsafeAddCoinPair
       , unsafeSubCoinPair
       , unsafeIntegerPairToCoinPair
       , sumCoinPairs
       , checkCoinPair
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Formatting ((%), int, bprint)
import qualified Formatting.Buildable

import           Pos.Core.Common.Coin
import           Pos.Core.Common.GoldDollar

type CoinPair = (Coin, GoldDollar)

instance {-# OVERLAPPING #-} Buildable CoinPair where
    build (Coin c, GoldDollar gd) = 
        bprint ("("%int%" coins, "%int%" gds)") c gd

zeroCoinPair :: CoinPair
zeroCoinPair = (mkCoin 0, mkGoldDollar 0)

addCoinPair :: CoinPair -> CoinPair -> Maybe CoinPair
addCoinPair (c1, gd1) (c2, gd2) = (,) <$> addCoin c1 c2 <*> addGoldDollar gd1 gd2

subCoinPair :: CoinPair -> CoinPair -> Maybe CoinPair
subCoinPair (c1, gd1) (c2, gd2) = (,) <$> subCoin c1 c2 <*> subGoldDollar gd1 gd2

unsafeAddCoinPair :: CoinPair -> CoinPair -> CoinPair
unsafeAddCoinPair (c1, gd1) (c2, gd2) =
    (unsafeAddCoin c1 c2, unsafeAddGoldDollar gd1 gd2)

unsafeSubCoinPair :: CoinPair -> CoinPair -> CoinPair
unsafeSubCoinPair (c1, gd1) (c2, gd2) =
    (unsafeSubCoin c1 c2, unsafeSubGoldDollar gd1 gd2)

unsafeIntegerPairToCoinPair :: (Integer, Integer) -> CoinPair
unsafeIntegerPairToCoinPair (c, gd) =
    (unsafeIntegerToCoin c, unsafeIntegerToGoldDollar gd)

sumCoinPairs
    :: (Container coins, Element coins ~ CoinPair)
    => coins -> (Integer, Integer)
sumCoinPairs ls =
    let coinSum = sum . map (coinToInteger . fst) $ toList ls 
        gdSum = sum . map (goldDollarToInteger . snd) $ toList ls
    in (coinSum, gdSum)

checkCoinPair :: MonadError Text m => CoinPair -> m ()
checkCoinPair (coin, gd) = do
    _ <- checkCoin coin  
    checkGoldDollar gd