{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Client.Txp.Currency
    ( Currency (..)
    ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Formatting (Format)

import           Pos.Core (Coin (..), GoldDollar (..),
                     maxCoinVal, getCoin, coinF,
                     maxGoldDollarVal, getGoldDollar, goldDollarF)
import           Pos.Util.Util (leftToPanic)

class (Bounded c, Ord c, Eq c) => Currency c where 
    mkMoney :: Word64 -> c

    checkMoney :: MonadError Text m => c -> m ()

    formatter :: Format r (c -> r)

    unsafeGetMoney :: c -> Word64

    sumMoneys :: (Container moneys, Element moneys ~ c) => moneys -> Integer
    sumMoneys = sum . map moneyToInteger . toList

    moneyToInteger :: c -> Integer
    moneyToInteger = toInteger . unsafeGetMoney
    {-# INLINE moneyToInteger #-}

    unsafeAddMoney :: c -> c -> c
    unsafeAddMoney (unsafeGetMoney -> a) (unsafeGetMoney -> b)
        | res >= a && res >= b && res <= unsafeGetMoney (maxBound @c) = mkMoney res
        | otherwise =
        error $ "unsafeAddMoney: overflow when summing " <> show a <> " + " <> show b
      where
        res = a+b
    {-# INLINE unsafeAddMoney #-}

    addMoney :: c -> c -> Maybe c
    addMoney (unsafeGetMoney -> a) (unsafeGetMoney -> b)
        | res >= a && res >= b && res <= unsafeGetMoney (maxBound @c) = Just $ mkMoney res
        | otherwise = Nothing
      where
        res = a+b
    {-# INLINE addMoney #-}

    subMoney :: c -> c -> Maybe c
    subMoney (unsafeGetMoney -> a) (unsafeGetMoney -> b)
        | a >= b = Just (mkMoney (a-b))
        | otherwise = Nothing
    
    unsafeSubMoney :: c -> c -> c
    unsafeSubMoney a b = fromMaybe (error "unsafeSubMoney: underflow") (subMoney a b)
    {-# INLINE unsafeSubMoney #-}

    unsafeMulMoney :: Integral a => c -> a -> c
    unsafeMulMoney (unsafeGetMoney -> a) b
        | res <= moneyToInteger (maxBound @c) = mkMoney (fromInteger res)
        | otherwise = error "unsafeMulMoney: overflow"
      where
        res = toInteger a * toInteger b

    divMoney :: Integral a => c -> a -> c
    divMoney (unsafeGetMoney -> a) b =
        mkMoney (fromInteger (toInteger a `div` toInteger b))

    integerToMoney :: Integer -> Either Text c
    integerToMoney n
        | n < 0 = Left $ "integerToMoney: value is negative (" <> show n <> ")"
        | n <= moneyToInteger (maxBound :: c) = pure $ mkMoney (fromInteger n)
        | otherwise = Left $ "integerToMoney: value is too big (" <> show n <> ")"

    unsafeIntegerToMoney :: Integer -> c
    unsafeIntegerToMoney n = leftToPanic "unsafeIntegerToMoney: " (integerToMoney n)
    {-# INLINE unsafeIntegerToMoney #-}

instance Currency Coin where
    mkMoney c = either error (const coin) (checkMoney coin)
      where
        coin = (Coin c)
    {-# INLINE mkMoney #-}

    checkMoney (Coin c)
        | c <= maxCoinVal = pure ()
        | otherwise       = throwError $ "Coin: " <> show c <> " is too large"

    formatter = coinF

    unsafeGetMoney = getCoin
    {-# INLINE unsafeGetMoney #-}

instance Currency GoldDollar where
    mkMoney c = either error (const gd) (checkMoney gd)
      where
        gd = (GoldDollar c)
    {-# INLINE mkMoney #-}

    checkMoney (GoldDollar c)
        | c <= maxGoldDollarVal = pure ()
        | otherwise       = throwError $ "GoldDollar: " <> show c <> " is too large"

    formatter = goldDollarF

    unsafeGetMoney = getGoldDollar
    {-# INLINE unsafeGetMoney #-}