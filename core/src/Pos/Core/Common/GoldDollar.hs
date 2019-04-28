{-# LANGUAGE RankNTypes #-}
module Pos.Core.Common.GoldDollar
       ( GoldDollar (..)
       , mkGoldDollar
       , checkGoldDollar
       , goldDollarF

       , maxGoldDollarVal
       , sumGoldDollars

       -- * Conversions
       , unsafeGetGoldDollar
       , goldDollarToInteger
       , integerToGoldDollar
       , unsafeIntegerToGoldDollar

       -- * Arithmetic operations
       , unsafeAddGoldDollar
       , unsafeSubGoldDollar
       , unsafeMulGoldDollar
       , addGoldDollar
       , subGoldDollar
       , mulGoldDollar
       , divGoldDollar
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.Data (Data)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable
import qualified Text.JSON.Canonical as Canonical (FromJSON (..),
                     ReportSchemaErrors, ToJSON (..))
import           Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Util.Json.Canonical ()
import           Pos.Util.Util (leftToPanic)

-- | GoldDollar is the least possible unit of currency.
newtype GoldDollar = GoldDollar
    { getGoldDollar :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable GoldDollar where
    build (GoldDollar n) = bprint (int%" gd(s)") n

instance Bounded GoldDollar where
    minBound = GoldDollar 0
    maxBound = GoldDollar maxGoldDollarVal

instance Bi GoldDollar where
    encode = encode . unsafeGetGoldDollar
    decode = GoldDollar <$> decode
    encodedSizeExpr size pxy = size (unsafeGetGoldDollar <$> pxy)

instance Monad m => Canonical.ToJSON m GoldDollar where
    toJSON = Canonical.toJSON @_ @Word64 . unsafeGetGoldDollar  -- i. e. String

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m GoldDollar where
    fromJSON = fmap GoldDollar . Canonical.fromJSON

instance Aeson.FromJSON GoldDollar where
    parseJSON v = mkGoldDollar <$> Aeson.parseJSON v

instance Aeson.ToJSON GoldDollar where
    toJSON = Aeson.toJSON . unsafeGetGoldDollar

instance FromHttpApiData GoldDollar where
    parseUrlPiece p = do
        c <- GoldDollar <$> parseQueryParam p
        checkGoldDollar c
        pure c

instance ToHttpApiData GoldDollar where
    toQueryParam = pretty . goldDollarToInteger

-- | Maximal possible value of 'GoldDollar'.
maxGoldDollarVal :: Word64
maxGoldDollarVal = maxBound @Word64

-- | Makes a 'GoldDollar' but is _|_ if that coin exceeds 'maxGoldDollarVal'.
-- You can also use 'checkGoldDollar' to do that check.
mkGoldDollar :: Word64 -> GoldDollar
mkGoldDollar n = either error (const gd) (checkGoldDollar gd)
  where
    gd = (GoldDollar n)
{-# INLINE mkGoldDollar #-}

checkGoldDollar :: MonadError Text m => GoldDollar -> m ()
checkGoldDollar (GoldDollar c)
    | c <= maxGoldDollarVal = pure ()
    | otherwise       = throwError $ "GoldDollar: " <> show c <> " is too large"

-- | GoldDollar formatter which restricts type.
goldDollarF :: Format r (GoldDollar -> r)
goldDollarF = build

-- | Unwraps 'GoldDollar'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetGoldDollar :: GoldDollar -> Word64
unsafeGetGoldDollar = getGoldDollar
{-# INLINE unsafeGetGoldDollar #-}

-- | Compute sum of all coins in container. Result is 'Integer' as a
-- protection against possible overflow. If you are sure overflow is
-- impossible, you can use 'unsafeIntegerToGoldDollar'.
sumGoldDollars
    :: (Container coins, Element coins ~ GoldDollar)
    => coins -> Integer
sumGoldDollars = sum . map goldDollarToInteger . toList

goldDollarToInteger :: GoldDollar -> Integer
goldDollarToInteger = toInteger . unsafeGetGoldDollar
{-# INLINE goldDollarToInteger #-}

-- Addition of coins. Returns 'Nothing' in case of overflow.
addGoldDollar :: GoldDollar -> GoldDollar -> Maybe GoldDollar
addGoldDollar (unsafeGetGoldDollar -> a) (unsafeGetGoldDollar -> b)
    | res >= a && res >= b && res <= unsafeGetGoldDollar (maxBound @GoldDollar) = Just (GoldDollar res)
    | otherwise = Nothing
  where
    res = a+b
{-# INLINE addGoldDollar #-}

-- | Only use if you're sure there'll be no overflow.
unsafeAddGoldDollar :: GoldDollar -> GoldDollar -> GoldDollar
unsafeAddGoldDollar a b =
    case addGoldDollar a b of
        Just r -> r
        Nothing ->
            error $ "unsafeAddGoldDollar: overflow when summing " <> show a <> " + " <> show b
{-# INLINE unsafeAddGoldDollar #-}

-- | Subtraction of coins. Returns 'Nothing' when the subtrahend is bigger
-- than the minuend, and 'Just' otherwise.
subGoldDollar :: GoldDollar -> GoldDollar -> Maybe GoldDollar
subGoldDollar (unsafeGetGoldDollar -> a) (unsafeGetGoldDollar -> b)
    | a >= b = Just (GoldDollar (a-b))
    | otherwise = Nothing

-- | Only use if you're sure there'll be no underflow.
unsafeSubGoldDollar :: GoldDollar -> GoldDollar -> GoldDollar
unsafeSubGoldDollar a b = fromMaybe (error "unsafeSubGoldDollar: underflow") (subGoldDollar a b)
{-# INLINE unsafeSubGoldDollar #-}

-- | Multiplication between 'GoldDollar's. Returns 'Nothing' in case of overflow.
mulGoldDollar :: Integral a => GoldDollar -> a -> Maybe GoldDollar
mulGoldDollar (unsafeGetGoldDollar -> a) b
    | res <= goldDollarToInteger (maxBound @GoldDollar) = Just $ GoldDollar (fromInteger res)
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b
{-# INLINE mulGoldDollar #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulGoldDollar :: Integral a => GoldDollar -> a -> GoldDollar
unsafeMulGoldDollar a b =
    case mulGoldDollar a b of
         Just r  -> r
         Nothing -> error "unsafeMulGoldDollar: overflow"
{-# INLINE unsafeMulGoldDollar #-}

divGoldDollar :: Integral b => GoldDollar -> b -> GoldDollar
divGoldDollar (unsafeGetGoldDollar -> a) b = GoldDollar (a `div` fromIntegral b)
{-# INLINE divGoldDollar #-}

integerToGoldDollar :: Integer -> Either Text GoldDollar
integerToGoldDollar n
    | n < 0 = Left $ "integerToGoldDollar: value is negative (" <> show n <> ")"
    | n <= goldDollarToInteger (maxBound :: GoldDollar) = pure $ GoldDollar (fromInteger n)
    | otherwise = Left $ "integerToGoldDollar: value is too big (" <> show n <> ")"

unsafeIntegerToGoldDollar :: Integer -> GoldDollar
unsafeIntegerToGoldDollar n = leftToPanic "unsafeIntegerToGoldDollar: " (integerToGoldDollar n)
{-# INLINE unsafeIntegerToGoldDollar #-}

-- Place this here to avoid TH staging issues.
deriveSafeCopySimple 0 'base ''GoldDollar

