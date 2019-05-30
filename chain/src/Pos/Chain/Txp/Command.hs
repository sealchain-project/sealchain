{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.Command
  ( Command (..)
  , cmdPayload
  , verifyCommand
  , ProcessedCommand (..)
  , _ProcSucc
  , _ProcFail
  , Payload (..)
  , pPayload
  , pGasPrice
  , ParsedCode (..)
  , pcCode
  , pcExps
  , CommandResult (..)
  , crGas
  ) where

import           Universum

import           Control.Lens hiding ((.=))
import           Data.ByteString (ByteString)
import           Data.Aeson

import           Pact.Types.Gas
import           Pact.Types.Runtime
import           Pact.Types.RPC
import           Pact.Parse

import           Pos.Core.Common (Coin)

data Command a = Command
  { _cmdPayload  :: !a
  } deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand a =
  ProcSucc !(Command (Payload a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

verifyCommand :: Command ByteString -> ProcessedCommand ParsedCode
verifyCommand orig@Command{..} = case ppcmdPayload' of
      Right env' -> ProcSucc $ orig { _cmdPayload = env' }
      e          -> ProcFail $ "Invalid command: " ++ toErrStr e
  where
    ppcmdPayload' = traverse parsePact =<< eitherDecodeStrict' _cmdPayload
    parsePact :: Text -> Either String ParsedCode
    parsePact code = ParsedCode code <$> parseExprs code
    toErrStr :: Either String a -> String
    toErrStr (Right _) = ""
    toErrStr (Left s) = s ++ "; "
{-# INLINE verifyCommand #-}

-- | Pair parsed Pact expressions with the original text.
data ParsedCode = ParsedCode
  { _pcCode :: !Text
  , _pcExps :: ![Exp Parsed]
  } deriving (Eq,Show,Generic)
instance NFData ParsedCode

data Payload c = Payload
  { _pPayload  :: !(PactRPC c)
  , _pGasPrice :: !Coin
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (Payload a)

instance (ToJSON a) => ToJSON (Payload a) where 
  toJSON (Payload payload gasPrice) =
    object [ "payload" .= toJSON payload 
           , "gasPrice" .= toJSON gasPrice
           ]

instance (FromJSON a) => FromJSON (Payload a) where 
  parseJSON =
      withObject "Payload" $ \o ->
        Payload <$> (o .: "payload")
                <*> (o .: "gasPrice")

data CommandResult = CommandResult
  { _crGas    :: Gas
  } deriving (Eq,Show)

makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
