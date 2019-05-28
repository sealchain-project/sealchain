{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Pos.Chain.Txp.Toil.Pact.Command
  ( Command (..), cmdPayload, cmdSigners, cmdHash, verifyCommand
  , ProcessedCommand(..), _ProcSucc, _ProcFail
  , Payload (..), pPayload
  , ParsedCode (..), pcCode, pcExps
  , CommandResult (..), crGas
  ) where

import           Universum

import           Control.Lens hiding ((.=))
import           Data.ByteString (ByteString)
import           Data.Aeson as A

import           Pact.Types.Runtime
import           Pact.Types.RPC
import           Pact.Parse

data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigners :: !(Set PublicKey)
  , _cmdHash    :: !Hash
  } deriving (Eq,Show,Ord,Generic,Functor,Foldable,Traversable)

instance NFData a => NFData (Command a)

-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand a =
  ProcSucc !(Command (Payload a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (ProcessedCommand a)

verifyCommand :: Command ByteString -> ProcessedCommand ParsedCode
verifyCommand orig@Command{..} = case ppcmdPayload' of
      Right env' -> ProcSucc $ orig { _cmdPayload = env' }
      e          -> ProcFail $ "Invalid command: " ++ toErrStr e
  where
    ppcmdPayload' = traverse parsePact =<< A.eitherDecodeStrict' _cmdPayload
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
  { _pPayload :: !(PactRPC c)
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (Payload a)
instance (ToJSON a) => ToJSON (Payload a) where toJSON = lensyToJSON 1
instance (FromJSON a) => FromJSON (Payload a) where parseJSON = lensyParseJSON 1

data CommandResult = CommandResult
  { _crGas    :: Gas
  } deriving (Eq,Show)

makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
