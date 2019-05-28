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
  ( Command(..),cmdPayload, cmdSigners, cmdHash, verifyCommand
  , ProcessedCommand(..),_ProcSucc,_ProcFail
  , Payload(..),pPayload
  , ParsedCode(..),pcCode,pcExps
  , CommandError(..),ceMsg,ceDetail
  , CommandSuccess(..),csData
  , CommandResult(..),crResult,crGas
  , ExecutionMode(..), emTxId
  ) where

import           Universum

import           Control.Lens hiding ((.=))
import           Data.ByteString (ByteString)
import           Data.Aeson as A

import           Pact.Types.Runtime
import           Pact.Types.Orphans ()
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

data CommandError = CommandError {
      _ceMsg    :: String
    , _ceDetail :: Maybe String
}

instance ToJSON CommandError where
    toJSON (CommandError m d) =
        object $ [ "status" .= ("failure" :: String)
                 , "error" .= m ] ++
        maybe [] ((:[]) . ("detail" .=)) d

newtype CommandSuccess a = CommandSuccess { _csData :: a }
  deriving (Eq, Show)

instance (ToJSON a) => ToJSON (CommandSuccess a) where
    toJSON (CommandSuccess a) =
        object [ "status" .= ("success" :: String)
               , "data" .= a ]

instance (FromJSON a) => FromJSON (CommandSuccess a) where
    parseJSON = withObject "CommandSuccess" $ \o ->
        CommandSuccess <$> o .: "data"

data CommandResult = CommandResult
  { _crResult :: Value
  , _crGas    :: Gas
  } deriving (Eq,Show)

data ExecutionMode =
    Transactional { _emTxId :: TxId } 
    | Local
    deriving (Eq,Show)

makeLenses ''ExecutionMode
makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandError
makeLenses ''CommandSuccess
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
makePrisms ''ExecutionMode
