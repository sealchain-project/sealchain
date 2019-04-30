{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.Tx
       ( Tx (..)
       , checkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF

       , TxId
       , TxAttributes

       , TxIn (..)

       , TxOut (..)
       , isOriginTxOut
       , isGDTxOut

       , TxValidationRules (..)
       , TxValidationRulesConfig (..)
       , mkLiveTxValidationRules
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson (FromJSON (..), FromJSONKey (..), 
                     FromJSONKeyFunction (..), ToJSON (toJSON), ToJSONKey (..),
                     Value (..), object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText, typeMismatch)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text as T
import           Formatting (Format, bprint, build, builder, int, sformat, (%))
import qualified Formatting.Buildable as Buildable
import           Numeric.Natural (Natural)
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF,
                     verifyGeneric)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     decodeKnownCborDataItem, cborError,
                     deriveIndexedBi, encodeKnownCborDataItem, encodeListLen,
                     enforceSize)
import           Pos.Core.Attributes (Attributes, areAttributesKnown,
                     unknownAttributesLength)
import           Pos.Core.Common (Address (..), Coin (..), GoldDollar (..), 
                     checkCoin, coinF, coinToInteger, 
                     goldDollarF, goldDollarToInteger, checkGoldDollar)
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Core.Util.LogSafe (SecureLog (..))
import           Pos.Crypto (Hash, decodeAbstractHash, hash, hashHexF,
                     shortHashF)
import           Pos.Util.Util (toAesonError)

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable Tx

instance Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%
             " with inputs "%listJson%", outputs: "%listJson % builder)
            (hash tx) _txInputs _txOutputs attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance Bi Tx where
    encode tx = encodeListLen 3
                <> encode (_txInputs tx)
                <> encode (_txOutputs tx)
                <> encode (_txAttributes tx)
    decode = do
        enforceSize "Tx" 3
        UnsafeTx <$> decode <*> decode <*> decode

    encodedSizeExpr size pxy = 1
        + size (_txInputs     <$> pxy)
        + size (_txOutputs    <$> pxy)
        + size (_txAttributes <$> pxy)

instance NFData Tx

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Verify inputs and outputs are non empty; have enough coins.
-- HLint suggested to use (||) instead of if then else. The suggestion
-- was significantly less readable/intuitive.
{-# ANN checkTx ("HLint: ignore" :: Text) #-}
checkTx
    :: MonadError Text m
    => TxValidationRules
    -> Tx
    -> m ()
checkTx txValRules it =
    case verRes of
        VerSuccess -> pure ()
        failure    -> throwError $ verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ 
        zipWith outputPredicates [0 ..] (toList (_txOutputs it)) <>
        zipWith otherPredicates [0 ..] (toList (_txOutputs it))

    outputPredicates (i :: Word) TxOut {..} =
        [ ( txOutValue > Coin 0
          , sformat
                ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue
          )
        , ( isRight (checkCoin txOutValue)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]
    outputPredicates (i :: Word) TxOutGD {..} =
        [ ( txOutGD > GoldDollar 0
          , sformat
                ("output #"%int%" has non-positive value: "%goldDollarF)
                i txOutGD
          )
        , ( isRight (checkGoldDollar txOutGD)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]
        -- The following rules check to see if we have passed a "cutoffEpoch"
        -- after which we reject transactions larger than a size specified
        -- in the `configuration.yaml` via the `TxValidationRules` struct.
    otherPredicates (i :: Word) _ =
        [( if currentEpoch > cutoffEpoch
                then (fromIntegral $ tvrTxAttrSize txValRules)
                      > unknownAttributesLength (_txAttributes it)
                else True
          , sformat
                ("size of Tx unknown attributes in input #"%int%" is too large")
                i
          )
        , ( if currentEpoch > cutoffEpoch
                then all ( < (fromIntegral $ tvrAddrAttrSize txValRules))
                         (map unknownAttributesLength txOutAddrAttribs)
                else True
          , sformat
                ("size of Address unknown attributes in input #"%int%" is too large")
                i
          )
        ]
    currentEpoch = tvrCurrentEpoch txValRules
    cutoffEpoch = tvrAddrAttrCutoff txValRules
    txOutAddresses = map txOutAddress (NE.toList (_txOutputs it))
    txOutAddrAttribs = map addrAttributes txOutAddresses

-- | Because there is no limit on the size of Attributes
-- (which allows unecessary bloating of the blockchain)
-- this struct introduces limits configurable via the
-- `configuration.yaml` file which are activated at
-- the `tvrAddrAttrCutoff` epoch.

data TxValidationRules = TxValidationRules
    { tvrAddrAttrCutoff :: !EpochIndex
    , tvrCurrentEpoch   :: !EpochIndex
    , tvrAddrAttrSize   :: !Natural
    , tvrTxAttrSize     :: !Natural
    } deriving (Eq, Generic, Show)

-- This second datatype goes into the `Configuration` config and is
-- read from disk (and thus doesn't know what the current epoch is).
-- The above `TxValidationRules` is has the current epoch inserted
-- and is passed down the validation call graph.
data TxValidationRulesConfig = TxValidationRulesConfig
    { tvrcAddrAttrCutoff :: !EpochIndex
    , tvrcAddrAttrSize   :: !Natural
    , tvrcTxAttrSize     :: !Natural
    } deriving (Eq, Generic, Show)

instance FromJSON TxValidationRulesConfig where
    parseJSON = withObject "txValidationRules" $ \v -> TxValidationRulesConfig
        <$> v .: "attribResrictEpoch"
        <*> v .: "addrAttribSize"
        <*> v .: "txAttribSize"

instance ToJSON TxValidationRulesConfig where
    toJSON (TxValidationRulesConfig aaCutoff aaSize taSize) =
        object [ "attribResrictEpoch" .= aaCutoff
               , "addrAttribSize" .= aaSize
               , "txAttribSize" .= taSize
               ]

mkLiveTxValidationRules :: EpochIndex -> TxValidationRulesConfig -> TxValidationRules
mkLiveTxValidationRules currentEpoch tvrc =
    TxValidationRules { tvrAddrAttrCutoff = tvrcAddrAttrCutoff tvrc
                      , tvrCurrentEpoch = currentEpoch
                      , tvrAddrAttrSize = tvrcAddrAttrSize tvrc
                      , tvrTxAttrSize = tvrcTxAttrSize tvrc
                      }

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

instance Buildable (SecureLog TxId) where
    build _ = "<txid>"

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Transaction arbitrary input.
data TxIn
      -- | TxId = Which transaction's output is used
      -- | Word32 = Index of the output in transaction's outputs
    = TxInUtxo TxId Word32
    deriving (Eq, Ord, Generic, Show, Typeable)

instance FromJSON TxIn where
    parseJSON v = toAesonError =<< txInFromText <$> parseJSON v

instance ToJSON TxIn where
    toJSON = toJSON . txInToText

instance FromJSONKey TxIn where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . txInFromText)

instance ToJSONKey TxIn where
    toJSONKey = toJSONKeyText txInToText

instance Hashable TxIn

instance Buildable TxIn where
    build (TxInUtxo txInHash txInIndex) =
        bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex

instance Bi TxIn where
    encode (TxInUtxo txInHash txInIndex) =
        encodeKnownCborDataItem (txInHash, txInIndex)
    decode = do
        uncurry TxInUtxo <$> decodeKnownCborDataItem

instance NFData TxIn

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
    ["TxInUtxo", h, idx]     -> TxInUtxo <$> decodeAbstractHash h <*> readEither idx
    _                        -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText (TxInUtxo txInHash txInIndex) =
    sformat ("TxInUtxo_"%hashHexF%"_"%int) txInHash txInIndex

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output.
data TxOut = 
    TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    }
    | TxOutGD
    { txOutAddress :: !Address
    , txOutGD      :: !GoldDollar
    }
     deriving (Eq, Ord, Generic, Show, Typeable)

instance FromJSON TxOut where
    parseJSON (Object o)
        | HM.member "txOut" o   = flip TxOut <$> ((o .: "txOut") >>= (.: "address"))
                                             <*> ((o .: "txOut") >>= (.: "coin"))
        | HM.member "txOutGD" o = flip TxOutGD <$> ((o .: "txOutGD") >>= (.: "address"))
                                               <*> ((o .: "txOutGD") >>= (.: "gd"))
    parseJSON invalid = typeMismatch "TxOut" invalid

instance ToJSON TxOut where
    toJSON (TxOut addr coin) =
        object [ "txOut" .= object [ "address" .= sformat build addr
                                   , "coin"    .= coinToInteger coin ]
               ]
    toJSON (TxOutGD addr gd) =
        object [ "txOutGD" .= object [ "address" .= sformat build addr
                                     , "gd"      .= goldDollarToInteger gd ]
               ]

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress
    build TxOutGD {..} =
        bprint ("TxOutGD "%goldDollarF%" -> "%build) txOutGD txOutAddress

instance NFData TxOut

makeLenses ''Tx

deriveIndexedBi ''TxOut [
    Cons 'TxOut [
        Field [| 0 :: Address |],
        Field [| 1 :: Coin    |]
    ],
    Cons 'TxOutGD [
        Field [| 0 :: Address |],
        Field [| 1 :: GoldDollar |]
    ]]

deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''Tx

deriveJSON defaultOptions ''Tx

isOriginTxOut :: TxOut -> Bool
isOriginTxOut TxOut{..} = True
isOriginTxOut _ = False

isGDTxOut :: TxOut -> Bool
isGDTxOut TxOutGD{..} = True
isGDTxOut _ = False