
module Pos.Client.Txp.Failure
    ( TxError (..)
    , isNotEnoughMoneyTxError
    , isCheckedTxError
    ) where

import           Universum

import           Formatting (bprint, build, stext, (%))
import qualified Formatting.Buildable

import           Pos.Core (Address)


data TxError =
      NotEnoughMoney
      -- ^ Parameter: how much more money is needed
    | NotEnoughAllowedMoney
      -- ^ Parameter: how much more money is needed and which available input addresses
      -- are present in output addresses set
    | FailedToStabilize
      -- ^ Parameter: how many attempts were performed
    | OutputIsRedeem !Address
      -- ^ One of the tx outputs is a redemption address
    | RedemptionDepleted
      -- ^ Redemption address has already been used
    | SafeSignerNotFound !Address
      -- ^ The safe signer at the specified address was not found
    | SignedTxNotBase16Format
      -- ^ Externally-signed transaction is not in Base16-format.
    | SignedTxUnableToDecode !Text
      -- ^ Externally-signed transaction cannot be decoded.
    | SignedTxSignatureNotBase16Format
      -- ^ Signature of externally-signed transaction is not in Base16-format.
    | SignedTxInvalidSignature !Text
      -- ^ Signature of externally-signed transaction is invalid.
    | OutputContainsState
      -- ^ Signature of externally-signed transaction is invalid.
    | NotGDIssuer
      -- ^ Not has the permission to issue
    | GeneralTxError !Text
      -- ^ Parameter: description of the problem
    deriving (Show, Generic)

isNotEnoughMoneyTxError :: TxError -> Bool
isNotEnoughMoneyTxError = \case
    NotEnoughMoney{}        -> True
    NotEnoughAllowedMoney{} -> True
    _                       -> False

instance Exception TxError

instance Buildable TxError where
    build NotEnoughMoney =
        bprint ("Transaction creation error: not enough money")
    build NotEnoughAllowedMoney =
        bprint ("Transaction creation error: not enough money on addresses which are not included \
                \in output addresses set")
    build FailedToStabilize =
        "Transaction creation error: failed to stabilize fee"
    build (OutputIsRedeem addr) =
        bprint ("Output address "%build%" is a redemption address") addr
    build RedemptionDepleted =
        bprint "Redemption address balance is 0"
    build (SafeSignerNotFound addr) =
        bprint ("Address "%build%" has no associated safe signer") addr
    build SignedTxNotBase16Format =
        "Externally-signed transaction is not in Base16-format."
    build (SignedTxUnableToDecode msg) =
        bprint ("Unable to decode externally-signed transaction: "%stext) msg
    build SignedTxSignatureNotBase16Format =
        "Signature of externally-signed transaction is not in Base16-format."
    build (SignedTxInvalidSignature msg) =
        bprint ("Signature of externally-signed transaction is invalid: "%stext) msg
    build OutputContainsState =
        bprint ("Outputs contains TxOutState")
    build NotGDIssuer =
        bprint ("Not the GD issuer")
    build (GeneralTxError msg) =
        bprint ("Transaction creation error: "%stext) msg

isCheckedTxError :: TxError -> Bool
isCheckedTxError = \case
    NotEnoughMoney{}        -> True
    NotEnoughAllowedMoney{} -> True
    FailedToStabilize{}     -> False
    OutputIsRedeem{}        -> True
    RedemptionDepleted{}    -> True
    SafeSignerNotFound{}    -> True
    SignedTxNotBase16Format{}          -> True
    SignedTxUnableToDecode{}           -> True
    SignedTxSignatureNotBase16Format{} -> True
    SignedTxInvalidSignature{}         -> True
    OutputContainsState{}   -> True
    NotGDIssuer{}           -> True
    GeneralTxError{}        -> True
