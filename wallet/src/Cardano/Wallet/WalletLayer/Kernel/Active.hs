{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.WalletLayer.Kernel.Active (
    pay
  , issue
  , estimateFees
  ) where

import           Universum

import           Data.Time.Units (Second)
import qualified Serokell.Util.Base16 as Base16

import           Pos.Chain.Txp (Tx (..))
import           Pos.Core (AddrAttributes (..), Address (..), Coin, GoldDollar, CoinPair)
import           Pos.Core.Attributes (Attributes (..))
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.V1.Types (unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal (walletProtocolMagic)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.WalletLayer (EstimateFeesError (..),
                     NewPaymentError (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Kernel.Conv

-- | Generates a new payment @and submit it as pending@.
pay :: MonadIO m
    => Kernel.ActiveWallet
    -> PassPhrase
    -> V1.Payment
    -> m (Either NewPaymentError (Tx, TxMeta))
pay activeWallet pw payment = liftIO $ do
    genesisConfig <- Node.getCoreConfig (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $
      runExceptT $ do
        (accId, payees) <- withExceptT NewPaymentWalletIdDecodingFailed $
                                   setupPayment payment

        -- Verify that all payee addresses are of the same `NetworkMagic`
        -- as our `ActiveWallet`.
        let nm = makeNetworkMagic $ Kernel.walletPassive activeWallet ^. walletProtocolMagic
        ExceptT $ pure $ verifyPayeesNM nm payees

        -- Pay the payees
        withExceptT NewPaymentError $ ExceptT $
            Kernel.pay genesisConfig activeWallet pw accId payees

-- | Verifies that the `NetworkMagic` of each payee address matches the
-- provided `NetworkMagic`.
verifyPayeesNM
    :: NetworkMagic
    -> NonEmpty (Address, CoinPair)
    -> Either NewPaymentError ()
verifyPayeesNM nm payees =
    case nonEmpty invalidPayees of
        Nothing -> Right ()
        Just is -> Left $ NewPaymentAddressBadNetworkMagic nm is
  where
    addressHasValidMagic :: AddrAttributes -> Bool
    addressHasValidMagic addrAttrs = nm == (aaNetworkMagic addrAttrs)
    --
    verifyPayeeNM
        :: (Address, CoinPair)
        -> Either Address ()
    verifyPayeeNM (addr, _)
        | (addressHasValidMagic ((attrData . addrAttributes) addr)) = Right ()
        | otherwise = Left addr
    --
    invalidPayees :: [Address]
    invalidPayees = fst $ partitionEithers (toList (map verifyPayeeNM payees))

-- | Generates a new issurance @and submit it as pending@.
issue :: MonadIO m
    => Kernel.ActiveWallet
    -> PassPhrase
    -> V1.Issurance
    -> m (Either NewPaymentError (Tx, TxMeta)) -- | TODO xl rename error name
issue activeWallet pw issurance = liftIO $ do
    genesisConfig <- Node.getCoreConfig (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $
      runExceptT $ do
        (accId, issuedGds, proof) <- withExceptT NewPaymentWalletIdDecodingFailed $
                                   setupIssurance issurance
        withExceptT NewPaymentError $ ExceptT $
            Kernel.issue genesisConfig activeWallet pw accId issuedGds proof


-- | Estimates the fees for a payment.
estimateFees :: MonadIO m
             => Kernel.ActiveWallet
             -> V1.Payment
             -> m (Either EstimateFeesError Coin)
estimateFees activeWallet payment = liftIO $ do
    genesisConfig <- Node.getCoreConfig (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
      runExceptT $ do
        (accId, payees) <- withExceptT EstimateFeesWalletIdDecodingFailed $
                                   setupPayment payment
        withExceptT EstimateFeesError $ ExceptT $
          Kernel.estimateFees genesisConfig activeWallet accId payees

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupAccount :: Monad m
             => V1.PaymentSource
             -> ExceptT Text m HD.HdAccountId
setupAccount paymentSource = do
    rootId <- fromRootId wId
    let accIx  = HD.HdAccountIx (V1.getAccIndex . V1.psAccountIndex $ paymentSource)
        accId  = HD.HdAccountId {
                     _hdAccountIdParent = rootId
                   , _hdAccountIdIx     = accIx
                   }
    return accId
  where
    wId = V1.psWalletId paymentSource

setupPayment :: Monad m
             => V1.Payment
             -> ExceptT Text m ( HD.HdAccountId
                               , NonEmpty (Address, CoinPair)
                               )
setupPayment payment = do
    accId <- setupAccount $ V1.pmtSource payment
    return (accId, payees)
  where
    payees = (\(V1.PaymentDistribution a c) -> (unV1 a, unV1 c)) <$>
                V1.pmtDestinations payment

setupIssurance :: Monad m
               => V1.Issurance
               -> ExceptT Text m ( HD.HdAccountId
                                 , GoldDollar
                                 , ByteString
                                 )
setupIssurance issurance = do
    accId <- setupAccount $ V1.issSource issurance
    proof <- exceptT (Base16.decode $ V1.iiProof issuranceInfo)
    return (accId, issuedGds, proof)
  where
    issuranceInfo = V1.issInfo issurance
    issuedGds = unV1 $ V1.iiIncrement issuranceInfo
