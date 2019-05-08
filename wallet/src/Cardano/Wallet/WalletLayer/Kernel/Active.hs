{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.WalletLayer.Kernel.Active (
    pay
  -- , estimateFees
  ) where

import           Universum

import           Data.Time.Units (Second)

import           Pos.Chain.Txp (Tx (..))
import           Pos.Core (AddrAttributes (..), Address (..), CoinPair)
import           Pos.Core.Attributes (Attributes (..))
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.V1.Types (unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
-- import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
--                      (CoinSelectionOptions (..), ExpenseRegulation,
--                      InputGrouping, newOptions)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal (walletProtocolMagic)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.WalletLayer ( --EstimateFeesError (..),
                     NewPaymentError (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Kernel.Conv

-- | Generates a new transaction @and submit it as pending@.
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

-- | Estimates the fees for a payment.
-- estimateFees :: MonadIO m
--              => Kernel.ActiveWallet
--              -> InputGrouping
--              -> ExpenseRegulation
--              -> V1.Payment
--              -> m (Either EstimateFeesError Coin)
-- estimateFees activeWallet grouping regulation payment = liftIO $ do
--     policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
--     limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
--       runExceptT $ do
--         (opts, accId, payees) <- withExceptT EstimateFeesWalletIdDecodingFailed $
--                                    setupPayment policy grouping regulation payment
--         withExceptT EstimateFeesError $ ExceptT $
--           Kernel.estimateFees activeWallet opts accId payees

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: Monad m
             => V1.Payment
             -> ExceptT Text m ( HD.HdAccountId
                               , NonEmpty (Address, CoinPair)
                               )
setupPayment payment = do
    rootId <- fromRootId wId
    let accIx  = HD.HdAccountIx (V1.getAccIndex . V1.psAccountIndex . V1.pmtSource $ payment)
        accId  = HD.HdAccountId {
                     _hdAccountIdParent = rootId
                   , _hdAccountIdIx     = accIx
                   }
        payees = (\(V1.PaymentDistribution a c) -> (unV1 a, unV1 c)) <$>
                   V1.pmtDestinations payment
    return (accId, payees)
  where
    wId = V1.psWalletId . V1.pmtSource $ payment
