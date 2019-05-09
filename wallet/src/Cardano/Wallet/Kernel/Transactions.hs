{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.Transactions (
      pay
    , issue
    -- * Errors
    , NewTransactionError(..)
    , SignTransactionError(..)
    , PaymentError(..)
    , EstimateFeesError(..)
    , NumberOfMissingUtxos(..)
    , mkStdTx
    , submitSignedTx
    -- * Internal & testing use only low-level APIs
    , newTransaction
    , toMeta
  ) where

import           Universum

import           Control.Lens (makeLenses, to)
import           Control.Monad.Except (MonadError (..), withExceptT)
import           Control.Retry (RetryPolicyM, RetryStatus, applyPolicy,
                     fullJitterBackoff, limitRetries, retrying)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (DB, NewPendingError)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal (ActiveWallet (..), walletNode)
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.Pending (PartialTxMeta, newPending)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedTx (..), WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core
import           Cardano.Wallet.WalletLayer.Kernel.Conv (exceptT)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (Tx (..), TxAux (..), TxId,
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), Utxo)
import           Pos.Chain.Txp as Core (TxAux, TxIn, TxOut,
                     TxOutAux, toaOut, txOutAddress)
import           Pos.Client.Txp (HasTxFeePolicy (..), MonadAddresses (..), TxError, 
                     InputSelectionPolicy (..), PendingAddresses (..), 
                     makeMPubKeyTxAddrs, createMTx, createGDIssuanceTx) --, estimateTxFee)
import           Pos.Core (Address, GoldDollar, CoinPair, mkCoin, mkGoldDollar)
import qualified Pos.Core as Core
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic,
                     PublicKey, SafeSigner (..), ShouldCheckPassphrase (..), 
                     Signature (..), hash,)

{-------------------------------------------------------------------------------
  Monad for call client module
-------------------------------------------------------------------------------}

data ClientEnv = ClientEnv
  { _ceActiveWallet :: ActiveWallet
  , _cePassPhrase   :: PassPhrase
  , _ceAccountId    :: HdAccountId
  , _ceSigners      :: (Address -> Maybe SafeSigner)
  }

makeLenses ''ClientEnv

type ClientMode m = ReaderT ClientEnv (ExceptT NewTransactionError m)

instance MonadIO m => HasTxFeePolicy (ClientMode m) where
    getTxFeePolicy = do
        env <- ask
        let activeWallet = env ^. ceActiveWallet
        lift . liftIO $ Node.getFeePolicy (walletPassive activeWallet ^. walletNode)

instance MonadIO m => MonadAddresses (ClientMode m) where
    type AddrData (ClientMode m)  = ()
    getNewAddress _ _ _ = createNewAddress
    getFakeChangeAddress _ _ = createNewAddress

createNewAddress :: MonadIO m => ClientMode m Address
createNewAddress = do
    env <- ask
    let passiveWallet = walletPassive $ env ^. ceActiveWallet
    let spendingPassword = env ^. cePassPhrase
    let srcAccountId = env ^. ceAccountId
    lift $ withExceptT NewTransactionErrorCreateAddressFailed $ ExceptT $ liftIO $
        Kernel.createAddress spendingPassword
                            (AccountIdHdRnd srcAccountId) $
                            passiveWallet

runClientMode :: 
       ActiveWallet 
    -> PassPhrase 
    -> HdAccountId 
    -> (Address -> Maybe SafeSigner)
    -> ClientMode m a 
    -> ExceptT NewTransactionError m a
runClientMode activeWallet spendingPassword accountId hdwSigners action = do
    let env = ClientEnv { _ceActiveWallet = activeWallet
                        , _cePassPhrase   = spendingPassword
                        , _ceAccountId    = accountId
                        , _ceSigners      = hdwSigners
                        }
    runReaderT action env

clientCreateMTx
    :: MonadIO m
    => Genesis.Config
    -> NonEmpty TxOutAux
    -> Utxo
    -> ClientMode m (TxAux, NonEmpty TxOut)
clientCreateMTx genesisConfig outputs utxo = do
    env <- ask
    let hdwSigners = env ^. ceSigners
    resultE <- createMTx 
               genesisConfig 
               (PendingAddresses Set.empty) 
               OptimizeForSecurity 
               utxo 
               hdwSigners
               outputs 
               ()
    case resultE of 
        Left e       -> throwError $ NewTransactionClientError e
        Right result -> return result 

clientCreateGDIssuranceTx
    :: MonadIO m
    => Genesis.Config
    -> GoldDollar
    -> ByteString
    -> Utxo
    -> ClientMode m (TxAux, NonEmpty TxOut)
clientCreateGDIssuranceTx genesisConfig issuedGDs proof utxo = do
    env <- ask
    let hdwSigners = env ^. ceSigners
    resultE <- createGDIssuanceTx 
               genesisConfig 
               (PendingAddresses Set.empty) 
               OptimizeForSecurity 
               utxo 
               hdwSigners 
               issuedGDs 
               proof 
               ()
    case resultE of 
        Left e       -> throwError $ NewTransactionClientError e
        Right result -> return result 

-- clientEstimateTxFee
--     :: MonadIO m
--     => Genesis.Config
--     -> Utxo
--     -> NonEmpty TxOutAux
--     -> ClientMode m TxFee
-- clientEstimateTxFee genesisConfig utxo outputs = do
--     resultE <- estimateTxFee genesisConfig (PendingAddresses Set.empty) OptimizeForSecurity utxo outputs
--     case resultE of 
--         Left e       -> throwError $ NewTransactionClientError e
--         Right result -> return result 

{-------------------------------------------------------------------------------
  Generating payments and estimating fees
-------------------------------------------------------------------------------}
-- TODO xl delete this 
data NumberOfMissingUtxos = NumberOfMissingUtxos Int

instance Buildable NumberOfMissingUtxos where
    build (NumberOfMissingUtxos number) =
        bprint ("NumberOfMissingUtxos " % build) number

instance Arbitrary NumberOfMissingUtxos where
    arbitrary = oneof [ NumberOfMissingUtxos <$> arbitrary
                      ]

data NewTransactionError =
    NewTransactionUnknownAccount UnknownHdAccount
  | NewTransactionUnknownAddress UnknownHdAddress
--   | NewTransactionErrorCoinSelectionFailed CoinSelHardErr
  | NewTransactionErrorCreateAddressFailed Kernel.CreateAddressError
  | NewTransactionErrorSignTxFailed SignTransactionError
  | NewTransactionInvalidTxIn
  | NewTransactionNotEnoughUtxoFragmentation NumberOfMissingUtxos
  | NewTransactionClientError TxError

instance Buildable NewTransactionError where
    build (NewTransactionUnknownAccount err) =
        bprint ("NewTransactionUnknownAccount " % build) err
    build (NewTransactionUnknownAddress err) =
        bprint ("NewTransactionUnknownAddress " % build) err
    -- build (NewTransactionErrorCoinSelectionFailed err) =
    --     bprint ("NewTransactionErrorCoinSelectionFailed " % build) err
    build (NewTransactionErrorCreateAddressFailed err) =
        bprint ("NewTransactionErrorCreateAddressFailed " % build) err
    build (NewTransactionErrorSignTxFailed err) =
        bprint ("NewTransactionErrorSignTxFailed " % build) err
    build NewTransactionInvalidTxIn =
        bprint "NewTransactionInvalidTxIn"
    build (NewTransactionNotEnoughUtxoFragmentation err) =
        bprint ("NewTransactionNotEnoughUtxoFragmentation" % build) err
    build (NewTransactionClientError err) =
        bprint ("NewTransactionClientError" % build) err


instance Arbitrary NewTransactionError where
    arbitrary = oneof [
        NewTransactionUnknownAccount <$> arbitrary
      , NewTransactionErrorCreateAddressFailed <$> arbitrary
      , NewTransactionErrorSignTxFailed <$> arbitrary
      , pure NewTransactionInvalidTxIn
      , NewTransactionNotEnoughUtxoFragmentation <$> arbitrary
      ]

data PaymentError = PaymentNewTransactionError NewTransactionError
                  | PaymentNewPendingError NewPendingError
                  | PaymentSubmissionMaxAttemptsReached
                  -- ^ When trying to send the newly-created transaction via
                  -- 'newPending' and the submission layer, we hit the number
                  -- of retries/max time allocated for the operation.
                  | PaymentNoHdAddressForSrcAddress HD.UnknownHdAddress
                  -- ^ When we don't have HD-address corresponding to source
                  -- address of transaction.

instance Buildable PaymentError where
    build (PaymentNewTransactionError txErr) =
        bprint ("PaymentNewTransactionError " % build) txErr
    build (PaymentNewPendingError npe) =
        bprint ("PaymentNewPendingError " % build) npe
    build PaymentSubmissionMaxAttemptsReached =
        bprint "PaymentSubmissionMaxAttemptsReached"
    build (PaymentNoHdAddressForSrcAddress addrErr) =
        bprint ("PaymentNoHdAddressForSrcAddress" % build) addrErr

-- | Workhorse kernel function to perform a payment.
pay :: Genesis.Config
    -> ActiveWallet
    -> PassPhrase
    -> HdAccountId
    -- ^ The source HD Account from where the payment was originated
    -> NonEmpty (Address, CoinPair)
    -- ^ The payees
    -> IO (Either PaymentError (Tx, TxMeta))
pay genesisConfig activeWallet spendingPassword accountId payees = do
    newTransactionAndSubmit activeWallet accountId $
        newPayment genesisConfig activeWallet spendingPassword accountId payees

-- | Workhorse kernel function to perform a payment.
issue :: Genesis.Config
      -> ActiveWallet
      -> PassPhrase
      -- -> CoinSelectionOptions
      -> HdAccountId
      -- ^ The source HD Account from where the payment was originated
      -> GoldDollar
      -- ^ GD to issued.
      -> ByteString
    -- ^ proof of issurance.
      -> IO (Either PaymentError (Tx, TxMeta))
issue genesisConfig activeWallet spendingPassword accountId issuedGDs proof = do
    newTransactionAndSubmit activeWallet accountId $
        newIssurance genesisConfig activeWallet spendingPassword accountId issuedGDs proof

-- | Workhorse kernel function to perform a transaction. It includes logic to
-- stop trying to perform a payment if the payment would take more than 30
-- seconds, as well as internally retrying up to 5 times to propagate the
-- transaction via 'newPending'.
newTransactionAndSubmit 
    :: ActiveWallet
    -> HdAccountId
    -- ^ The source HD Account from where the payment was originated
    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
    -- ^ new transaction action
    -> IO (Either PaymentError (Tx, TxMeta))
newTransactionAndSubmit activeWallet accountId action = do
    retrying retryPolicy shouldRetry $ \rs -> do
        res <- action
        case res of
             Left e      -> return (Left $ PaymentNewTransactionError e)
             Right (txAux, partialMeta, _utxo) -> do
                 succeeded <- newPending activeWallet accountId txAux partialMeta
                 case succeeded of
                      Left e   -> do
                          -- If the next retry would bring us to the
                          -- end of our allowed retries, we fail with
                          -- a proper error
                          retriesLeft <- applyPolicy retryPolicy rs
                          return . Left $ case retriesLeft of
                               Nothing ->
                                   PaymentSubmissionMaxAttemptsReached
                               Just _  ->
                                   PaymentNewPendingError e
                      Right meta -> return $ Right (taTx $ txAux, meta)

-- See <https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter>
retryPolicy :: RetryPolicyM IO
retryPolicy = fullJitterBackoff 5000000 <> limitRetries 6

-- If this is a hard coin selection error we cannot recover, stop
-- retrying. If we get a 'Tx' as output, stop retrying immediately.
shouldRetry :: RetryStatus -> Either PaymentError a -> IO Bool
shouldRetry _ (Right _)                             = return False
shouldRetry _ (Left (PaymentNewTransactionError _)) = return False
shouldRetry _ _                                     = return True

-- | Submits externally-signed transaction to the blockchain.
-- The result of this function is equal to the result of 'pay' function.
submitSignedTx
    :: ActiveWallet
    -> Tx
    -> NonEmpty (Address, Signature TxSigData, PublicKey)
    -> IO (Either PaymentError (Tx, TxMeta))
submitSignedTx aw@ActiveWallet{..} tx srcAddrsWithProofs =
    retrying retryPolicy shouldRetry $ \rs -> do
        res <- runExceptT $ do
            -- STEP 0: get wallet snapshot.
            snapshot <- liftIO $ getWalletSnapshot walletPassive
            -- STEP 1: create witnesses.
            -- Since we already received inputs signatures with corresponding derived PKs,
            -- just form witnesses from them.
            let witnesses = V.fromList . NonEmpty.toList $ flip NonEmpty.map srcAddrsWithProofs $
                    \(_srcAddr, txSig, derivedPK) -> PkWitness derivedPK txSig

            -- STEP 2: make 'TxAux'.
            let txAux = TxAux tx witnesses

            -- STEP 3: Compute metadata
            let txId = hash tx
            -- Currently it's assumed that all source addresses belong to 'the same/ account,
            -- so we can just take the first source address to find our 'HdAccountId'.
            let (firstSrcAddress, _, _) = NonEmpty.head srcAddrsWithProofs
            firstSrcHdAddress <- withExceptT PaymentNoHdAddressForSrcAddress $ exceptT $
                lookupCardanoAddress snapshot firstSrcAddress
            let (HD.HdAddress (HD.HdAddressId srcAccountId _) _) = firstSrcHdAddress
            -- We use `getCreationTimestamp` provided by the `NodeStateAdaptor`
            -- to compute the createdAt timestamp for `TxMeta`.
            txMetaCreatedAt_ <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)

            -- STEP 4: Get available UTxO
            utxo <- withExceptT (PaymentNewTransactionError . NewTransactionUnknownAccount) $ exceptT $
                currentAvailableUtxo snapshot srcAccountId

            let inputs = NonEmpty.toList $ _txInputs tx
                maybeInputsWithCoins = map (collectInputCoins utxo) inputs
                inputsWithCoins = NonEmpty.fromList $ catMaybes maybeInputsWithCoins
            -- If 'tx' is valid (i.e. contains correct inputs), we already have
            -- all inputs with corresponding coins.
            let numberOfValidInputs = NonEmpty.length inputsWithCoins
                numberOfAllInputs = length maybeInputsWithCoins
            if numberOfValidInputs /= numberOfAllInputs then
                -- Something is wrong with inputs, this 'tx' should be rejected.
                ExceptT $ return $ Left $ PaymentNewTransactionError NewTransactionInvalidTxIn
            else do
                -- We have to calculate the sum of input coins.
                let spentInputCoins = paymentAmount . NonEmpty.toList $ (toaOut . snd <$> inputsWithCoins)

                -- STEP 5: form meta-data.
                partialMeta <- liftIO $ createNewMeta srcAccountId
                                                      txId
                                                      txMetaCreatedAt_
                                                      inputsWithCoins
                                                      (_txOutputs tx)
                                                      spentInputCoins
                -- STEP 6: our new pending tx.
                withExceptT PaymentNewPendingError $ ExceptT $
                    newPending aw srcAccountId txAux partialMeta
        case res of
            Left e -> do
                -- If the next retry would bring us to the end of our allowed retries,
                -- we fail with a proper error
                retriesLeft <- applyPolicy retryPolicy rs
                return $ case retriesLeft of
                    Nothing -> Left PaymentSubmissionMaxAttemptsReached
                    Just _  -> Left e
            Right meta ->
                return $ Right (tx, meta)
  where
    -- If utxo is valid, we definitely know that .
    collectInputCoins :: Utxo
                      -> TxIn
                      -> Maybe (TxIn, TxOutAux)
    collectInputCoins utxo txInput = case Map.lookup txInput utxo of
        Nothing       -> Nothing
        Just txOutput -> Just (txInput, txOutput)

-- | new transaction for payment
newPayment
    :: Genesis.Config
    -> ActiveWallet
    -> PassPhrase
    -- ^ The spending password.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> NonEmpty (Address, CoinPair)
    -- ^ The payees.
    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newPayment genesisConfig aw spendingPassword accountId payees = do
    let outputs = NonEmpty.fromList $ concatMap toTxOuts payees
    newTransaction aw spendingPassword accountId $
        clientCreateMTx genesisConfig outputs 

-- | new transaction for issurance
newIssurance
    :: Genesis.Config
    -> ActiveWallet
    -> PassPhrase
    -- ^ The spending password.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> GoldDollar
    -- ^ GD to issued.
    -> ByteString
    -- ^ proof of issurance.
    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newIssurance genesisConfig aw spendingPassword accountId issuedGDs proof = do
    newTransaction aw spendingPassword accountId $
        clientCreateGDIssuranceTx genesisConfig issuedGDs proof

-- | low-level function, Creates a new 'TxAux' and corresponding 'TxMeta',
-- without submitting it to the network.
newTransaction
    :: ActiveWallet
    -> PassPhrase
    -- ^ The spending password.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> (Utxo -> ClientMode IO (TxAux, NonEmpty TxOut))
    -- ^ The action.
    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newTransaction aw@ActiveWallet{..} spendingPassword accountId clientAction = do
    let pm = walletPassive ^. Internal.walletProtocolMagic
        nm = makeNetworkMagic pm

    db <- getWalletSnapshot walletPassive
    mbEsk <- Keystore.lookup
               nm
               (WalletIdHdRnd $ accountId ^. hdAccountIdParent)
               (walletPassive ^. Internal.walletKeystore)

    let getSigner :: Address -> Maybe SafeSigner
        getSigner addr = 
            let signerE = mkSigner nm spendingPassword mbEsk db addr
            in case signerE of
                Left _       -> Nothing
                Right signer -> Just signer

    runExceptT $ do
        availableUtxo <- withExceptT NewTransactionUnknownAccount $ exceptT $
                         currentAvailableUtxo db accountId

        (txAux, inputOuts) <- runClientMode aw spendingPassword accountId getSigner $
                              clientAction availableUtxo
       
        txMetaCreatedAt_  <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)
        let txId = hash $ taTx txAux
            txInputs = _txInputs $ taTx txAux
            txOutputs = _txOutputs $ taTx txAux 
            spentInputs = paymentAmount $ NonEmpty.toList inputOuts
            inputOutsAux = map TxOutAux inputOuts
        partialMeta <- liftIO $ 
            createNewMeta accountId txId txMetaCreatedAt_ (NonEmpty.zip txInputs inputOutsAux) txOutputs spentInputs
        return (txAux, partialMeta, availableUtxo)       

toTxOuts :: (Address, CoinPair) -> [TxOutAux]
toTxOuts (addr, (coins, gds)) = 
    let sealOut | coins == (mkCoin 0)   = []
                | otherwise             = [TxOut addr coins]
        gdOut   | gds == (mkGoldDollar 0) = []
                | otherwise               = [TxOutGD addr gds]
    in map TxOutAux $ sealOut <> gdOut

-- | This is called when we create a new Pending Transaction.
-- This actually returns a function because we don`t know yet our outputs.
createNewMeta :: HdAccountId -> TxId -> Core.Timestamp -> NonEmpty (TxIn, TxOutAux) -> [TxOut] -> CoinPair -> IO PartialTxMeta
createNewMeta hdId txId time inp out spentInputs = do
    -- this partially applied function indicates the lack of all TxMeta at this stage.
    return $ metaForNewTx time hdId txId inp out spentInputs

metaForNewTx  :: Core.Timestamp -> HdAccountId -> TxId -> NonEmpty (TxIn, TxOutAux) -> [TxOut] -> CoinPair -> CoinPair -> TxMeta
metaForNewTx time accountId txId inputs outputs (spentCoins, spentGDs) (gainedCoins, gainedGDs) =
    TxMeta {
          _txMetaId = txId
        , _txMetaAmount = absCoin spentCoins gainedCoins
        , _txMetaGDAmount = absGoldDollar spentGDs gainedGDs
        , _txMetaInputs = inputsForMeta
        , _txMetaOutputs = aux <$> outputs
        , _txMetaCreationAt = time
        , _txMetaIsOutgoing = gainedCoins < spentCoins -- it`s outgoing if our inputs spent are more than the new utxo.
        , _txMetaIsGDOutgoing = gainedGDs < spentGDs
        , _txMetaWalletId = _fromDb $ getHdRootId (accountId ^. hdAccountIdParent)
        , _txMetaAccountIx = getHdAccountIx $ accountId ^. hdAccountIdIx
    }
  where
    inputsForMeta = toInput <$> inputs
    aux txOut = (txOutAddress txOut, toCoinPair' txOut)
    toInput (TxInUtxo txid index, txOutAux) = 
            let (addr, coins) = aux $ toaOut txOutAux
            in (txid, index, addr, coins)

-- | Different wraper for @metaForNewTx@ mainly for testing Only NewPending Transactions.
toMeta :: Core.Timestamp -> HdAccountId -> RawResolvedTx -> PartialTxMeta
toMeta time accountId UnsafeRawResolvedTx{..} outCoin =
    let txId = hash . taTx $ rawResolvedTx
        (txIn :: NonEmpty TxIn) = _txInputs $ taTx rawResolvedTx
        (inputsRes :: NonEmpty TxOutAux) = rawResolvedTxInputs
        spentInputCoins = paymentAmount . NonEmpty.toList $ toaOut <$> inputsRes
        inputs = NonEmpty.zip txIn inputsRes
        txOut = _txOutputs $ taTx rawResolvedTx
    in metaForNewTx time accountId txId inputs txOut spentInputCoins outCoin

{-------------------------------------------------------------------------------
  Estimating fees
-------------------------------------------------------------------------------}

data EstimateFeesError = EstFeesTxCreationFailed NewTransactionError

instance Buildable EstimateFeesError where
    build (EstFeesTxCreationFailed newTxErr) =
        bprint ("EstFeesTxCreationFailed " % build) newTxErr

instance Arbitrary EstimateFeesError where
    arbitrary = EstFeesTxCreationFailed <$> arbitrary

-- estimateFees 
--     :: Genesis.Config
--     -> ActiveWallet
--     -> PassPhrase
--     -> HdAccountId
--     -- ^ The source HD Account from where the payment should originate
--     -> NonEmpty (Address, CoinPair)
--     -- ^ The payees
--     -> IO (Either EstimateFeesError Coin)
-- estimateFees genesisConfig aw@ActiveWallet{..} spendingPassword accountId payees = do
--     -- res <- newUnsignedTransaction activeWallet options accountId payees
--     -- case res of
--     --      Left e  -> return . Left . EstFeesTxCreationFailed $ e
--     --      Right (_db, _tx, fees, _originalUtxo) -> do
--     --          -- sanity check of fees is done.
--     --          return $ Right fees
--     let outputs = NonEmpty.fromList $ concatMap toTxOuts payees

--     db <- getWalletSnapshot walletPassive
--     runExceptT $ do
--         availableUtxo <- withExceptT EstFeesTxCreationFailed $ 
--                          withExceptT NewTransactionUnknownAccount $ exceptT $
--                          currentAvailableUtxo db accountId

--         TxFee coins <- withExceptT EstFeesTxCreationFailed $ 
--                        runClientMode aw spendingPassword accountId $
--                        clientEstimateTxFee genesisConfig availableUtxo outputs
--         return coins

-- | Errors during transaction signing
--
-- NOTE: Under normal circumstances these should /never/ be thrown. If they
-- do, it most likely indicates a bug.
data SignTransactionError =
    SignTransactionMissingKey Address
  | SignTransactionErrorUnknownAddress Address
  | SignTransactionErrorNotOwned Address

instance Buildable SignTransactionError where
    build (SignTransactionMissingKey addr) =
        bprint ("SignTransactionMissingKey " % build) addr
    build (SignTransactionErrorUnknownAddress addr) =
        bprint ("SignTransactionErrorUnknownAddress " % build) addr
    build (SignTransactionErrorNotOwned addr) =
        bprint ("SignTransactionErrorNotOwned " % build) addr

-- in order to be able to generate an Arbitrary address we'd need to use
-- the cardano-sl-core test package
instance Arbitrary SignTransactionError where
    arbitrary = oneof
        [ SignTransactionMissingKey <$> arbitrary
        , SignTransactionErrorUnknownAddress <$> arbitrary
        , SignTransactionErrorNotOwned <$> arbitrary
        ]

mkSigner :: NetworkMagic
         -> PassPhrase
         -> Maybe EncryptedSecretKey
         -> DB
         -> Address
         -> Either SignTransactionError SafeSigner
mkSigner _ _ Nothing _ addr = Left (SignTransactionMissingKey addr)
mkSigner nm spendingPassword (Just esk) snapshot addr =
    case Getters.lookupCardanoAddress snapshot addr of
        Left _ -> Left (SignTransactionErrorUnknownAddress addr)
        Right hdAddr ->
            let addressIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdIx
                                       . to HD.getHdAddressIx
                accountIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdParent
                                       . HD.hdAccountIdIx
                                       . to HD.getHdAccountIx
                res = Core.deriveLvl2KeyPair nm
                                             (Core.IsBootstrapEraAddr True)
                                             (ShouldCheckPassphrase False)
                                             spendingPassword
                                             esk
                                             accountIndex
                                             addressIndex
            -- eks address fix - we need to use the esk as returned
            -- from Core.deriveLvl2KeyPair rather than rely on the
            -- one from encrypted secret key delivered to mkSigner
            in case res of
                 Just (a, eskAddr) | a == addr ->
                     Right (SafeSigner eskAddr spendingPassword)
                 _otherwise              ->
                     Left (SignTransactionErrorNotOwned addr)

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => ProtocolMagic
        -> (forall a. [a] -> m [a])
        -- ^ Shuffle function
        -> (Core.Address -> Either e SafeSigner)
        -- ^ Signer for each input of the transaction
        -> NonEmpty (Core.TxIn, Core.TxOutAux)
        -- ^ Selected inputs
        -> [Core.TxOutAux]
        -- ^ Selected outputs
        -> [Core.TxOutAux]
        -- ^ Change outputs
        -> m (Either e Core.TxAux)
mkStdTx pm shuffleFun hdwSigners inps outs change = do
    allOuts <- shuffleFun $ outs <> change
    return $ makeMPubKeyTxAddrs pm hdwSigners (fmap repack inps) allOuts
    where
         -- | Repacks a utxo-derived tuple into a format suitable for
         -- 'TxOwnedInputs'.
        repack :: (Core.TxIn, Core.TxOutAux) -> (Core.TxOut, Core.TxIn)
        repack (txIn, aux) = (Core.toaOut aux, txIn)
