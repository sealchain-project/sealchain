{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Logic for local processing of transactions.
-- Local transaction is a transaction which has not yet been added to the blockchain.

module Pos.DB.Txp.Logic.Local
       ( TxpProcessTransactionMode
       , txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       , txGetPayload

       -- * Utils for transaction processing and mempool normalization
       , txProcessTransactionAbstract
       , txNormalizeAbstract
       ) where

import           Universum

import           Control.Monad.Except (mapExceptT, runExceptT, throwError)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import           Pos.Chain.Txp (ExtendedLocalToilM, LocalToilEnv (..), 
                     LocalToilState (..), MemPool, ToilVerFailure (..), 
                     TxAux (..), TxId, TxUndo, TxValidationRules (..), 
                     TxpConfiguration (..), UndoMap, Utxo, UtxoModifier, 
                     PactState, extendLocalToilM, mkLiveTxValidationRules, 
                     mpLocalTxs, normalizeToil, processTx, topsortTxs, 
                     utxoToLookup, defPactState)
import           Pos.Chain.Update (BlockVersionData)
import           Pos.Core (EpochIndex, SlotCount, siEpoch)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Core.JsonLog.LogEvents (MemPoolModifyReason (..))
import           Pos.Core.Reporting (reportError)
import           Pos.Core.Slotting (MonadSlots (..), epochOrSlotToEpochIndex,
                     getEpochOrSlot)
import           Pos.Crypto (WithHash (..))
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (MonadGState (..))
import           Pos.DB.GState.Common (getTip)
import           Pos.DB.GState.Lock (Priority (..), StateLock, StateLockMetrics,
                     withStateLock)
import           Pos.DB.Txp.Logic.Common (buildUtxo, unsafeGetStateRoot)
import           Pos.DB.Txp.MemState (GenericTxpLocalData (..), MempoolExt,
                     MonadTxpMem, TxpLocalWorkMode, getLocalTxsMap, getTxpTip,
                     getLocalUndos, getMemPool, getTxpExtra, getUtxoModifier, 
                     getPactState, setTxpLocalData, withTxpLocalData)
import           Pos.DB.Txp.Pact (GStateDb, newGStateDb)
import           Pos.Util.Util (HasLens')
import           Pos.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog,
                     logDebug, logError, logWarning)

type TxpProcessTransactionMode ctx m =
    ( TxpLocalWorkMode ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , MempoolExt m ~ ()
    , CanJsonLog m
    )

{-# INLINE txProcessTransaction #-}
-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: ( TxpProcessTransactionMode ctx m)
    => Genesis.Config ->  TxpConfiguration -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransaction genesisConfig txpConfig itw =
    withStateLock LowPriority ProcessTransaction $
        \_tip -> txProcessTransactionNoLock genesisConfig txpConfig itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => Genesis.Config
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionNoLock genesisConfig txpConfig = txProcessTransactionAbstract
    (configEpochSlots genesisConfig)
    genesisConfig
    buildContext
    processTxHoisted
  where
    buildContext :: Utxo -> TxAux -> m ()
    buildContext _ _ = pure ()

    processTxHoisted ::
           BlockVersionData
        -> TxValidationRules
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure (ExtendedLocalToilM () () GStateDb m) TxUndo
    processTxHoisted bvd txValRules = do
        mapExceptT extendLocalToilM
            ... (processTx (configProtocolMagic genesisConfig) txValRules txpConfig bvd)

txProcessTransactionAbstract ::
       forall extraEnv extraState ctx p m a.
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState, p ~ GStateDb)
    => SlotCount
    -> Genesis.Config
    -> (Utxo -> TxAux -> m extraEnv)
    -> (BlockVersionData -> TxValidationRules -> EpochIndex -> (TxId, TxAux) -> ExceptT ToilVerFailure (ExtendedLocalToilM extraEnv extraState p m) a)
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionAbstract epochSlots genesisConfig buildEnv txAction itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    -- Note: we need to read tip from the DB and check that it's the
    -- same as the one in mempool. That's because mempool state is
    -- valid only with respect to the tip stored there. Normally tips
    -- will match, because whenever we apply/rollback blocks we
    -- normalize mempool. However, there is a corner case when we
    -- receive an unexpected exception after modifying GState and
    -- before normalization. In this case normalization can fail and
    -- tips will differ. Rejecting transactions in this case should be
    -- fine, because the fact that we receive exceptions likely
    -- indicates that something is bad and we have more serious issues.
    --
    -- Also note that we don't need to use a snapshot here and can be
    -- sure that GState won't change, because changing it requires
    -- 'StateLock' which we own inside this function.
    tipDB <- lift getTip
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot epochSlots)
    utxoModifier <- withTxpLocalData getUtxoModifier
    utxo <- buildUtxo utxoModifier [txAux]
    extraEnv <- lift $ buildEnv utxo txAux
    bvd <- gsAdoptedBVData
    currentEpoch <- epochOrSlotToEpochIndex . getEpochOrSlot <$> getTipHeader
    let txValRulesConfig = configTxValRules $ genesisConfig
        txValRules = mkLiveTxValidationRules currentEpoch txValRulesConfig

    mp <- withTxpLocalData getMemPool
    undo <- withTxpLocalData getLocalUndos
    pactSt <- withTxpLocalData getPactState
    tip <- withTxpLocalData getTxpTip
    extra <- withTxpLocalData getTxpExtra
    pRes <- lift $ launchNamedPureLog id $ 
              processTransactionPure
              bvd
              txValRules
              epoch
              utxo
              extraEnv
              tipDB
              itw
              (utxoModifier, mp, undo, pactSt, tip, extra)
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right txpData -> do
            logDebug
                (sformat ("Transaction is processed successfully: " %build) txId)
            lift $ withTxpLocalData $ flip setTxpLocalData txpData

  where
    processTransactionPure
        :: BlockVersionData
        -> TxValidationRules
        -> EpochIndex
        -> Utxo
        -> extraEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> (UtxoModifier, MemPool, UndoMap, PactState, HeaderHash, extraState)
        -> NamedPureLogger m (Either ToilVerFailure (UtxoModifier, MemPool, UndoMap, PactState, HeaderHash, extraState))
    processTransactionPure bvd txValRules curEpoch utxo extraEnv tipDB tx (um, mp, undo, pactSt, tip, extraState)
        | tipDB /= tip = pure . Left $ ToilTipsMismatch tipDB tip
        | otherwise = do
            persister <- lift $ newGStateDb
            let initialEnv = LocalToilEnv { _lteUtxoLookup = (utxoToLookup utxo) 
                                          , _ltePersister = persister
                                          }
                initialState = LocalToilState { _ltsMemPool = mp
                                              , _ltsUtxoModifier = um
                                              , _ltsUndos = undo
                                              , _ltsPactState = pactSt
                                              }
            res :: (Either ToilVerFailure a, (LocalToilState, extraState)) <-
                    usingStateT (initialState, extraState) $
                    usingReaderT (initialEnv, extraEnv) $
                    runExceptT $
                    txAction bvd txValRules curEpoch tx
            case res of
                (Left er, _) -> pure $ Left er
                (Right _, (LocalToilState {..}, newExtraState)) -> pure $ Right
                    (_ltsUtxoModifier, _ltsMemPool, _ltsUndos, _ltsPactState, tip, newExtraState)
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

{-# INLINE txNormalize #-}
-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => Genesis.Config -> TxValidationRules -> TxpConfiguration -> m ()
txNormalize genesisConfig txValRules txpConfig = do
    txNormalizeAbstract (configEpochSlots genesisConfig) buildContext
        (normalizeToilHoisted txValRules)
  where
    buildContext :: Utxo -> [TxAux] -> m ()
    buildContext _ _ = pure ()

    normalizeToilHoisted
        :: TxValidationRules
        -> BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ExtendedLocalToilM () () GStateDb m ()
    normalizeToilHoisted txValRules' bvd epoch txs =
        extendLocalToilM
            $ normalizeToil (configProtocolMagic genesisConfig) txValRules' txpConfig bvd epoch
            $ HM.toList txs

txNormalizeAbstract ::
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState)
    => SlotCount
    -> (Utxo -> [TxAux] -> m extraEnv)
    -> (BlockVersionData -> EpochIndex -> HashMap TxId TxAux -> ExtendedLocalToilM extraEnv extraState GStateDb m ())
    -> m ()
txNormalizeAbstract epochSlots buildEnv normalizeAction =
    getCurrentSlot epochSlots >>= \case
        Nothing -> do
            tip <- getTip
            stateRoot <- unsafeGetStateRoot tip
            -- Clear and update tip
            withTxpLocalData $ flip setTxpLocalData (mempty, def, mempty, defPactState stateRoot, tip, def)
        Just (siEpoch -> epoch) -> do
            globalTip <- getTip
            localTxs <- withTxpLocalData getLocalTxsMap
            let txAuxes = toList localTxs
            utxo <- buildUtxo mempty txAuxes
            extraEnv <- buildEnv utxo txAuxes
            bvd <- gsAdoptedBVData

            persister <- newGStateDb
            stateRoot <- unsafeGetStateRoot globalTip
            let initialEnv = LocalToilEnv { _lteUtxoLookup = (utxoToLookup utxo) 
                                          , _ltePersister = persister
                                          }
            let initialState =
                    LocalToilState
                        { _ltsMemPool = def
                        , _ltsUtxoModifier = mempty
                        , _ltsUndos = mempty
                        , _ltsPactState = defPactState stateRoot  -- | clear PactState every slot
                        }
            (LocalToilState {..}, newExtraState) <-
                launchNamedPureLog id $
                execStateT
                    (runReaderT
                         (normalizeAction bvd epoch localTxs)
                         (initialEnv, extraEnv))
                    (initialState, def)
            withTxpLocalData $ flip setTxpLocalData
                ( _ltsUtxoModifier
                , _ltsMemPool
                , _ltsUndos
                , _ltsPactState
                , globalTip
                , newExtraState)

-- | Get 'TxPayload' from mempool to include into a new block which
-- will be based on the given tip. In something goes wrong, empty
-- payload is returned. That's because we would sooner create an empty
-- block to maintain decent chain quality than skip block creation.
--
-- We need to explicitly check that tip matches, even though we do
-- mempool normalization whenever we apply/rollback a block. That's
-- because we can't make them both atomically, i. e. can't guarantee
-- that either none or both of them will be done.
txGetPayload :: (MonadIO m, MonadTxpMem ext ctx m, WithLogger m) => HeaderHash -> m [TxAux]
txGetPayload neededTip = do
    (view mpLocalTxs -> memPool, memPoolTip) <- withTxpLocalData $ \(TxpLocalData{..}) ->
        (,) <$> readTVar txpMemPool <*> readTVar txpTip
    let tipMismatchMsg =
            sformat
                ("txGetPayload: tip mismatch (in DB: )"%build%
                 ", (in mempool: "%build%")")
                neededTip memPoolTip
    let topsortFailMsg = "txGetPayload: topsort failed!"
    let convertTx (txId, txAux) = WithHash (taTx txAux) txId
    case (memPoolTip == neededTip, topsortTxs convertTx $ HM.toList memPool) of
        (False, _)       -> [] <$ logWarning tipMismatchMsg
        (True, Nothing)  -> [] <$ logError topsortFailMsg
        (True, Just res) -> return $ map snd res
