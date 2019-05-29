{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

-- | Logic for global processing of transactions.  Global transaction
-- is a transaction which has already been added to the blockchain.

module Pos.DB.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ProcessBlundsSettings (..)
       , processBlunds
       , applyBlocksWith
       , blundToAuxNUndo
       ) where

import           Universum

import           Control.Lens (magnify, zoom)
import           Control.Monad.Except (throwError)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Formatting (build, sformat, (%))

import           Pos.Chain.Block (ComponentBlock (..))
import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBootStakeholders)
import           Pos.Chain.Genesis (GenesisWStakeholders)
import           Pos.Chain.Txp (ExtendedGlobalToilM, GlobalToilEnv (..),
                     GlobalToilM, GlobalToilState (..), StakesView (..),
                     ToilVerFailure, TxAux, TxUndo, TxValidationRules (..),
                     TxValidationRulesConfig (..), TxpConfiguration (..),
                     TxpUndo, Utxo, UtxoModifier, PactState (..), applyToil,
                     defGlobalToilState, flattenTxPayload, gtsUtxoModifier,
                     gtsPactState, mkLiveTxValidationRules, rollbackToil, 
                     runGlobalToilM, utxoToLookup, verifyToil, defPactState)
import           Pos.Core (epochIndexL, mkCoin)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Exception (assertionFailed)
import           Pos.Core.Slotting (epochOrSlotToEpochIndex, getEpochOrSlot)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB (SomeBatchOp (..), getTipHeader)
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.DB.GState.Common (getTip)
import           Pos.DB.GState.Stakes (getRealStake, getRealTotalStake)
import           Pos.DB.Txp.Logic.Common (buildUtxo, buildUtxoForRollback, 
                     defaultGasModel, unsafeGetStateRoot)
import           Pos.DB.Txp.Settings (TxpBlock, TxpBlund, TxpCommonMode,
                     TxpGlobalApplyMode, TxpGlobalRollbackMode,
                     TxpGlobalSettings (..), TxpGlobalVerifyMode)
import           Pos.DB.Txp.Pact (PactOp (..), GStateDb, newGStateDb)
import           Pos.DB.Txp.Stakes (StakesOp (..))
import           Pos.DB.Txp.Utxo (UtxoOp (..))
import           Pos.Util.AssertMode (inAssertMode)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (launchNamedPureLog)

----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: Genesis.Config -> TxpConfiguration -> TxpGlobalSettings
txpGlobalSettings genesisConfig txpConfig = TxpGlobalSettings
    { tgsVerifyBlocks   = verifyBlocks pm genesisConfig txpConfig
    , tgsApplyBlocks    = applyBlocksWith
        pm
        genesisConfig
        txpConfig
        (processBlundsSettings False $ applyToil bootStakeholders)
    , tgsRollbackBlocks = rollbackBlocks bootStakeholders
    }
  where
    pm               = configProtocolMagic genesisConfig
    bootStakeholders = configBootStakeholders genesisConfig

----------------------------------------------------------------------------
-- Verify
----------------------------------------------------------------------------

verifyBlocks ::
       forall ctx m. (TxpGlobalVerifyMode ctx m)
    => ProtocolMagic
    -> Genesis.Config
    -> TxpConfiguration
    -> Bool
    -> OldestFirst NE TxpBlock
    -> m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
verifyBlocks pm genesisConfig txpConfig verifyAllIsKnown newChain = runExceptT $ do
    gasModel <- defaultGasModel
    persister <- newGStateDb
    stateRoot <- getTip >>= unsafeGetStateRoot
    let baseEnv = GlobalToilEnv { _gteUtxoLookup = utxoToLookup M.empty
                                , _gteTotalStake = (mkCoin 0) -- | because we do not need stakes while verifying
                                , _gteStakeGetter = getRealStake
                                , _gteGasModel = gasModel
                                , _gtePersister = persister
                                }
    bvd <- gsAdoptedBVData
    let verifyPure :: TxValidationRules -> [TxAux] -> GlobalToilM GStateDb m (Either ToilVerFailure TxpUndo)
        verifyPure txValRules = runExceptT
            . verifyToil pm txValRules bvd (tcAssetLockedSrcAddrs txpConfig) epoch verifyAllIsKnown
        foldStep
            :: TxValidationRulesConfig
            -> (UtxoModifier, [TxpUndo], PactState)
            -> TxpBlock
            -> ExceptT ToilVerFailure m (UtxoModifier, [TxpUndo], PactState)
        foldStep tvrc (modifier, undos, pactState) (convertPayload -> txAuxes) = do
            currentEpoch <- epochOrSlotToEpochIndex . getEpochOrSlot <$> lift getTipHeader
            baseUtxo <- utxoToLookup <$> buildUtxo modifier txAuxes
            let txValRules = mkLiveTxValidationRules currentEpoch tvrc
            let gte = baseEnv { _gteUtxoLookup = baseUtxo }
            let gts = GlobalToilState { _gtsUtxoModifier = modifier
                                      , _gtsStakesView = def -- | because we do not need stakes while verifying
                                      , _gtsPactState = pactState
                                      }
            res <- lift $ runGlobalToilM gte gts (verifyPure txValRules txAuxes)
            case res of
                (Left err, _)         -> throwError err
                (Right txpUndo, gts') ->
                    return (gts' ^. gtsUtxoModifier, (txpUndo : undos), gts' ^. gtsPactState)
        -- 'NE.fromList' is safe here, because there will be at least
        -- one 'foldStep' (since 'newChain' is not empty) and it will
        -- either fail (and then 'convertRes' will not be called) or
        -- will prepend something to the result.
        convertRes :: (UtxoModifier, [TxpUndo], PactState) -> OldestFirst NE TxpUndo
        convertRes (_, undos, _) = OldestFirst . NE.fromList . reverse $ undos
    let txValRulesConfig = configTxValRules $ genesisConfig
    convertRes <$> foldM (foldStep txValRulesConfig) (mempty, mempty, defPactState stateRoot) newChain
  where
    epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _)      = []

----------------------------------------------------------------------------
-- General processing
----------------------------------------------------------------------------

data ProcessBlundsSettings extraEnv extraState m = ProcessBlundsSettings
    { pbsProcessSingle   :: TxpBlund -> m (ExtendedGlobalToilM extraEnv extraState GStateDb m ())
    , pbsCreateEnv       :: Utxo -> [TxAux] -> m extraEnv
    , pbsExtraOperations :: extraState -> SomeBatchOp
    , pbsIsRollback      :: !Bool
    -- ^ This flag specifies whether we want to rollback transactions
    -- or apply them. It affects the way we construct base 'Utxo'. If
    -- we want to apply transactions, we should use 'buildUtxo' to
    -- resolved all their inputs. But if we want to rollback them, we
    -- should turn known outputs of transactions into 'Utxo'.
    }

processBlunds ::
       forall extraEnv extraState ctx m. (TxpCommonMode ctx m, Default extraState)
    => ProcessBlundsSettings extraEnv extraState m
    -> NE TxpBlund
    -> m SomeBatchOp
processBlunds ProcessBlundsSettings {..} blunds = do
    let toBatchOp (gts, extra) =
            globalToilStateToBatch gts <> pbsExtraOperations extra
    totalStake <- getRealTotalStake -- doesn't change

    gasModel <- defaultGasModel
    persister <- newGStateDb
    stateRoot <- getTip >>= unsafeGetStateRoot
    -- Note: base utxo also doesn't change, but we build it on each
    -- step (for different sets of transactions), because
    -- 'UtxoModifier' may accumulate some data and it may be more
    -- efficient.

    -- Another note: if we rollback transactions, we don't really need
    -- base utxo, but we have a sanity check in 'utxoDel' which forces
    -- us to construct base utxo here.
    let buildBaseUtxo :: UtxoModifier -> [TxAux] -> m Utxo
        buildBaseUtxo
            | pbsIsRollback = buildUtxoForRollback
            | otherwise = buildUtxo

    let step ::
               (GlobalToilState, extraState)
            -> TxpBlund
            -> m (GlobalToilState, extraState)
        step gts txpBlund = do
            processSingle <- pbsProcessSingle txpBlund
            let txAuxesAndUndos = blundToAuxNUndo txpBlund
                txAuxes = fst <$> txAuxesAndUndos
            baseUtxo <- buildBaseUtxo (gts ^. _1 . gtsUtxoModifier) txAuxes
            extraEnv <- pbsCreateEnv baseUtxo txAuxes
            let gte = GlobalToilEnv
                        { _gteUtxoLookup = utxoToLookup baseUtxo
                        , _gteTotalStake = totalStake
                        , _gteStakeGetter = getRealStake
                        , _gteGasModel = gasModel
                        , _gtePersister = persister
                        }
            let env = (gte, extraEnv)
            launchNamedPureLog id $ 
                flip execStateT gts . usingReaderT env $
                processSingle
    toBatchOp <$> foldM step (defGlobalToilState stateRoot, def) blunds

----------------------------------------------------------------------------
-- Apply and rollback
----------------------------------------------------------------------------

applyBlocksWith ::
       forall extraEnv extraState ctx m.
       (TxpGlobalApplyMode ctx m, Default extraState)
    => ProtocolMagic
    -> Genesis.Config
    -> TxpConfiguration
    -> ProcessBlundsSettings extraEnv extraState m
    -> OldestFirst NE TxpBlund
    -> m SomeBatchOp
applyBlocksWith pm genesisConfig txpConfig settings blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- verifyBlocks pm genesisConfig txpConfig False blocks
        whenLeft verdict $
            assertionFailed .
            sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
    processBlunds settings (getOldestFirst blunds)

processBlundsSettings ::
       forall p m.(Monad m, p ~ GStateDb)
    => Bool
    -> ([(TxAux, TxUndo)] -> GlobalToilM p m ())
    -> ProcessBlundsSettings () () m
processBlundsSettings isRollback pureAction =
    ProcessBlundsSettings
        { pbsProcessSingle = \txpBlund -> pure (processSingle txpBlund)
        , pbsCreateEnv = \_ _ -> pure ()
        , pbsExtraOperations = const mempty
        , pbsIsRollback = isRollback
        }
  where
    processSingle :: TxpBlund -> ExtendedGlobalToilM () () GStateDb m ()
    processSingle = zoom _1 . magnify _1 . pureAction . blundToAuxNUndo

rollbackBlocks ::
       forall ctx m. (TxpGlobalRollbackMode ctx m)
    => GenesisWStakeholders
    -> NewestFirst NE TxpBlund
    -> m SomeBatchOp
rollbackBlocks bootStakeholders (NewestFirst blunds) = processBlunds
    (processBlundsSettings True (rollbackToil bootStakeholders))
    blunds

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GlobalToilState' to batch of database operations.
globalToilStateToBatch :: GlobalToilState -> SomeBatchOp
globalToilStateToBatch GlobalToilState {..} =
    SomeBatchOp [ SomeBatchOp utxoOps
                , SomeBatchOp stakesOps
                , SomeBatchOp pactOps
                ]
  where
    StakesView (HM.toList -> stakes) total = _gtsStakesView
    utxoOps =
        map DelTxIn (MM.deletions _gtsUtxoModifier) ++
        map (uncurry AddTxOut) (MM.insertions _gtsUtxoModifier)
    stakesOps = addTotalStakeOp $ map (uncurry PutFtsStake) stakes
    addTotalStakeOp =
        case total of
            Nothing -> identity
            Just x  -> (PutTotalStake x :)
    pactOps =
        map (uncurry PactOp) $ M.toList (_psModifier _gtsPactState)
    

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
