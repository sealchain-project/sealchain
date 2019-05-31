{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | All high-level logic of Toil.  It operates in 'LocalToilM' and
-- 'GlobalToilM'.

module Pos.Chain.Txp.Toil.Logic
       ( verifyToil
       , applyToil
       , rollbackToil

       , normalizeToil
       , processTx
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (ExceptT, mapExceptT, throwError)
import qualified Data.Set as Set
import           Serokell.Data.Memory.Units (Byte)

import qualified Pact.Types.Gas as Pact (Gas (..), GasLimit (..))
import qualified Pact.Types.Term as Pact (PublicKey (..))
import qualified Pact.Types.Util as Pact (Hash (..))

import           Pos.Binary.Class (biSize, serialize')
import           Pos.Chain.Genesis (GenesisWStakeholders)
import           Pos.Chain.Txp.Command (CommandResult (..))
import           Pos.Chain.Txp.Configuration (TxpConfiguration (..),
                     memPoolLimitTx)
import           Pos.Chain.Txp.Toil.Failure (ToilVerFailure (..))
import           Pos.Chain.Txp.Toil.Monad (GlobalToilM, LocalToilM, 
                     UtxoM, VerifyAndApplyM, PactExecM,
                     hasTx, memPoolSize, putTxWithUndo,
                     utxoMToVerifyAndApplyM, utxoMToGlobalToilM,
                     verifyAndApplyMToLocalToilM, verifyAndApplyMToGlobalToilM,
                     pactExecMToVerifyAndApplyM, pactExecMToGlobalToilM)
import qualified Pos.Chain.Txp.Toil.Pact as Pact
import           Pos.Chain.Txp.Toil.Stakes (applyTxsToStakes, rollbackTxsStakes)
import           Pos.Chain.Txp.Toil.Types (TxFee (..))
import           Pos.Chain.Txp.Toil.Utxo (VerifyTxUtxoRes (..))
import qualified Pos.Chain.Txp.Toil.Utxo as Utxo
import           Pos.Chain.Txp.Topsort (topsortTxs)
import           Pos.Chain.Txp.Tx (Tx (..), TxId, TxOut (..), TxValidationRules,
                     txOutAddress)
import           Pos.Chain.Txp.TxAux (TxAux (..), checkTxAux)
import           Pos.Chain.Txp.TxOutAux (toaOut)
import           Pos.Chain.Txp.TxWitness (CommandWitness (..))
import           Pos.Chain.Txp.Undo (TxUndo, TxpUndo)
import           Pos.Chain.Update.BlockVersionData (BlockVersionData (..),
                     isBootstrapEraBVD)
import           Pos.Core (AddrAttributes (..), AddrStakeDistribution (..),
                     Address, EpochIndex, Coin (..), addrAttributesUnwrapped,
                     isRedeemAddress, unsafeIntegerToCoin, mkCoin)
import           Pos.Core.Common (integerToCoin)
import qualified Pos.Core.Common as Fee (TxFeePolicy (..),
                     calculateTxSizeLinear)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic, WithHash (..), hash)
import           Pos.Util (liftEither)

import           Sealchain.Mpt.MerklePatriciaMixMem (KVPersister)

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually.
--
-- If the 'Bool' argument is 'True', all data (script versions,
-- witnesses, addresses, attributes) must be known. Otherwise unknown
-- data is just ignored.
verifyToil
    :: (MonadIO m, KVPersister p)
    => ProtocolMagic
    -> TxValidationRules
    -> BlockVersionData
    -> Set Address
    -> EpochIndex
    -> Bool
    -> [TxAux]
    -> ExceptT ToilVerFailure (GlobalToilM p m) TxpUndo
verifyToil pm txValRules bvd lockedAssets curEpoch verifyAllIsKnown =
    mapM verifyTx
  where 
    verifyTx tx = 
        mapExceptT verifyAndApplyMToGlobalToilM $
            verifyAndApplyTx pm txValRules bvd lockedAssets curEpoch verifyAllIsKnown $ withTxId tx

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil 
    :: (MonadIO m, KVPersister p)
    => GenesisWStakeholders 
    -> [(TxAux, TxUndo)] 
    -> GlobalToilM p m ()
applyToil _ [] = pass
applyToil bootStakeholders txun = do
    applyTxsToStakes bootStakeholders txun
    mapM_ applyTx txun
  where
    applyTx (txAux@TxAux{..}, _) = do
        let txId = hash taTx
        _ <- utxoMToGlobalToilM $ Utxo.applyTxToUtxo $ WithHash taTx txId
        pactExecMToGlobalToilM . runExceptT $ applyTxToPact (txId, txAux) (mkCoin 0)

-- | Rollback transactions from one block.
rollbackToil :: Monad m => GenesisWStakeholders -> [(TxAux, TxUndo)] -> GlobalToilM p m ()
rollbackToil bootStakeholders txun = do
    rollbackTxsStakes bootStakeholders txun
    utxoMToGlobalToilM $
        mapM_ Utxo.rollbackTxUtxo $ reverse txun
    -- only rollback utxo

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: (MonadIO m, KVPersister p)
    => ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure (LocalToilM p m) TxUndo
processTx pm txValRules txpConfig bvd curEpoch tx@(id, aux) = do
    whenM (lift $ hasTx id) $ throwError ToilKnown
    whenM ((>= memPoolLimitTx txpConfig) <$> lift memPoolSize) $
        throwError (ToilOverwhelmed $ memPoolLimitTx txpConfig)
    undo <- mapExceptT verifyAndApplyMToLocalToilM $ 
            verifyAndApplyTx pm txValRules bvd (tcAssetLockedSrcAddrs txpConfig) curEpoch True tx
    undo <$ lift (putTxWithUndo id aux undo)

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: forall p m.(MonadIO m, KVPersister p)
    => ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> [(TxId, TxAux)]
    -> LocalToilM p m ()
normalizeToil pm txValRules txpConfig bvd curEpoch txs = mapM_ normalize ordered
  where
    -- If there is a cycle in the tx list, topsortTxs returns Nothing.
    -- Why is that not an error? And if its not an error, why bother
    -- top-sorting them anyway?
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, txAux) = WithHash (taTx txAux) i
    normalize ::
           (TxId, TxAux)
        -> LocalToilM p m ()
    normalize = void . runExceptT . processTx pm txValRules txpConfig bvd curEpoch

----------------------------------------------------------------------------
-- Verify and Apply logic
----------------------------------------------------------------------------

-- Note: it doesn't consider/affect stakes! That's because we don't
-- care about stakes for local txp.
verifyAndApplyTx
    :: (MonadIO m, KVPersister p)
    => ProtocolMagic
    -> TxValidationRules
    -> BlockVersionData
    -> Set Address
    -> EpochIndex
    -> Bool
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure (VerifyAndApplyM p m) TxUndo
verifyAndApplyTx pm txValRules adoptedBVD lockedAssets curEpoch verifyVersions tx@(_, txAux) = do
    whenLeft (checkTxAux txValRules txAux) (throwError . ToilInconsistentTxAux)
    let ctx = Utxo.VTxContext verifyVersions (makeNetworkMagic pm)
    vtur@VerifyTxUtxoRes {..} <- mapExceptT utxoMToVerifyAndApplyM $ 
                                 Utxo.verifyTxUtxo pm ctx lockedAssets txAux
    liftEither $ verifyGState adoptedBVD curEpoch txAux vtur
    lift $ utxoMToVerifyAndApplyM (applyTxToUtxo' tx)
    _ <- mapExceptT pactExecMToVerifyAndApplyM $ 
         applyTxToPact tx (mkCoin 0)
    pure vturUndo

isRedeemTx :: TxUndo -> Bool
isRedeemTx resolvedOuts = all isRedeemAddress inputAddresses
  where
    inputAddresses =
        fmap (txOutAddress . toaOut) . toList $ resolvedOuts

verifyGState ::
       BlockVersionData
    -> EpochIndex
    -> TxAux
    -> VerifyTxUtxoRes
    -> Either ToilVerFailure ()
verifyGState bvd@BlockVersionData {..} curEpoch txAux vtur = do
    verifyBootEra bvd curEpoch txAux
    let txFee = vturFee vtur
    let txSize = biSize txAux
    let limit = bvdMaxTxSize
    unless (isRedeemTx $ vturUndo vtur) $
        verifyTxFeePolicy txFee bvdTxFeePolicy txSize
    when (txSize > limit) $
        throwError $ ToilTooLargeTx txSize limit

verifyBootEra ::
       BlockVersionData -> EpochIndex -> TxAux -> Either ToilVerFailure ()
verifyBootEra bvd curEpoch TxAux {..} = do
    when (isBootstrapEraBVD bvd curEpoch) $
        whenNotNull notBootstrapDistrAddresses $
        throwError . ToilNonBootstrapDistr
  where
    notBootstrapDistrAddresses :: [Address]
    notBootstrapDistrAddresses =
        filter (not . isBootstrapEraDistr) $
        map txOutAddress $ toList $ _txOutputs taTx
    isBootstrapEraDistr :: Address -> Bool
    isBootstrapEraDistr (addrAttributesUnwrapped -> AddrAttributes {..}) =
        case aaStakeDistribution of
            BootstrapEraDistr -> True
            _                 -> False

verifyTxFeePolicy ::
       TxFee -> Fee.TxFeePolicy -> Byte -> Either ToilVerFailure ()
verifyTxFeePolicy (TxFee txFee) policy txSize = case policy of
    Fee.TxFeePolicyTxSizeLinear txSizeLinear -> do
        let
            -- We use 'ceiling' to convert from a fixed-precision fractional
            -- to coin amount. The actual fee is always a non-negative integer
            -- amount of coins, so if @min_fee <= fee@ holds (the ideal check),
            -- then @ceiling min_fee <= fee@ holds too.
            -- The reason we can't compare fractionals directly is that the
            -- minimal fee may need to appear in an error message (as a reason
            -- for rejecting the transaction).
            mTxMinFee = integerToCoin . ceiling $
                Fee.calculateTxSizeLinear txSizeLinear txSize
        -- The policy must be designed in a way that makes this impossible,
        -- but in case the result of its evaluation is negative or exceeds
        -- maximum coin value, we throw an error.
        txMinFee <- case mTxMinFee of
            Left reason -> throwError $
                ToilInvalidMinFee policy reason txSize
            Right a -> return a
        unless (txMinFee <= txFee) $
            throwError $
                ToilInsufficientFee policy (TxFee txFee) (TxFee txMinFee) txSize
    Fee.TxFeePolicyUnknown _ _ ->
        -- The minimal transaction fee policy exists, but the current
        -- version of the node doesn't know how to handle it. There are
        -- three possible options mentioned in [CSLREQ-157]:
        -- 1. Reject all new-coming transactions (b/c we can't calculate
        --    fee for them)
        -- 2. Use latest policy of known type
        -- 3. Discard the check
        -- Implementation-wise, the 1st option corresponds to throwing an
        -- error here (reject), the 3rd option -- doing nothing (accept), and
        -- the 2nd option would require some engineering feats to
        -- retrieve previous 'TxFeePolicy' and check against it.
        return ()

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux = (hash (taTx aux), aux)

applyTxToUtxo' :: (TxId, TxAux) -> UtxoM ()
applyTxToUtxo' (i, TxAux tx _) = Utxo.applyTxToUtxo (WithHash tx i)

applyTxToPact 
    :: (MonadIO m, KVPersister p)
    => (TxId, TxAux) 
    -> Coin 
    -> ExceptT ToilVerFailure (PactExecM p m) Coin
applyTxToPact (txId, TxAux tx wits) (Coin gasLimit) = do
    CommandResult{..} <- Pact.applyCmd (_txCommand tx) pactGasLimit pactHash pactSigners 
    let (Pact.Gas int64Gas) = _crGas
    return . unsafeIntegerToCoin . toInteger $ int64Gas
  where
    toPactPublicKey (CommandWitness pubKey _) = Pact.PublicKey . serialize' $ pubKey
    pactSigners = Set.fromList . map toPactPublicKey . toList . snd $ wits
    pactHash = Pact.Hash $ serialize' txId
    pactGasLimit = Pact.GasLimit gasLimit
