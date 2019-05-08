{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Explorer's version of Toil logic.

module Pos.Explorer.Txp.Toil.Logic
       ( eApplyToil
       , eRollbackToil
       , eNormalizeToil
       , eProcessTx
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (mapExceptT)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (delete)
import           Formatting (build, sformat, (%))

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Genesis (GenesisWStakeholders)
import           Pos.Chain.Txp (ToilVerFailure (..), Tx (..), TxAux (..), TxId,
                     TxOut (..), TxOutAux (..), TxUndo, TxValidationRules,
                     TxpConfiguration, extendGlobalToilM, extendLocalToilM,
                     topsortTxs, isOriginTxOut, isGDTxOut)
import qualified Pos.Chain.Txp as Txp
import           Pos.Chain.Update (BlockVersionData)
import           Pos.Core (Address, CoinPair, EpochIndex, Timestamp, 
                     mkCoin, sumCoins, mkGoldDollar, sumGoldDollars,
                     unsafeAddCoinPair, unsafeSubCoinPair)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (ProtocolMagic, WithHash (..), hash)
import           Pos.Explorer.Core (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Monad (EGlobalToilM, ELocalToilM,
                     ExplorerExtraM, delAddrBalance, delTxExtra,
                     explorerExtraMToEGlobalToilM, explorerExtraMToELocalToilM,
                     getAddrBalance, getAddrHistory, getTxExtra, getUtxoSum,
                     putAddrBalance, putTxExtra, putUtxoSum, updateAddrHistory)
import           Pos.Util.Util (Sign (..))
import           Pos.Util.Wlog (logError)

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil
    :: GenesisWStakeholders
    -> Maybe Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> EGlobalToilM ()
eApplyToil bootStakeholders mTxTimestamp txun hh = do
    extendGlobalToilM $ Txp.applyToil bootStakeholders txun
    explorerExtraMToEGlobalToilM $ mapM_ applier $ zip [0..] txun
  where
    applier :: (Word32, (TxAux, TxUndo)) -> ExplorerExtraM ()
    applier (i, (txAux, txUndo)) = do
        let tx = taTx txAux
            id = hash tx
            newExtra = TxExtra (Just (hh, i)) mTxTimestamp txUndo
        extra <- fromMaybe newExtra <$> getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txAux txUndo
        let balanceUpdate = getBalanceUpdate txAux txUndo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Rollback transactions from one block.
eRollbackToil :: GenesisWStakeholders -> [(TxAux, TxUndo)] -> EGlobalToilM ()
eRollbackToil bootStakeholders txun = do
    extendGlobalToilM $ Txp.rollbackToil bootStakeholders txun
    explorerExtraMToEGlobalToilM $ mapM_ extraRollback $ reverse txun
  where
    extraRollback :: (TxAux, TxUndo) -> ExplorerExtraM ()
    extraRollback (txAux, txUndo) = do
        delTxExtraWithHistory (hash (taTx txAux)) $
          getTxRelatedAddrs txAux txUndo
        let BalanceUpdate {..} = getBalanceUpdate txAux txUndo
        let balanceUpdate = BalanceUpdate {
            plusBalance = minusBalance,
            minusBalance = plusBalance
        }
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx
    :: ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> (TxUndo -> TxExtra)
    -> ExceptT ToilVerFailure ELocalToilM ()
eProcessTx pm txValRules txpConfig bvd curEpoch tx@(id, aux) createExtra = do
    undo <- mapExceptT extendLocalToilM $ Txp.processTx pm txValRules txpConfig bvd curEpoch tx
    lift $ explorerExtraMToELocalToilM $ do
        let extra = createExtra undo
        putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
        let balanceUpdate = getBalanceUpdate aux undo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil
    :: ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> [(TxId, (TxAux, TxExtra))]
    -> ELocalToilM ()
eNormalizeToil pm txValRules txpConfig bvd curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry (eProcessTx pm txValRules txpConfig bvd curEpoch) . repair
    repair (i, (txAux, extra)) = ((i, txAux), const extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data BalanceUpdate = BalanceUpdate
    { minusBalance :: [(Address, CoinPair)]
    , plusBalance  :: [(Address, CoinPair)]
    }

modifyAddrHistory :: (AddrHistory -> AddrHistory) -> Address -> ExplorerExtraM ()
modifyAddrHistory f addr = updateAddrHistory addr . f =<< getAddrHistory addr

putTxExtraWithHistory :: TxId -> TxExtra -> [Address] -> ExplorerExtraM ()
putTxExtraWithHistory id extra addrs = do
    putTxExtra id extra
    for_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory :: TxId -> [Address] -> ExplorerExtraM ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    for_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

updateUtxoSumFromBalanceUpdate :: BalanceUpdate -> ExplorerExtraM ()
updateUtxoSumFromBalanceUpdate balanceUpdate = do
    let plusCoin  = sumCoins $ map (fst . snd) $ plusBalance  balanceUpdate
        minusCoin = sumCoins $ map (fst . snd) $ minusBalance balanceUpdate
        coinChange  = plusCoin - minusCoin
        plusGD  = sumGoldDollars $ map (snd . snd) $ plusBalance  balanceUpdate
        minusGD = sumGoldDollars $ map (snd . snd) $ minusBalance balanceUpdate
        gdChange  = plusGD - minusGD
    (coinSum, gdSum) <- getUtxoSum
    putUtxoSum $ (coinSum + coinChange, gdSum + gdChange)

getTxRelatedAddrs :: TxAux -> TxUndo -> [Address]
getTxRelatedAddrs TxAux {taTx = UnsafeTx {..}} (toList -> undo) =
    map txOutAddress _txOutputs `unionList` map (txOutAddress . toaOut) undo
  where
    unionList :: [Address] -> [Address] -> [Address]
    unionList lhs rhs = HS.toList $ HS.union (HS.fromList lhs) (HS.fromList rhs)

combineBalanceUpdates :: BalanceUpdate -> [(Address, (Sign, CoinPair))]
combineBalanceUpdates BalanceUpdate {..} =
    let plusCombined  = map (\(addr, bal) -> (addr, (Plus, bal))) $ 
                        HM.toList $ 
                        HM.fromListWith unsafeAddCoinPair plusBalance
        minusCombined = map (\(addr, bal) -> (addr, (Minus, bal))) $ 
                        HM.toList $ 
                        HM.fromListWith unsafeAddCoinPair minusBalance
    in plusCombined <> minusCombined

updateAddrBalances :: BalanceUpdate -> ExplorerExtraM ()
updateAddrBalances (combineBalanceUpdates -> updates) = mapM_ updater updates
  where
    updater :: (Address, (Sign, CoinPair)) -> ExplorerExtraM ()
    updater (addr, (Plus, plus)) = do
        currentBalance <- fromMaybe (mkCoin 0, mkGoldDollar 0) <$> getAddrBalance addr
        let newBalance = unsafeAddCoinPair currentBalance plus
        putAddrBalance addr newBalance
    updater (addr, (Minus, minus)) = do
        maybeBalance <- getAddrBalance addr
        case maybeBalance of
            Nothing ->
                logError $
                    sformat ("updateAddrBalances: attempted to subtract "%build%
                             " from unknown address "%build)
                    minus addr
            Just currentBalance
                | littleThanBalance currentBalance minus ->
                    logError $
                        sformat ("updateAddrBalances: attempted to subtract "%build%
                                 " from address "%build%" which only has "%build)
                        minus addr currentBalance
                | otherwise -> do
                    let newBalance = unsafeSubCoinPair currentBalance minus
                    if newBalance == (mkCoin 0, mkGoldDollar 0) then
                        delAddrBalance addr
                    else
                        putAddrBalance addr newBalance

getBalanceUpdate :: TxAux -> TxUndo -> BalanceUpdate
getBalanceUpdate txAux txUndo =
    let undoOutputs = toList $ map toaOut txUndo
        txOutputs = _txOutputs (taTx txAux)

        filterUndoOutputs f = filter f undoOutputs
        filterTxOutputs f = filter f txOutputs

        minusCoinBalance = map (\txOut -> (txOutAddress txOut, (txOutValue txOut, mkGoldDollar 0))) $ filterUndoOutputs isOriginTxOut
        plusCoinBalance = map (\txOut -> (txOutAddress txOut, (txOutValue txOut, mkGoldDollar 0))) $ filterTxOutputs isOriginTxOut
        minusGDBalance = map (\txOut -> (txOutAddress txOut, (mkCoin 0, txOutGD txOut))) $ filterUndoOutputs isGDTxOut
        plusGDBalance = map (\txOut -> (txOutAddress txOut, (mkCoin 0, txOutGD txOut))) $ filterTxOutputs isGDTxOut

        minusBalance = minusCoinBalance <> minusGDBalance
        plusBalance = plusCoinBalance <> plusGDBalance

    in BalanceUpdate {..}

littleThanBalance :: CoinPair -> CoinPair -> Bool
littleThanBalance (c1, gd1) (c2, gd2) = c1 < c2 || gd1 < gd2