{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Client.Txp.IssueUtil
       ( createGDIssuanceTx
       ) where

import           Universum 

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import           Pos.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import           Pos.Chain.Txp (TxAux (..), TxIn (..),
                     TxOut (..), TxOutAux (..), Utxo, isStateTxOut)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Failure (TxError (..))
import           Pos.Client.Txp.Util (TxOwnedInputs, TxCreator, TxCreateMode, 
                     TxRaw (..), TxOutputs, PendingAddresses (..), 
                     InputSelectionPolicy (..), TxWithSpendings,
                     mkOutputsWithRem, makeMPubKeyTxAddrs, runTxCreator, prepareTxWithFee)
import           Pos.Core (Address, GoldDollar (..), addGoldDollar)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic, SafeSigner)

getCreator 
    :: ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> TxOwnedInputs TxOut
    -> [TxOutAux]
    -> Either TxError TxAux
getCreator pm hdwSigners = 
    makeMPubKeyTxAddrs pm getSigner
  where
    getSigner address =
        note (SafeSignerNotFound address) $
        hdwSigners address

createGDIssuanceTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> GoldDollar
    -> ByteString
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGDIssuanceTx genesisConfig pendingTx inputSelectionPolicy utxo hdwSigners issuedGDs proof addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOutsForIssurance genesisConfig pendingTx utxo issuedGDs proof addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)
  where
    pm = configProtocolMagic genesisConfig
    creator = getCreator pm hdwSigners

prepareInpsOutsForIssurance
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> GoldDollar
    -> ByteString
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, [TxOutAux])
prepareInpsOutsForIssurance genesisConfig pendingTx utxo issuedGDs proof addrData = do
    (fixedInps, fixedOuts) <- prepareStateInpsOutsForIssurance genesisConfig utxo issuedGDs proof addrData
    txRaw@TxRaw {..} <- prepareTxWithFee genesisConfig pendingTx utxo [] (NE.toList fixedInps) (NE.toList fixedOuts)
    outputsWithRem <- mkOutputsWithRem nm epochSlots addrData txRaw
    pure (trInputs, outputsWithRem)
  where
    nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    epochSlots = configEpochSlots genesisConfig

prepareStateInpsOutsForIssurance
    :: TxCreateMode m
    => Genesis.Config
    -> Utxo
    -> GoldDollar
    -> ByteString
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareStateInpsOutsForIssurance genesisConfig utxo issuedGDs proof addrData = do
    (stTxIn, stTxOut) <- 
        case (getTheOnlyTxOutState utxo) of 
            Just someVaule -> pure someVaule
            Nothing        -> throwError NotGDIssuer

    let oldTotalGDs = tosTotalGDs stTxOut
    newStTxOut <- TxOutAux <$> newStateOut oldTotalGDs
    gdOut      <- TxOutAux <$> newDollarOut

    let inps = (stTxOut, stTxIn) :| []
    let outs = newStTxOut :| [gdOut]
    return (inps, outs)
  where
    nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    epochSlots = configEpochSlots genesisConfig
    newStateOut oldTotalGDs = do 
        newTotalGDs <- case (addGoldDollar oldTotalGDs issuedGDs) of 
                           Nothing -> throwError $ GeneralTxError "The value of issued GDs exceeds the max value" 
                           Just c  -> return c
        rcvAddress <- lift . lift $ getNewAddress nm epochSlots addrData
        return $ TxOutState rcvAddress newTotalGDs proof

    newDollarOut = do
        rcvAddress <- lift . lift $ getNewAddress nm epochSlots addrData
        return $ TxOutGD rcvAddress issuedGDs

getTheOnlyTxOutState :: Utxo -> Maybe (TxIn, TxOut)
getTheOnlyTxOutState utxo = 
  case (M.toList $ M.filter (isStateTxOut . toaOut) utxo) of
      []                 -> Nothing
      [(txIn, txOutAux)] -> Just (txIn, toaOut txOutAux)
      _                  -> error "Found more than one TxOutState!"