{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Chain.Txp.Toil.Utxo.Monad
       (
         -- * Monadic Utxo
         UtxoM
       , runUtxoM
       , evalUtxoM
       , execUtxoM
       , utxoGet
       , utxoPut
       , utxoDel

       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Universum hiding (id)

import           Control.Lens ((%=))
import qualified Data.List.NonEmpty as NE
import           Fmt ((+|), (|+))

import           Pos.Chain.Txp.Toil.Types (UtxoLookup, UtxoModifier)
import           Pos.Chain.Txp.Tx (Tx (..), TxIn (..))
import           Pos.Chain.Txp.TxAux (TxAux (..))
import           Pos.Chain.Txp.TxOutAux (TxOutAux (..))
import           Pos.Chain.Txp.Undo (TxUndo)
import           Pos.Crypto (WithHash (..), hash)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Run 'UtxoM' action using 'UtxoLookup' and 'UtxoModifier'.
runUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> (a, UtxoModifier)
runUtxoM modifier getter = usingState modifier . usingReaderT getter

-- | Version of 'runUtxoM' which discards final state.
evalUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> a
evalUtxoM = fst ... runUtxoM

-- | Version of 'runUtxoM' which discards action's result.
execUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> UtxoModifier
execUtxoM = snd ... runUtxoM

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- ask
    MM.lookup utxoLookup txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGet id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGet id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: WithHash Tx -> UtxoM ()
applyTxToUtxo (WithHash UnsafeTx {..} txid) = do
    mapM_ utxoDel $ toList _txInputs
    mapM_ applyOutput . zip [0 ..] . toList . map TxOutAux $ _txOutputs
  where
    applyOutput (idx, toa) = utxoPut (TxInUtxo txid idx) toa

-- | Rollback application of given transaction to Utxo using Undo
-- data.  This function assumes that transaction has been really
-- applied and doesn't check anything.
rollbackTxUtxo :: (TxAux, TxUndo) -> UtxoM ()
rollbackTxUtxo (txAux, undo) = do
    let tx@UnsafeTx {..} = taTx txAux
    let txid = hash tx
    mapM_ utxoDel $ take (length _txOutputs) $ map (TxInUtxo txid) [0..]
    mapM_ (uncurry utxoPut) . toList $ NE.zip _txInputs undo

