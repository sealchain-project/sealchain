{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Pos.DB.Txp.MemState.Holder
       ( GenericTxpLocalData
       , mkTxpLocalData
       ) where

import           Universum
import qualified Universum.Unsafe as Unsafe

import           Data.Default (Default (def))

import           Pos.Chain.Txp (PactState, defPactState)
import           Pos.Chain.Block (HeaderHash, StateRoot (..), HasStateRoot (..))

import           Pos.DB.BlockIndex (getHeader)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Common (getTip)
import           Pos.DB.Rocks (MonadRealDB)
import           Pos.DB.Txp.MemState.Types (GenericTxpLocalData (..))

import qualified Sealchain.Mpt.MerklePatriciaMixMem as Mpt (StateRoot (..))
----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

mkTxpLocalData
    :: (Default e, MonadIO m, MonadRealDB ctx m, MonadDBRead m)
    => m (GenericTxpLocalData e)
mkTxpLocalData = do
    initTip <- getTip
    pactState <- unsafeInitPactState initTip
    TxpLocalData <$> newTVarIO mempty <*> newTVarIO def <*> newTVarIO mempty <*>
        newTVarIO initTip <*>
        newTVarIO pactState <*>
        newTVarIO def

unsafeInitPactState :: (MonadRealDB ctx m, MonadDBRead m) => HeaderHash -> m PactState
unsafeInitPactState tip = do
    (StateRoot bs) <- getStateRoot . Unsafe.fromJust <$> getHeader tip
    return $ defPactState (Mpt.StateRoot bs)
