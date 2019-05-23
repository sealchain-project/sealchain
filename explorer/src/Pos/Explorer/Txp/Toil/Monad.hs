{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Monads used for explorer's toil.

module Pos.Explorer.Txp.Toil.Monad
       (
         ExplorerExtraM

       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum

       , putTxExtra
       , delTxExtra
       , updateAddrHistory
       , putAddrBalance
       , delAddrBalance
       , putUtxoSum

       , ELocalToilM
       , explorerExtraMToELocalToilM

       , EGlobalToilM
       , explorerExtraMToEGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, zoom, (%=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)

import           Pos.Chain.Txp (ExtendedGlobalToilM, ExtendedLocalToilM,
                     TxId)
import           Pos.Core (Address, CoinPair)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtraLookup (..),
                     ExplorerExtraModifier, eemAddrBalances, eemAddrHistories,
                     eemLocalTxsExtra, eemNewUtxoSum)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (NamedPureLogger)

----------------------------------------------------------------------------
-- Monadic actions with extra txp data.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup extra values related to txp and modify them.
type ExplorerExtraM m
     = ReaderT ExplorerExtraLookup (StateT ExplorerExtraModifier (NamedPureLogger m))

getTxExtra :: Monad m => TxId -> ExplorerExtraM m (Maybe TxExtra)
getTxExtra txId = do
    baseLookup <- eelGetTxExtra <$> ask
    MM.lookup baseLookup txId <$> use eemLocalTxsExtra

getAddrHistory :: Monad m => Address -> ExplorerExtraM m AddrHistory
getAddrHistory addr = do
    use (eemAddrHistories . at addr) >>= \case
        Nothing -> eelGetAddrHistory <$> ask <*> pure addr
        Just hist -> pure hist

getAddrBalance :: Monad m => Address -> ExplorerExtraM m (Maybe CoinPair)
getAddrBalance addr = do
    baseLookup <- eelGetAddrBalance <$> ask
    MM.lookup baseLookup addr <$> use eemAddrBalances

getUtxoSum :: Monad m => ExplorerExtraM m (Integer, Integer)
getUtxoSum = fromMaybe <$> (eelGetUtxoSum <$> ask) <*> use eemNewUtxoSum

putTxExtra :: Monad m => TxId -> TxExtra -> ExplorerExtraM m ()
putTxExtra txId extra = eemLocalTxsExtra %= MM.insert txId extra

delTxExtra :: Monad m => TxId -> ExplorerExtraM m ()
delTxExtra txId = eemLocalTxsExtra %= MM.delete txId

updateAddrHistory :: Monad m => Address -> AddrHistory -> ExplorerExtraM m ()
updateAddrHistory addr hist = eemAddrHistories . at addr .= Just hist

putAddrBalance :: Monad m => Address -> CoinPair -> ExplorerExtraM m ()
putAddrBalance addr coin = eemAddrBalances %= MM.insert addr coin

delAddrBalance :: Monad m => Address -> ExplorerExtraM m ()
delAddrBalance addr = eemAddrBalances %= MM.delete addr

putUtxoSum :: Monad m => (Integer, Integer) -> ExplorerExtraM m ()
putUtxoSum utxoSum = eemNewUtxoSum .= Just utxoSum

----------------------------------------------------------------------------
-- Monad used for local Toil in Explorer.
----------------------------------------------------------------------------

type ELocalToilM m = ExtendedLocalToilM ExplorerExtraLookup ExplorerExtraModifier m

explorerExtraMToELocalToilM :: Monad m => ExplorerExtraM m ~> ELocalToilM m
explorerExtraMToELocalToilM = zoom _2 . magnify _2

----------------------------------------------------------------------------
-- Monad used for global Toil in Explorer.
----------------------------------------------------------------------------

type EGlobalToilM m
     = ExtendedGlobalToilM ExplorerExtraLookup ExplorerExtraModifier m

explorerExtraMToEGlobalToilM :: Monad m => ExplorerExtraM m ~> EGlobalToilM m 
explorerExtraMToEGlobalToilM = mapReaderT (mapStateT id . zoom _2) . magnify _2
