{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

-- | Some monads used in Toil and primitive actions.

module Pos.Chain.Txp.Toil.Monad
       ( VerifyAndApplyM
       , VerifyAndApplyEnv (..)
       , VerifyAndApplyState (..)
       , vaaeUtxo
       , vaasUtxoModifier
       , utxoGet
       , utxoPut
       , utxoDel
         -- * Monadic local Toil
       , LocalToilEnv (..)
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo
       , ExtendedLocalToilM
       , extendLocalToilM

         -- * Monadic global Toil
       , GlobalToilState (..)
       , gtsUtxoModifier
       , gtsStakesView
       , defGlobalToilState
       , GlobalToilEnv (..)
       , GlobalToilM
       , gteUtxo
       , gteTotalStake
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake
       , ExtendedGlobalToilM
       , extendGlobalToilM

        -- Convertions
       , verifyAndApplyMToLocalToilM
       , verifyAndApplyMToGlobalToilM

        -- * Pact execution
       , PactExecEnv (..)
       , PactExecState (..)
       , PactExecM
       , peeGasModel
       , pesRefStore
       , pesMPTreeDB
       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))

import           Pact.Persist.MPTree (MPTreeDB)
import           Pact.Types.Gas (GasModel)
import           Pact.Types.Runtime (RefStore)

import           Pos.Chain.Txp.Toil.Types (MemPool, StakesView, UndoMap,
                     UtxoLookup, UtxoModifier, mpLocalTxs, mpSize, svStakes,
                     svTotal)
import           Pos.Chain.Txp.Tx (TxId, TxIn)
import           Pos.Chain.Txp.TxAux (TxAux)
import           Pos.Chain.Txp.TxOutAux (TxOutAux)
import           Pos.Chain.Txp.Undo (TxUndo)
import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

----------------------------------------------------------------------------
-- Monad used for verify and apply tx.
----------------------------------------------------------------------------

data VerifyAndApplyState = VerifyAndApplyState
    { _vaasUtxoModifier :: !UtxoModifier
    }

makeLenses ''VerifyAndApplyState

data VerifyAndApplyEnv = VerifyAndApplyEnv
    { _vaaeUtxo     :: !UtxoLookup
    }

makeLenses ''VerifyAndApplyEnv

type VerifyAndApplyM m
     = ReaderT VerifyAndApplyEnv (StateT VerifyAndApplyState m)

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: (Monad m) => TxIn -> (VerifyAndApplyM m) (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- view vaaeUtxo
    MM.lookup utxoLookup txIn <$> use vaasUtxoModifier

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: (Monad m) => TxIn -> TxOutAux -> (VerifyAndApplyM m) ()
utxoPut txId txOut = utxoGet txId >>= \case
    Nothing -> vaasUtxoModifier %= MM.insert txId txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|txId|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: (Monad m) => TxIn -> (VerifyAndApplyM m) ()
utxoDel txId = utxoGet txId >>= \case
    Just _  -> vaasUtxoModifier %= MM.delete txId
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|txId|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in local Toil.
data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

data LocalToilEnv = LocalToilEnv
    { _lteUtxo       :: !UtxoLookup
    }

makeLenses ''LocalToilEnv

-- | Monad in which local Toil happens.
type LocalToilM m = ReaderT LocalToilEnv (StateT LocalToilState m)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: Monad m => TxId -> LocalToilM m Bool
hasTx tid = isJust <$> use (ltsMemPool . mpLocalTxs . at tid)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: Monad m => TxId -> TxAux -> TxUndo -> LocalToilM m ()
putTxWithUndo tid tx undo =
    unlessM (hasTx tid) $ do
        ltsMemPool . mpLocalTxs . at tid .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at tid .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: Monad m => LocalToilM m Int
memPoolSize = use $ ltsMemPool . mpSize

-- | Extended version of 'LocalToilM'. It allows to put extra data
-- into reader context, extra state and also adds logging
-- capabilities. It's needed for explorer which has more complicated
-- transaction processing.
type ExtendedLocalToilM extraEnv extraState m =
    ReaderT (LocalToilEnv, extraEnv) (
        StateT (LocalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'LocalToilM to 'ExtendedLocalToilM'.
extendLocalToilM :: Monad m => LocalToilM m a -> ExtendedLocalToilM extraEnv extraState m a
extendLocalToilM = mapReaderT (mapStateT lift . zoom _1) . magnify _1

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in global Toil.
data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: GlobalToilState
defGlobalToilState =
    GlobalToilState 
    { _gtsUtxoModifier = mempty
    , _gtsStakesView = def
    }

makeLenses ''GlobalToilState

-- | Immutable environment used in global Toil.
data GlobalToilEnv m = GlobalToilEnv
    { _gteUtxo        :: !UtxoLookup
    , _gteTotalStake  :: !Coin
    , _gteStakeGetter :: (StakeholderId -> m (Maybe Coin)) 
    }

makeLenses ''GlobalToilEnv

-- | Monad in which global Toil happens.
type GlobalToilM m
     = ReaderT (GlobalToilEnv m) (StateT GlobalToilState (NamedPureLogger m))

runGlobalToilM 
    :: forall m a. (WithLogger m)
    => GlobalToilEnv m
    -> GlobalToilState
    -> GlobalToilM m a
    -> m (a, GlobalToilState)
runGlobalToilM env gts =
    launchNamedPureLog id . usingStateT gts . usingReaderT env

-- | Get stake of a given stakeholder.
getStake :: Monad m => StakeholderId -> GlobalToilM m (Maybe Coin)
getStake shId = do
    stakeGetter <- view gteStakeGetter
    (<|>) <$> use (gtsStakesView . svStakes . at shId) <*> (lift . lift . lift $ (stakeGetter shId))

-- | Get total stake of all stakeholders.
getTotalStake :: Monad m => GlobalToilM m Coin
getTotalStake =
    maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

-- | Set stake of a given stakeholder.
setStake :: Monad m => StakeholderId -> Coin -> GlobalToilM m ()
setStake shId c = gtsStakesView . svStakes . at shId .= Just c

-- | Set total stake of all stakeholders.
setTotalStake :: Monad m => Coin -> GlobalToilM m ()
setTotalStake c = gtsStakesView . svTotal .= Just c

-- | Extended version of 'GlobalToilM'. It allows to put extra data
-- into reader context and extra state. It's needed for explorer which
-- has more complicated transaction processing.
type ExtendedGlobalToilM extraEnv extraState m =
    ReaderT (GlobalToilEnv m, extraEnv) (
        StateT (GlobalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'GlobalToilM to 'ExtendedGlobalToilM'.
extendGlobalToilM :: Monad m => GlobalToilM m ~> ExtendedGlobalToilM extraEnv extraState m
extendGlobalToilM = zoom _1 . magnify _1


----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

verifyAndApplyMToLocalToilM :: forall a m.Monad m => VerifyAndApplyM m a -> LocalToilM m a
verifyAndApplyMToLocalToilM action = do
    utxoModifier <- use ltsUtxoModifier
    utxoLookup <- view lteUtxo

    (res, vaas) <- 
        lift . lift $
        usingStateT (VerifyAndApplyState utxoModifier) $
        usingReaderT (VerifyAndApplyEnv utxoLookup) $
        action

    ltsUtxoModifier .= _vaasUtxoModifier vaas
    return res

verifyAndApplyMToGlobalToilM :: forall a m.Monad m => VerifyAndApplyM m a -> GlobalToilM m a
verifyAndApplyMToGlobalToilM action = do
    utxoModifier <- use gtsUtxoModifier
    utxoLookup <- view gteUtxo

    (res, vaas) <- 
        lift . lift . lift $
        usingStateT (VerifyAndApplyState utxoModifier) $
        usingReaderT (VerifyAndApplyEnv utxoLookup) $
        action

    gtsUtxoModifier .= _vaasUtxoModifier vaas
    return res

----------------------------------------------------------------------------
-- Monadic actions with Pact.
----------------------------------------------------------------------------

-- | Immutable environment used in Pact execution.
data PactExecEnv = PactExecEnv
    { _peeGasModel :: !GasModel
    }
makeLenses ''PactExecEnv

-- | Mutable state used in Pact execution.
data PactExecState p = PactExecState
    { _pesRefStore :: !RefStore
    , _pesMPTreeDB :: !(MPTreeDB p)
    }

makeLenses ''PactExecState

-- | Utility monad which allows to run Pact commands.
type PactExecM p m
     = ReaderT PactExecEnv (StateT (PactExecState p) m)