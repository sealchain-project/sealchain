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
       , lteUtxo
       , lteGasModel
       , ltePactMPDB
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , ltsPactState
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
       , gtsPactState
       , defGlobalToilState
       , GlobalToilEnv (..)
       , GlobalToilM
       , gteUtxo
       , gteTotalStake
       , gteStakeGetter
       , gteGasModel
       , gtePactMPDB
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
       , peeLoggers
       , peeGasEnv
       , pesRefStore
       , pesMPTreeDB
       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))

import qualified Pact.Persist.MPTree as Pact (MPTreeDB)
import qualified Pact.Types.Gas as Pact (GasModel, GasEnv)
import qualified Pact.Types.Logger as Pact (Loggers)
import qualified Pact.Types.Runtime as Pact (RefStore)

import           Pos.Chain.Txp.Toil.Types (MemPool, StakesView, UndoMap,
                     UtxoLookup, UtxoModifier, PactState, 
                     mpLocalTxs, mpSize, svStakes, svTotal)
import           Pos.Chain.Txp.Tx (TxId, TxIn)
import           Pos.Chain.Txp.TxAux (TxAux)
import           Pos.Chain.Txp.TxOutAux (TxOutAux)
import           Pos.Chain.Txp.Undo (TxUndo)
import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Sealchain.Mpt.MerklePatriciaMixMem (MPDB)

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
    , _ltsPactState    :: !PactState
    }

makeLenses ''LocalToilState

data LocalToilEnv p = LocalToilEnv
    { _lteUtxo       :: !UtxoLookup
    , _lteGasModel   :: !Pact.GasModel
    , _ltePactMPDB   :: !(MPDB p)
    }

makeLenses ''LocalToilEnv

-- | Monad in which local Toil happens.
type LocalToilM p m = ReaderT (LocalToilEnv p) (StateT LocalToilState m)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: Monad m => TxId -> LocalToilM p m Bool
hasTx tid = isJust <$> use (ltsMemPool . mpLocalTxs . at tid)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: Monad m => TxId -> TxAux -> TxUndo -> LocalToilM p m ()
putTxWithUndo tid tx undo =
    unlessM (hasTx tid) $ do
        ltsMemPool . mpLocalTxs . at tid .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at tid .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: Monad m => LocalToilM p m Int
memPoolSize = use $ ltsMemPool . mpSize

-- | Extended version of 'LocalToilM'. It allows to put extra data
-- into reader context, extra state and also adds logging
-- capabilities. It's needed for explorer which has more complicated
-- transaction processing.
type ExtendedLocalToilM extraEnv extraState p m =
    ReaderT (LocalToilEnv p, extraEnv) (
        StateT (LocalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'LocalToilM to 'ExtendedLocalToilM'.
extendLocalToilM :: Monad m => LocalToilM p m a -> ExtendedLocalToilM extraEnv extraState p m a
extendLocalToilM = mapReaderT (mapStateT lift . zoom _1) . magnify _1

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in global Toil.
data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    , _gtsPactState    :: !PactState
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: GlobalToilState
defGlobalToilState =
    GlobalToilState 
    { _gtsUtxoModifier = mempty
    , _gtsStakesView = def
    , _gtsPactState = def
    }

makeLenses ''GlobalToilState

-- | Immutable environment used in global Toil.
data GlobalToilEnv p m = GlobalToilEnv
    { _gteUtxo        :: !UtxoLookup
    , _gteTotalStake  :: !Coin
    , _gteStakeGetter :: (StakeholderId -> m (Maybe Coin)) 
    , _gteGasModel    :: !Pact.GasModel
    , _gtePactMPDB    :: !(MPDB p)
    }

makeLenses ''GlobalToilEnv

-- | Monad in which global Toil happens.
type GlobalToilM p m
     = ReaderT (GlobalToilEnv p m) (StateT GlobalToilState (NamedPureLogger m))

runGlobalToilM 
    :: forall p m a. (WithLogger m)
    => GlobalToilEnv p m
    -> GlobalToilState
    -> GlobalToilM p m a
    -> m (a, GlobalToilState)
runGlobalToilM env gts =
    launchNamedPureLog id . usingStateT gts . usingReaderT env

-- | Get stake of a given stakeholder.
getStake :: Monad m => StakeholderId -> GlobalToilM p m (Maybe Coin)
getStake shId = do
    stakeGetter <- view gteStakeGetter
    (<|>) <$> use (gtsStakesView . svStakes . at shId) <*> (lift . lift . lift $ (stakeGetter shId))

-- | Get total stake of all stakeholders.
getTotalStake :: Monad m => GlobalToilM p m Coin
getTotalStake =
    maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

-- | Set stake of a given stakeholder.
setStake :: Monad m => StakeholderId -> Coin -> GlobalToilM p m ()
setStake shId c = gtsStakesView . svStakes . at shId .= Just c

-- | Set total stake of all stakeholders.
setTotalStake :: Monad m => Coin -> GlobalToilM p m ()
setTotalStake c = gtsStakesView . svTotal .= Just c

-- | Extended version of 'GlobalToilM'. It allows to put extra data
-- into reader context and extra state. It's needed for explorer which
-- has more complicated transaction processing.
type ExtendedGlobalToilM extraEnv extraState p m =
    ReaderT (GlobalToilEnv p m, extraEnv) (
        StateT (GlobalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'GlobalToilM to 'ExtendedGlobalToilM'.
extendGlobalToilM :: Monad m => GlobalToilM p m ~> ExtendedGlobalToilM extraEnv extraState p m
extendGlobalToilM = zoom _1 . magnify _1

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

verifyAndApplyMToLocalToilM :: forall a p m.Monad m => VerifyAndApplyM m a -> LocalToilM p m a
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

verifyAndApplyMToGlobalToilM :: forall p a m.Monad m => VerifyAndApplyM m a -> GlobalToilM p m a
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
    { _peeLoggers :: !Pact.Loggers
    , _peeGasEnv  :: !Pact.GasEnv
    }

makeLenses ''PactExecEnv

-- | Mutable state used in Pact execution.
data PactExecState p = PactExecState
    { _pesRefStore :: !Pact.RefStore
    , _pesMPTreeDB :: !(Pact.MPTreeDB p)
    }

makeLenses ''PactExecState

-- | Utility monad which allows to run Pact commands.
type PactExecM p m
     = ReaderT PactExecEnv (StateT (PactExecState p) m)