{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

-- | Some monads used in Toil and primitive actions.

module Pos.Chain.Txp.Toil.Monad
       ( UtxoM
       , utxoGet
       , utxoPut
       , utxoDel
       , evalUtxoM
       , execUtxoM
       , runUtxoM

        -- * Pact execution
       , PactExecEnv (..)
       , PactExecState (..)
       , PactExecM
       , peeLoggers
       , peeGasModel
       , pesRefStore
       , pesMPTreeDB

       , VerifyAndApplyM
       , VerifyAndApplyEnv (..)
       , VerifyAndApplyState (..)
       , vaaeUtxoLookup
       , vaaeGasModel
       , vaaePersister
       , vaasUtxoModifier
       , vaasPactState
         -- * Monadic local Toil
       , LocalToilEnv (..)
       , LocalToilState (..)
       , lteUtxoLookup
       , lteGasModel
       , ltePersister
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
       , gteUtxoLookup
       , gteTotalStake
       , gteStakeGetter
       , gteGasModel
       , gtePersister
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake
       , ExtendedGlobalToilM
       , extendGlobalToilM

        -- Convertions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM
       , pactExecMToLocalToilM
       , pactExecMToGlobalToilM
       , utxoMToVerifyAndApplyM
       , pactExecMToVerifyAndApplyM
       , verifyAndApplyMToLocalToilM
       , verifyAndApplyMToGlobalToilM

       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))

import qualified Pact.Persist.MPTree as Pact (MPTreeDB (..), 
                     getStateRoot, getModifier, newMPTreeDB)
import qualified Pact.Types.Gas as Pact (GasModel)
import qualified Pact.Types.Logger as Pact (Loggers, neverLog)
import qualified Pact.Types.Runtime as Pact (RefStore)

import           Pos.Chain.Txp.Toil.Types (MemPool, StakesView, UndoMap,
                     UtxoLookup, UtxoModifier, PactState (..), defPactState,
                     mpLocalTxs, mpSize, svStakes, svTotal)
import           Pos.Chain.Txp.Tx (TxId, TxIn)
import           Pos.Chain.Txp.TxAux (TxAux)
import           Pos.Chain.Txp.TxOutAux (TxOutAux)
import           Pos.Chain.Txp.Undo (TxUndo)
import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Sealchain.Mpt.MerklePatriciaMixMem (StateRoot (..))

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
utxoPut tid txOut = utxoGet tid >>= \case
    Nothing -> identity %= MM.insert tid txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|tid|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel tid = utxoGet tid >>= \case
    Just _  -> identity %= MM.delete tid
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|tid|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monadic actions with Pact.
----------------------------------------------------------------------------

-- | Immutable environment used in Pact execution.
data PactExecEnv = PactExecEnv
    { _peeLoggers  :: !Pact.Loggers
    , _peeGasModel :: !Pact.GasModel
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

----------------------------------------------------------------------------
-- Monad used for verify and apply tx.
----------------------------------------------------------------------------

data VerifyAndApplyState = VerifyAndApplyState
    { _vaasUtxoModifier :: !UtxoModifier
    , _vaasPactState    :: !PactState
    }

makeLenses ''VerifyAndApplyState

data VerifyAndApplyEnv p = VerifyAndApplyEnv
    { _vaaeUtxoLookup :: !UtxoLookup
    , _vaaeGasModel   :: !Pact.GasModel
    , _vaaePersister  :: !p
    }

makeLenses ''VerifyAndApplyEnv

type VerifyAndApplyM p m
     = ReaderT (VerifyAndApplyEnv p) (StateT VerifyAndApplyState m)

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
    { _lteUtxoLookup :: !UtxoLookup
    , _lteGasModel   :: !Pact.GasModel
    , _ltePersister  :: !p
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
defGlobalToilState :: StateRoot -> GlobalToilState
defGlobalToilState sr =
    GlobalToilState 
    { _gtsUtxoModifier = mempty
    , _gtsStakesView = def
    , _gtsPactState = defPactState sr
    }

makeLenses ''GlobalToilState

-- | Immutable environment used in global Toil.
data GlobalToilEnv p m = GlobalToilEnv
    { _gteUtxoLookup  :: !UtxoLookup
    , _gteTotalStake  :: !Coin
    , _gteStakeGetter :: (StakeholderId -> m (Maybe Coin)) 
    , _gteGasModel    :: !Pact.GasModel
    , _gtePersister   :: !p
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

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: forall p m.Monad m => UtxoM ~> LocalToilM p m
utxoMToLocalToilM = mapReaderT f . magnify lteUtxoLookup
  where
    f :: State UtxoModifier
      ~> StateT LocalToilState m
    f = state . runState . zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: forall p m.Monad m => UtxoM ~> GlobalToilM p m
utxoMToGlobalToilM = mapReaderT f . magnify gteUtxoLookup
  where
    f :: State UtxoModifier
      ~> StateT GlobalToilState (NamedPureLogger m)
    f = state . runState . zoom gtsUtxoModifier

pactExecMToLocalToilM :: forall p m.Monad m => PactExecM p m ~> LocalToilM p m
pactExecMToLocalToilM action = do
    let _peeLoggers = Pact.neverLog -- | TODO xl fix this
    _peeGasModel <- view lteGasModel

    PactState {..} <- use ltsPactState
    let _pesRefStore = _psRefStore
    p <- view ltePersister
    let _pesMPTreeDB = Pact.newMPTreeDB p _psStateRoot _psModifier _peeLoggers

    (res, pes) <- 
        lift . lift $
        usingStateT (PactExecState {..}) $
        usingReaderT (PactExecEnv {..}) $
        action

    let refStore' = pes ^. pesRefStore 
    let stateRoot' = Pact.getStateRoot $ pes ^. pesMPTreeDB 
    let modifier' = Pact.getModifier $ pes ^. pesMPTreeDB
    ltsPactState .= PactState refStore' stateRoot' modifier'
    return res

pactExecMToGlobalToilM :: forall p m.Monad m => PactExecM p m ~> GlobalToilM p m
pactExecMToGlobalToilM action = do
    let _peeLoggers = Pact.neverLog -- | TODO xl fix this
    _peeGasModel <- view gteGasModel

    PactState {..} <- use gtsPactState
    let _pesRefStore = _psRefStore
    p <- view gtePersister
    let _pesMPTreeDB = Pact.newMPTreeDB p _psStateRoot _psModifier _peeLoggers

    (res, pes) <- 
        lift . lift . lift $
        usingStateT (PactExecState {..}) $
        usingReaderT (PactExecEnv {..}) $
        action

    let refStore' = pes ^. pesRefStore 
    let stateRoot' = Pact.getStateRoot $ pes ^. pesMPTreeDB 
    let modifier' = Pact.getModifier $ pes ^. pesMPTreeDB
    gtsPactState .= PactState refStore' stateRoot' modifier'
    return res

utxoMToVerifyAndApplyM :: forall p m.Monad m => UtxoM ~> VerifyAndApplyM p m
utxoMToVerifyAndApplyM = mapReaderT f . magnify vaaeUtxoLookup
  where
    f :: State UtxoModifier
      ~> StateT VerifyAndApplyState m
    f = state . runState . zoom vaasUtxoModifier

pactExecMToVerifyAndApplyM :: forall p m.Monad m => PactExecM p m ~> VerifyAndApplyM p m
pactExecMToVerifyAndApplyM action = do
    let _peeLoggers = Pact.neverLog -- | TODO xl fix this
    _peeGasModel <- view vaaeGasModel

    PactState {..} <- use vaasPactState
    let _pesRefStore = _psRefStore
    p <- view vaaePersister
    let _pesMPTreeDB = Pact.newMPTreeDB p _psStateRoot _psModifier _peeLoggers

    (res, pes) <- 
        lift . lift $
        usingStateT (PactExecState {..}) $
        usingReaderT (PactExecEnv {..}) $
        action

    let refStore' = pes ^. pesRefStore 
    let stateRoot' = Pact.getStateRoot $ pes ^. pesMPTreeDB 
    let modifier' = Pact.getModifier $ pes ^. pesMPTreeDB
    vaasPactState .= PactState refStore' stateRoot' modifier'
    return res

verifyAndApplyMToLocalToilM :: Monad m => VerifyAndApplyM p m a -> LocalToilM p m a
verifyAndApplyMToLocalToilM action = do
    _vaasUtxoModifier <- use ltsUtxoModifier
    _vaasPactState <- use ltsPactState
    _vaaeUtxoLookup <- view lteUtxoLookup
    _vaaeGasModel <- view lteGasModel
    _vaaePersister <- view ltePersister
    (res, vaas) <- 
        lift . lift $
        usingStateT (VerifyAndApplyState{..}) $
        usingReaderT (VerifyAndApplyEnv{..}) $
        action

    ltsUtxoModifier .= vaas ^. vaasUtxoModifier
    ltsPactState .= vaas ^. vaasPactState
    return res

verifyAndApplyMToGlobalToilM :: forall p a m.Monad m => VerifyAndApplyM p m a -> GlobalToilM p m a
verifyAndApplyMToGlobalToilM action = do
    _vaasUtxoModifier <- use gtsUtxoModifier
    _vaasPactState <- use gtsPactState
    _vaaeUtxoLookup <- view gteUtxoLookup
    _vaaeGasModel <- view gteGasModel
    _vaaePersister <- view gtePersister
    (res, vaas) <- 
        lift . lift . lift $
        usingStateT (VerifyAndApplyState{..}) $
        usingReaderT (VerifyAndApplyEnv{..}) $
        action

    gtsUtxoModifier .= vaas ^. vaasUtxoModifier
    gtsPactState .= vaas ^. vaasPactState
    return res
