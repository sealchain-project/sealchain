{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.Toil.Pact.Interpreter
  ( PactDbEnv(..)
  , MsgData(..)
  , EvalResult(..)
  , evalExec
  , evalExecState
  , setupEvalEnv
  , interpret
  ) where

import           Universum hiding (try)

import           Control.Monad.Catch hiding (throwM)
import qualified Data.Set as S
import           Data.Aeson
import           Data.Default

import           Pact.Types.Term
import           Pact.Types.Runtime
import           Pact.Compile
import           Pact.Eval hiding (evalContinuation)

import           Pos.Chain.Txp.Command (ParsedCode (..))

data PactDbEnv e = PactDbEnv {
  pdPactDb    :: !(PactDb e),
  pdPactDbVar :: !(MVar e)
  }

data MsgData = MsgData {
  mdSigs :: !(S.Set PublicKey),
  mdData :: !Value,
  mdHash :: !Hash
  }

data EvalResult = EvalResult
  { _erInput    :: ![Term Name]
  , _erOutput   :: ![Term Name]
  , _erLogs     :: ![TxLog Value]
  , _erRefStore :: !RefStore
  , _erExec     :: !(Maybe PactExec)
  , _erGas      :: Gas
  } deriving (Eq,Show)


evalExec :: EvalEnv e -> ParsedCode -> IO EvalResult
evalExec env pc = evalExecState def env pc

evalExecState :: EvalState -> EvalEnv e -> ParsedCode -> IO EvalResult
evalExecState initState evalEnv ParsedCode {..} = do
  terms <- throwEither $ compileExps (mkTextInfo _pcCode) _pcExps
  interpret initState evalEnv terms

setupEvalEnv
  :: PactDbEnv e
  -> TxId
  -> MsgData
  -> RefStore
  -> GasEnv
  -> EvalEnv e
setupEvalEnv dbEnv tId msgData refStore gasEnv =
  EvalEnv {
    _eeRefStore = refStore
  , _eeMsgSigs = mdSigs msgData
  , _eeMsgBody = mdData msgData
  , _eeTxId = Just tId
  , _eeEntity = Nothing
  , _eePactStep = Nothing
  , _eePactDb = pdPactDb dbEnv
  , _eePactDbVar = pdPactDbVar dbEnv
  , _eePurity = PImpure
  , _eeHash = mdHash msgData
  , _eeGasEnv = gasEnv
  , _eeNamespacePolicy = permissiveNamespacePolicy
  , _eeSPVSupport = noSPVSupport
  }

interpret :: EvalState -> EvalEnv e -> [Term Name] -> IO EvalResult
interpret initState evalEnv terms = do
  ((rs,logs), eState) <-
    runEval initState evalEnv $ evalTerms terms
  let gas = _evalGas eState
      refStore = updateRefStore (_evalRefs eState) . _eeRefStore $ evalEnv
      pactExec = _evalPactExec eState
  return $! EvalResult terms rs logs refStore pactExec gas

evalTerms :: [Term Name] -> Eval e ([Term Name],[TxLog Value])
evalTerms terms = do
  let safeRollback =
        void (try (evalRollbackTx def) :: Eval e (Either SomeException ()))
  handle (\(e :: SomeException) -> safeRollback >> throwM e) $ do
        evalBeginTx def
        rs <- mapM eval terms
        logs <- evalCommitTx def
        return (rs,logs)
{-# INLINE evalTerms #-}
