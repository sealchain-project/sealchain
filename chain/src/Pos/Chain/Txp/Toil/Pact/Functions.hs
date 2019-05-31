{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Chain.Txp.Toil.Pact.Functions where

import           Universum

import           Control.Lens ((.=))
import           Control.Monad.Except
import qualified Data.Text as T

import           Pact.Persist.MPTree (MPTreeDB (..), persister)
import           Pact.PersistPactDb (DbEnv (..), pactdb, initDbEnv, createSchema)
import           Pact.Types.Gas (GasEnv (..), GasModel, GasLimit, GasPrice (..))
import           Pact.Types.Persistence (TxId (..))
import           Pact.Types.RPC (ExecMsg (..), PactRPC (..))
import           Pact.Types.Term (PublicKey)
import           Pact.Types.Util (Hash)

import           Pos.Chain.Txp.Command
import           Pos.Chain.Txp.Toil.Failure
import           Pos.Chain.Txp.Toil.Monad
import           Pos.Chain.Txp.Toil.Pact.Interpreter
import           Pos.Core (coinToInteger)

import           Sealchain.Mpt.MerklePatriciaMixMem (MPDB (..), KVPersister, emptyTriePtr)

applyCmd 
  :: (MonadIO m, KVPersister p)
  => Command ByteString 
  -> Hash
  -> GasLimit
  -> GasModel
  -> Set PublicKey
  -> ExceptT ToilVerFailure (PactExecM p m) CommandResult
applyCmd cmd = applyCmd' (verifyCommand cmd)

applyCmd' 
  :: (MonadIO m, KVPersister p)
  => ProcessedCommand ParsedCode 
  -> Hash
  -> GasLimit
  -> GasModel
  -> Set PublicKey
  -> ExceptT ToilVerFailure (PactExecM p m) CommandResult
applyCmd' (ProcFail s) _ _ _ _ = throwError $ ToilPactError (T.pack s)
applyCmd' (ProcSucc cmd) txHash gasLimit gasModel signers = runPayload cmd txHash gasLimit gasModel signers

runPayload 
  :: (MonadIO m, KVPersister p)
  => Command (Payload ParsedCode) 
  -> Hash
  -> GasLimit
  -> GasModel
  -> Set PublicKey
  -> ExceptT ToilVerFailure (PactExecM p m) CommandResult
runPayload Command{..} txHash gasLimit gasModel signers = case (_pPayload _cmdPayload) of
    Exec pm         -> applyExec pm txHash gasLimit gasModel signers gasPrice
    Continuation _  -> throwError $ ToilPactError "Pact continuation is not supportted for now"
  where
    gasPrice = GasPrice . fromInteger . toInteger . coinToInteger $ _pGasPrice _cmdPayload

applyExec 
  :: (MonadIO m, KVPersister p)
  => ExecMsg ParsedCode 
  -> Hash
  -> GasLimit
  -> GasModel
  -> Set PublicKey
  -> GasPrice
  -> ExceptT ToilVerFailure (PactExecM p m) CommandResult
applyExec (ExecMsg parsedCode edata) txHash gasLimit gasModel signers gasPrice = do
  when (null (_pcExps parsedCode)) $ throwError $ ToilPactError "No expressions found"

  refStore <- use pesRefStore
  let gasEnv = GasEnv gasLimit gasPrice gasModel 
  pactDbEnv <- lift $ newPactDbEnv

  let evalEnv = setupEvalEnv pactDbEnv (TxId (1::Word64))
                (MsgData signers edata txHash) refStore gasEnv  
  res <- liftIO $ tryAny $ evalExec evalEnv parsedCode
  case res of
    Left (SomeException ex) -> throwError $ ToilPactError $ "Command execution error" <> (show ex)
    Right EvalResult{..} -> do
      when (isJust _erExec) $ throwError $ ToilPactError "Pact execution is not supportted for now"

      dbEnv <- liftIO $ takeMVar (pdPactDbVar pactDbEnv) 
      pesMPTreeDB .= _db dbEnv -- | save new MPTreeDB
      pesRefStore .= _erRefStore

      return $ CommandResult _erGas

newPactDbEnv 
  :: (MonadIO m, KVPersister p) 
  => PactExecM p m (PactDbEnv (DbEnv (MPTreeDB p)))
newPactDbEnv = do
  mptDb <- use pesMPTreeDB
  loggers <- view peeLoggers
  let dbe = initDbEnv loggers persister mptDb
  pactDbEnv <- liftIO $ PactDbEnv pactdb <$> newMVar dbe
  let curRoot = stateRoot . _mpdb $ mptDb 
  when (curRoot == emptyTriePtr) $ liftIO $ initSchema pactDbEnv -- | init schema when state root is empty
  return pactDbEnv

initSchema :: PactDbEnv (DbEnv p) -> IO ()
initSchema PactDbEnv {..} = do
  createSchema pdPactDbVar