{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation params
         InputSelectionPolicy (..)
       , defaultInputSelectionPolicy
       , PendingAddresses (..)

       -- * Tx creation
       , HasTxFeePolicy (..)
       , TxCreateMode
       , makeAbstractTx
       , makeUnsignedAbstractTx
       , runTxCreator
       , makePubKeyTx
       , makeMPubKeyTx
       , makeMPubKeyTxAddrs
       , makeRedemptionTx
       , mkOutputsWithRem
       , prepareTxWithFee
       , createGenericTx
       , createTx
       , createMTx
       , createUnsignedTx
       , estimateTxFee

       -- * Fees logic
       , txToLinearFee
       , computeTxFee

       -- * Additional datatypes
       , TxRaw (..)
       , TxCreator
       , TxOutputs
       , TxOwnedInputs
       , TxWithSpendings
       ) where

import           Universum hiding (keys, tail)

import           Control.Lens (makeLenses, (%=), (.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError),
                     runExceptT)
import           Data.Default (Default (..))
import           Data.Fixed (Fixed, HasResolution)
import qualified Data.HashSet as HS
import           Data.List (partition, tail)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Semigroup (Semigroup)
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Pos.Binary (biSize)
import           Pos.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import           Pos.Chain.Txp (Tx (..), TxAux (..), TxFee (..), TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), Utxo, isOriginTxOut, originUtxo, 
                     isGDTxOut, gdUtxo, isStateTxOut, emptyTxCommand)
import           Pos.Client.Txp.Addresses
import           Pos.Client.Txp.Currency
import           Pos.Client.Txp.Failure
import           Pos.Core (Address, Coin, SlotCount, TxFeePolicy (..),
                     TxSizeLinear (..), GoldDollar (..), calculateTxSizeLinear, 
                     coinToInteger, integerToCoin, isRedeemAddress, mkCoin,
                     txSizeLinearMinValue, mkGoldDollar)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic, RedeemSecretKey, SafeSigner,
                     SignTag (SignRedeemTx, SignTx), deterministicKeyGen,
                     fakeSigner, hash, redeemSign, redeemToPublic, safeSign,
                     safeToPublic)
import           Pos.Infra.Util.LogSafe (SecureLog, buildUnsecure)
import           Test.QuickCheck (Arbitrary (..), elements)


type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux
type TxWithSpendings = (TxAux, NonEmpty TxOut)

-- | List of addresses which are refered by at least one output of transaction
-- which is not yet confirmed i.e. detected in block.
newtype PendingAddresses = PendingAddresses (Set Address)
#if MIN_VERSION_base(4,9,0)
    deriving (Show, Semigroup, Monoid)
#else
    deriving (Show, Monoid)
#endif

instance Buildable TxWithSpendings where
    build (txAux, neTxOut) =
        bprint ("("%build%", "%listJson%")") txAux neTxOut

-- This datatype corresponds to raw transaction.
data TxRaw c = TxRaw
    { trInputs         :: !(TxOwnedInputs TxOut)
    -- ^ Selected inputs from Utxo
    , trOutputs        :: ![TxOutAux]
    -- ^ Output addresses of tx (without remaining output)
    , trRemainingMoney :: !c
    -- ^ Remaining money
    } deriving (Show)

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Specifies the way Utxos are going to be grouped.
data InputSelectionPolicy
    = OptimizeForSecurity       -- ^ Spend everything from the address
    | OptimizeForHighThroughput -- ^ No grouping, prefer confirmed addresses
    deriving (Show, Eq, Enum, Bounded, Generic)

instance Buildable InputSelectionPolicy where
    build = \case
        OptimizeForSecurity       -> "securely"
        OptimizeForHighThroughput -> "high throughput"

instance Buildable (SecureLog InputSelectionPolicy) where
    build = buildUnsecure

instance Default InputSelectionPolicy where
    def = defaultInputSelectionPolicy

defaultInputSelectionPolicy :: InputSelectionPolicy
defaultInputSelectionPolicy = OptimizeForSecurity

instance Arbitrary InputSelectionPolicy where
    arbitrary = elements [minBound .. maxBound]

-- | Mode for creating transactions. We need to know fee policy.
class Monad m => HasTxFeePolicy m where
    getTxFeePolicy :: m TxFeePolicy

type TxCreateMode m
    = ( HasTxFeePolicy m
      , MonadAddresses m
      )

-- | Generic function to create an unsigned transaction, given desired inputs and outputs
makeUnsignedAbstractTx
    :: TxOwnedInputs owner
    -> [TxOutAux]
    -> Tx
makeUnsignedAbstractTx inputs outputs = tx
  where
    tx = UnsafeTx txInputs txOutputs emptyTxCommand txAttributes
    txInputs = map snd inputs
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()

-- | Generic function to create a transaction, given desired inputs,
-- outputs and a way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> Either e TxInWitness)
               -> TxOwnedInputs owner
               -> [TxOutAux]
               -> Either e TxAux
makeAbstractTx mkWit txInputs outputs = do
    let tx = makeUnsignedAbstractTx txInputs outputs
        txSigData = TxSigData
            { txSigTxHash = hash tx
            }
    txWitness <- V.fromList . toList <$>
        for txInputs (\(addr, _) -> mkWit addr txSigData)
    pure $ TxAux tx txWitness

-- | Datatype which contains all data from DB which is necessary
-- to create transactions
data TxCreatorData = TxCreatorData
    { _tcdFeePolicy            :: !TxFeePolicy
    , _tcdInputSelectionPolicy :: !InputSelectionPolicy
    }

makeLenses ''TxCreatorData

-- | Transformer which holds data necessary for creating transactions
type TxCreator m = ReaderT TxCreatorData (ExceptT TxError m)

runTxCreator
    :: HasTxFeePolicy m
    => InputSelectionPolicy
    -> TxCreator m a
    -> m (Either TxError a)
runTxCreator inputSelectionPolicy action = runExceptT $ do
    _tcdFeePolicy <- lift $ getTxFeePolicy
    let _tcdInputSelectionPolicy = inputSelectionPolicy
    runReaderT action TxCreatorData{..}

-- | Like 'makePubKeyTx', but allows usage of different signers
makeMPubKeyTx
    :: ProtocolMagic
    -> (owner -> Either e SafeSigner)
    -> TxOwnedInputs owner
    -> [TxOutAux]
    -> Either e TxAux
makeMPubKeyTx pm getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          getSs addr <&> \ss ->
              PkWitness (safeToPublic ss) (safeSign pm SignTx ss sigData)

-- | More specific version of 'makeMPubKeyTx' for convenience
makeMPubKeyTxAddrs
    :: ProtocolMagic
    -> (Address -> Either e SafeSigner)
    -> TxOwnedInputs TxOut
    -> [TxOutAux]
    -> Either e TxAux
makeMPubKeyTxAddrs pm hdwSigners = makeMPubKeyTx pm getSigner
  where
    getSigner txOut = hdwSigners $ txOutAddress txOut

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx
    :: ProtocolMagic
    -> SafeSigner
    -> TxInputs
    -> [TxOutAux]
    -> TxAux
makePubKeyTx pm ss txInputs txOutputs = either absurd identity $
    makeMPubKeyTx pm (\_ -> Right ss) (map ((), ) txInputs) txOutputs

makeRedemptionTx
    :: ProtocolMagic
    -> RedeemSecretKey
    -> TxInputs
    -> TxOutputs
    -> TxAux
makeRedemptionTx pm rsk txInputs txOutputs = either absurd identity $
    makeAbstractTx mkWit (map ((), ) txInputs) (NE.toList txOutputs)
  where rpk = redeemToPublic rsk
        mkWit _ sigData =
            Right $ RedeemWitness rpk (redeemSign pm SignRedeemTx rsk sigData)

-- | Helper for summing values of `TxOutAux`s
sumTxOutMoneys :: (Container toas, Element toas ~ TxOutAux, Currency c)  
               => MoneyGetter c -> toas -> Integer
sumTxOutMoneys getter = sumMoneys . map (getter . toaOut) . toList

integerToFee :: MonadError TxError m => Integer -> m TxFee
integerToFee =
    either (throwError . invalidFee) (pure . TxFee) . integerToCoin
  where
    invalidFee reason = GeneralTxError ("Invalid fee: " <> reason)

fixedToFee :: (MonadError TxError m, HasResolution a) => Fixed a -> m TxFee
fixedToFee = integerToFee . ceiling

-- | get money out of TxOut
type MoneyGetter c = TxOut -> c
-- | Wrapper of Utxo contains specialize currency
data QualifiedUtxo c = UnsafeQualifiedUtxo !Utxo (MoneyGetter c)

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPickingWay c = QualifiedUtxo c -> [TxOutAux] -> c -> Either TxError FlatUtxo

-- TODO [CSM-526] Scatter on submodules

-------------------------------------------------------------------------
-- Simple inputs picking
-------------------------------------------------------------------------

data InputPickerState c = InputPickerState
    { _ipsMoneyLeft        :: !c
    , _ipsAvailableOutputs :: !FlatUtxo
    }

makeLenses ''InputPickerState

type InputPicker c = StateT (InputPickerState c) (Either TxError)

plainInputPicker :: forall c.(Currency c) => PendingAddresses -> InputPickingWay c 
plainInputPicker (PendingAddresses pendingAddrs) (UnsafeQualifiedUtxo utxo getter) _outputs moneyToSpent =
    evalStateT (pickInputs []) (InputPickerState moneyToSpent sortedUnspent)
  where
    onlyConfirmedInputs :: Set.Set Address -> (TxIn, TxOutAux) -> Bool
    onlyConfirmedInputs addrs (_, (TxOutAux{..})) = not ((txOutAddress toaOut) `Set.member` addrs)
    --
    -- NOTE (adinapoli, kantp) Under certain circumstances, it's still possible for the `confirmed` set
    -- to be exhausted and for the utxo to be picked from the `unconfirmed`, effectively allowing for the
    -- old "slow" behaviour which could create linear chains of dependent transactions which can then be
    -- submitted to relays and possibly fail to be accepted if they arrive in an out-of-order fashion,
    -- effectively piling up in the mempool of the edgenode and in need to be resubmitted.
    -- However, this policy significantly reduce the likelyhood of such edge case to happen, as for exchanges
    -- the `confirmed` set would tend to be quite big anyway.
    -- We should revisit such policy and its implications during a proper rewrite.
    --
    -- NOTE (adinapoli, kantp) There is another subtle corner case which involves such partitioning; it's now
    -- in theory (by absurd reasoning) for the `confirmed` set to contain only dust, which would yes involve a
    -- "high throughput" Tx but also a quite large one, bringing it closely to the "Toil too large" error
    -- (The same malady the @OptimiseForSecurity@ policy was affected by).
    sortedUnspent = confirmed ++ unconfirmed

    (confirmed, unconfirmed) =
      -- Give precedence to "confirmed" addresses.
      partition (onlyConfirmedInputs pendingAddrs)
                (sortOn (Down . getter . toaOut . snd) (M.toList utxo))

    pickInputs :: FlatUtxo -> InputPicker c FlatUtxo
    pickInputs inps = do
        moneyLeft <- use ipsMoneyLeft
        if moneyLeft == mkMoney 0
            then return inps
            else do
            mNextOut <- fmap fst . uncons <$> use ipsAvailableOutputs
            case mNextOut of
                Nothing -> throwError NotEnoughMoney
                Just inp@(_, (TxOutAux txOut)) -> do
                    ipsMoneyLeft .= unsafeSubMoney moneyLeft (min (getter txOut) moneyLeft)
                    ipsAvailableOutputs %= tail
                    pickInputs (inp : inps)

-------------------------------------------------------------------------
-- Grouped inputs picking
-------------------------------------------------------------------------

-- | Group of unspent transaction outputs which belongs
-- to one address
data UtxoGroup c = UtxoGroup
    { ugAddr       :: !Address
    , ugTotalMoney :: !c
    , ugUtxo       :: !(NonEmpty (TxIn, TxOutAux))
    } deriving (Show)

-- | Group unspent outputs by addresses
groupUtxo :: Currency c => Utxo -> MoneyGetter c -> [UtxoGroup c]
groupUtxo utxo getter =
    map mkUtxoGroup preUtxoGroups
  where
    futxo = M.toList utxo
    preUtxoGroups = NE.groupAllWith (txOutAddress . toaOut . snd) futxo
    mkUtxoGroup ugUtxo@(sample :| _) =
        let ugAddr = txOutAddress . toaOut . snd $ sample
            ugTotalMoney = unsafeIntegerToMoney . sumTxOutMoneys getter $
                map snd ugUtxo
        in UtxoGroup {..}

data GroupedInputPickerState c = GroupedInputPickerState
    { _gipsMoneyLeft             :: !c
    , _gipsAvailableOutputGroups :: ![UtxoGroup c]
    }

makeLenses ''GroupedInputPickerState

type GroupedInputPicker c = StateT (GroupedInputPickerState c) (Either TxError)

groupedInputPicker :: forall c.Currency c => InputPickingWay c
groupedInputPicker (UnsafeQualifiedUtxo utxo getter) outputs moneyToSpent =
    evalStateT (pickInputs []) (GroupedInputPickerState moneyToSpent sortedGroups)
  where
    gUtxo = groupUtxo utxo getter
    outputAddrsSet = foldl' (flip HS.insert) mempty $
        map (txOutAddress . toaOut) outputs
    isOutputAddr = flip HS.member outputAddrsSet
    sortedGroups = sortOn (Down . ugTotalMoney) $
        filter (not . isOutputAddr . ugAddr) gUtxo
    disallowedInputGroups = filter (isOutputAddr . ugAddr) gUtxo
    disallowedMoney = sumMoneys $ map ugTotalMoney disallowedInputGroups

    pickInputs :: FlatUtxo -> GroupedInputPicker c FlatUtxo
    pickInputs inps = do
        moneyLeft <- use gipsMoneyLeft
        if moneyLeft == mkMoney 0
            then return inps
            else do
                mNextOutGroup <- fmap fst . uncons <$> use gipsAvailableOutputGroups
                case mNextOutGroup of
                    Nothing -> if disallowedMoney >= moneyToInteger moneyLeft
                        then throwError $ NotEnoughAllowedMoney
                        else throwError NotEnoughMoney
                    Just UtxoGroup {..} -> do
                        gipsMoneyLeft .= unsafeSubMoney moneyLeft (min ugTotalMoney moneyLeft)
                        gipsAvailableOutputGroups %= tail
                        pickInputs (toList ugUtxo ++ inps)

-------------------------------------------------------------------------
-- Further logic
-------------------------------------------------------------------------

-- | Given filtered Utxo, desired outputs and fee size,
-- prepare correct inputs and outputs for transaction
-- (and tell how much to send to remaining address)
prepareTxRawWithPicker
    :: (Monad m, Currency c)
    => InputPickingWay c
    -> QualifiedUtxo c
    -> [TxOutAux]
    -> c
    -> TxCreator m (TxRaw c)
prepareTxRawWithPicker inputPicker qutxo@(UnsafeQualifiedUtxo _ getter) outputs fee = do
    mapM_ (checkIsNotRedeemAddr . txOutAddress . toaOut) outputs

    totalMoney <- sumTxOuts outputs
    moneyToSpent <- case integerToMoney (sumMoneys [totalMoney, fee]) of
        -- we don't care about exact number if user desires all money in the world
        Left _  -> throwError NotEnoughMoney
        Right c -> pure c

    when (moneyToSpent == mkMoney 0) $
        throwError $ GeneralTxError "Attempted to send 0 money"

    futxo <- either throwError pure $ inputPicker qutxo outputs moneyToSpent
    case nonEmpty futxo of
        Nothing       -> throwError $ GeneralTxError "Failed to prepare inputs!"
        Just inputsNE -> do
            totalTxAmount <- sumTxOuts $ map snd $ NE.toList inputsNE
            let trInputs = map formTxInputs inputsNE
                trRemainingMoney = totalTxAmount `unsafeSubMoney` moneyToSpent
            let trOutputs = outputs
            pure TxRaw {..}
  where
    sumTxOuts = either (throwError . GeneralTxError) pure .
        integerToMoney . sumTxOutMoneys getter
    formTxInputs (inp, TxOutAux txOut) = (txOut, inp)
    checkIsNotRedeemAddr outAddr =
        when (isRedeemAddress outAddr) $
            throwError $ OutputIsRedeem outAddr

prepareTxRaw
    :: (Monad m, Currency c)
    => PendingAddresses
    -> QualifiedUtxo c
    -> [TxOutAux]
    -> c
    -> TxCreator m (TxRaw c)
prepareTxRaw pendingTx utxo outputs fee = do
    inputSelectionPolicy <- view tcdInputSelectionPolicy
    let inputPicker =
          case inputSelectionPolicy of
            OptimizeForHighThroughput -> plainInputPicker pendingTx
            OptimizeForSecurity       -> groupedInputPicker
    prepareTxRawWithPicker inputPicker utxo outputs fee

-- Returns set of tx outputs including change output (if it's necessary)
mkOutputsWithRem
    :: TxCreateMode m
    => NetworkMagic
    -> SlotCount
    -> AddrData m
    -> (TxRaw Coin)
    -> TxCreator m [TxOutAux]
mkOutputsWithRem nm epochSlots addrData TxRaw {..}
    | trRemainingMoney == mkCoin 0 = pure trOutputs
    | otherwise = do
        changeAddr <- lift . lift $ getNewAddress nm epochSlots addrData
        let txOut = TxOut changeAddr trRemainingMoney
        pure $ TxOutAux txOut : trOutputs

mkOutputsWithRemForUnsignedTx
    :: (TxRaw Coin)
    -> Address
    -> [TxOutAux]
mkOutputsWithRemForUnsignedTx TxRaw {..} changeAddress
    | trRemainingMoney == mkCoin 0 = trOutputs
    | otherwise =
        -- Change is here, so we have to use provided 'changeAddress' for it.
        -- It is assumed that 'changeAddress' was created (as usual HD-address)
        -- by external wallet and stored in the corresponding wallet.
        let txOutForChange = TxOut changeAddress trRemainingMoney
        in TxOutAux txOutForChange : trOutputs

prepareInpsOuts
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, [TxOutAux])
prepareInpsOuts genesisConfig pendingTx utxo outputs addrData = do
    (gdInps, gdOuts) <- prepareGDInpsOutsIfNeeded genesisConfig pendingTx utxo outputs Nothing (Just addrData)
    txRaw@TxRaw {..} <- prepareTxWithFee genesisConfig pendingTx utxo (NE.toList outputs) gdInps gdOuts
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    outputsWithRem <-
        mkOutputsWithRem nm (configEpochSlots genesisConfig) addrData txRaw
    pure (trInputs, outputsWithRem)

prepareInpsOutsForUnsignedTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> TxOutputs
    -> Address
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsForUnsignedTx genesisConfig pendingTx utxo outputs changeAddress = do
    (gdInps, gdOuts) <- prepareGDInpsOutsIfNeeded genesisConfig pendingTx utxo outputs (Just changeAddress) Nothing
    txRaw@TxRaw {..} <- prepareTxWithFee genesisConfig pendingTx utxo (NE.toList outputs) gdInps gdOuts
    let outputsWithRem = mkOutputsWithRemForUnsignedTx txRaw changeAddress
    pure (trInputs, NE.fromList outputsWithRem)

hasStateOuts :: TxOutputs -> Bool
hasStateOuts = not . null . NE.filter (isStateTxOut . toaOut)

createGenericTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> (TxOwnedInputs TxOut -> [TxOutAux] -> Either TxError TxAux)
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTx genesisConfig pendingTx creator inputSelectionPolicy utxo outputs addrData =
    runTxCreator inputSelectionPolicy $ do
        when (hasStateOuts outputs) $ throwError OutputContainsState
        (inps, outs) <- prepareInpsOuts genesisConfig pendingTx utxo outputs addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)

createGenericTxSingle
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> (TxInputs -> [TxOutAux] -> Either TxError TxAux)
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTxSingle genesisConfig pendingTx creator =
    createGenericTx genesisConfig pendingTx (creator . map snd)

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMTx genesisConfig pendingTx groupInputs utxo hdwSigners outputs addrData =
    createGenericTx
        genesisConfig
        pendingTx
        (makeMPubKeyTxAddrs (configProtocolMagic genesisConfig) getSigner)
        groupInputs
        utxo
        outputs
        addrData
  where
    getSigner address =
        note (SafeSignerNotFound address) $
        hdwSigners address

-- | Make a multi-transaction using given secret key and info for
-- outputs.
createTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> SafeSigner
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createTx genesisConfig pendingTx utxo ss outputs addrData = createGenericTxSingle
    genesisConfig
    pendingTx
    (\i o -> Right $ makePubKeyTx (configProtocolMagic genesisConfig) ss i o)
    OptimizeForHighThroughput
    utxo
    outputs
    addrData

-- | Create unsigned Tx, it will be signed by external wallet.
createUnsignedTx
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> Address
    -> m (Either TxError (Tx,NonEmpty TxOut))
createUnsignedTx genesisConfig pendingTx selectionPolicy utxo outputs changeAddress =
    runTxCreator selectionPolicy $ do
        when (hasStateOuts outputs) $ throwError OutputContainsState
        (inps, outs) <- prepareInpsOutsForUnsignedTx genesisConfig
                                                     pendingTx
                                                     utxo
                                                     outputs
                                                     changeAddress
        let tx = makeUnsignedAbstractTx inps (NE.toList outs)
        pure (tx, map fst inps)

estimateTxFee
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> m (Either TxError TxFee)
estimateTxFee genesisConfig pendingTx inputSelectionPolicy utxo outputs =
    runTxCreator inputSelectionPolicy $ do 
        when (hasStateOuts outputs) $ throwError OutputContainsState
        computeTxFee genesisConfig pendingTx utxo outputs

-----------------------------------------------------------------------------
-- Fees logic
-----------------------------------------------------------------------------

-- | Helper function to reduce code duplication
withLinearFeePolicy
    :: Monad m
    => (TxSizeLinear -> TxCreator m a)
    -> TxCreator m a
withLinearFeePolicy action = view tcdFeePolicy >>= \case
    TxFeePolicyUnknown w _ -> throwError $ GeneralTxError $
        sformat ("Unknown fee policy, tag: "%build) w
    TxFeePolicyTxSizeLinear linearPolicy ->
        action linearPolicy

-- | Prepare transaction considering fees
prepareTxWithFee
    :: MonadAddresses m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> [TxOutAux]
    -> [(TxOut, TxIn)]
    -> [TxOutAux]
    -> TxCreator m (TxRaw Coin)
prepareTxWithFee genesisConfig pendingTx utxo outputs otherInps otherOuts =
    withLinearFeePolicy $ \linearPolicy ->
        stabilizeTxFee genesisConfig pendingTx linearPolicy oriUtxo oriOuts otherInps otherOuts
  where
    oriUtxo = UnsafeQualifiedUtxo (originUtxo utxo) txOutValue
    oriOuts = filter (isOriginTxOut . toaOut) outputs

-- | Compute, how much fees we should pay to send money to given
-- outputs
computeTxFee
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxFee
computeTxFee genesisConfig pendingTx utxo outputs = do
    (gdInps, gdOuts) <- prepareGDInpsOutsIfNeeded genesisConfig pendingTx utxo outputs Nothing Nothing
    TxRaw {..} <- prepareTxWithFee genesisConfig pendingTx utxo (NE.toList outputs) gdInps gdOuts
    let outAmount = sumTxOutMoneys txOutValue $ filter (isOriginTxOut . toaOut) trOutputs
        inAmount = sumTxOutMoneys txOutValue $ map (TxOutAux . fst) $ NE.filter (isOriginTxOut . fst) trInputs
        remaining = coinToInteger trRemainingMoney
    integerToFee $ inAmount - outAmount - remaining

-- | Search such spendings that transaction's fee would be stable.
--
-- Stabilisation is simple iterative algorithm which performs
-- @ fee <- minFee( tx(fee) ) @ per iteration step.
-- It does *not* guarantee to find minimal possible fee, but is expected
-- to converge in O(|utxo|) steps, where @ utxo @ a set of addresses
-- encountered in utxo.
--
-- Alogrithm consists of two stages:
--
-- 1. Iterate until @ fee_{i+1} <= fee_i @.
-- It can last for no more than @ ~2 * |utxo| @ iterations. Really, let's
-- consider following cases:
--
--     * Number of used input addresses increased at i-th iteration, i.e.
--       @ |inputs(tx(fee_i))| > |inputs(tx(fee_{i-1}))| @,
--       which can happen no more than |utxo| times.
--
--     * Number of tx input addresses stayed the same, i.e.
--       @ |inputs(tx(fee_i))| = |inputs(tx(fee_{i-1}))| @.
--
--       If @ fee_i <= fee_{i-1} @ then stage 1 has already finished. Otherwise,
--       since inputs and outputs are picked deterministically, inputs and
--       outputs for @ tx(fee_i) @ and @ tx(fee_{i-1}) @ are the same and they
--       differ only in remainder amount. Since we assume @ fee_i > fee_{i-1} @,
--       then @ rem(tx(fee_i)) < rem(tx(fee_{i-1})) @ by evaluation method.
--       It leads to @ size(tx(fee_i)) <= size(tx(fee_{i-1})) @ and thus
--       @ minFee(tx(fee_i)) <= minFee(tx(fee_{i-1})) @,
--       i.e. @ fee_{i+1} <= fee_{i} @.
--
--     * Number if input addresses decreased.
--       It may occur when fee increases more than on current remainder.
--       In this case fee on next iteration would indeed decrease, because
--       size of single input is much greater than any fluctuations of
--       remainder size (in bytes).
--
-- In total, case (1) occurs no more than |utxo| times, case (2) is always
-- followed by case (1), and case (3) terminates current stage immediatelly,
-- thus stage 1 takes no more than, approximatelly, @ 2 * |utxo| @ iterations.
--
-- 2. Once we find such @ i @ for which @ fee_{i+1} <= fee_i @, we can return
-- @ tx(fee_i) @ as answer, but it may contain overestimated fee (which is still
-- valid).
-- To possibly find better solutions we iterate for several times more.
stabilizeTxFee
    :: forall m. MonadAddresses m
    => Genesis.Config
    -> PendingAddresses
    -> TxSizeLinear
    -> QualifiedUtxo Coin
    -> [TxOutAux]
    -> [(TxOut, TxIn)]
    -> [TxOutAux]
    -> TxCreator m (TxRaw Coin)
stabilizeTxFee genesisConfig pendingTx linearPolicy qutxo@(UnsafeQualifiedUtxo utxo _) outputs otherInps otherOuts = do
    minFee <- fixedToFee (txSizeLinearMinValue linearPolicy)
    mtx <- stabilizeTxFeeDo (False, firstStageAttempts) minFee
    case mtx of
        Nothing -> throwError FailedToStabilize
        Just tx -> pure $ tx & \(S.Min (S.Arg _ txRaw)) -> txRaw
  where
    firstStageAttempts = 2 * length utxo + 5
    secondStageAttempts = 10

    stabilizeTxFeeDo :: (Bool, Int)
                     -> TxFee
                     -> TxCreator m $ Maybe (S.ArgMin TxFee (TxRaw Coin))
    stabilizeTxFeeDo (_, 0) _ = pure Nothing
    stabilizeTxFeeDo (isSecondStage, attempt) expectedFee@(TxFee fee) = do
        TxRaw{..} <- prepareTxRaw pendingTx qutxo outputs fee
        let realInputs = NE.fromList $ otherInps <> (NE.toList trInputs)
        let realOutputs = otherOuts <> trOutputs
        let txRaw = TxRaw realInputs realOutputs trRemainingMoney

        let pm = configProtocolMagic genesisConfig
            nm = makeNetworkMagic pm
        fakeChangeAddr <- lift . lift $ getFakeChangeAddress nm $ configEpochSlots
            genesisConfig

        txMinFee <- txToLinearFee linearPolicy $ createFakeTxFromRawTx
            pm
            fakeChangeAddr
            txRaw
        let txRawWithFee = S.Min $ S.Arg expectedFee txRaw
        let iterateDo step = stabilizeTxFeeDo step txMinFee
        case expectedFee `compare` txMinFee of
            LT -> iterateDo (isSecondStage, attempt - 1)
            EQ -> pure (Just txRawWithFee)
            GT -> do
                let nextStep = (True, if isSecondStage then attempt - 1 else secondStageAttempts)
                futureRes <- iterateDo nextStep
                return $! Just txRawWithFee S.<> futureRes

-- | Calcucate linear fee from transaction's size
txToLinearFee
    :: MonadError TxError m
    => TxSizeLinear -> TxAux -> m TxFee
txToLinearFee linearPolicy =
    fixedToFee .
    calculateTxSizeLinear linearPolicy .
    biSize @TxAux

-- | Function is used to calculate intermediate fee amounts
-- when forming a transaction
createFakeTxFromRawTx :: ProtocolMagic -> Address -> (TxRaw Coin) -> TxAux
createFakeTxFromRawTx pm fakeAddr TxRaw{..} =
    let fakeOutMB
            | trRemainingMoney == mkCoin 0 = Nothing
            | otherwise =
                Just $
                TxOutAux (TxOut fakeAddr trRemainingMoney)
        txOutsWithRem = maybe trOutputs (\remTx -> remTx : trOutputs) fakeOutMB

        -- We create fake signers instead of safe signers,
        -- because safe signer requires passphrase
        -- but we don't want to reveal our passphrase to compute fee.
        -- Fee depends on size of tx in bytes, sign of a tx has the fixed size
        -- so we can use arbitrary signer.
        (_, fakeSK) = deterministicKeyGen "patakbardaqskovoroda228pva1488kk"
    in either absurd identity $ makeMPubKeyTxAddrs
           pm
           (\_ -> Right $ fakeSigner fakeSK)
           trInputs
           txOutsWithRem

prepareGDInpsOutsIfNeeded
    :: TxCreateMode m
    => Genesis.Config
    -> PendingAddresses
    -> Utxo
    -> TxOutputs
    -> Maybe Address
    -> Maybe (AddrData m)
    -> TxCreator m ([(TxOut, TxIn)], [TxOutAux])
prepareGDInpsOutsIfNeeded genesisConfig pendingTx utxo outputs changeAddrM addrDataM
    | (length gdOuts) == 0 = pure ([], [])
    | otherwise = do
        txRaw@TxRaw {..} <- prepareTxRaw pendingTx gdQUtxo gdOuts (mkGoldDollar 0)
        outputsWithRem <- mkGDOutputsWithRem nm epochSlots changeAddrM addrDataM txRaw
        pure (NE.toList trInputs, outputsWithRem)
  where 
    nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    epochSlots = configEpochSlots genesisConfig
    gdQUtxo = UnsafeQualifiedUtxo (gdUtxo utxo) txOutGD
    gdOuts = NE.filter (isGDTxOut . toaOut) outputs

mkGDOutputsWithRem
    :: TxCreateMode m
    => NetworkMagic
    -> SlotCount
    -> Maybe Address
    -> Maybe (AddrData m)
    -> TxRaw GoldDollar
    -> TxCreator m [TxOutAux]
mkGDOutputsWithRem nm epochSlots changeAddrM addrDataM TxRaw {..}
    | trRemainingMoney == mkGoldDollar 0 = pure trOutputs
    | otherwise = do
        newAddr <- case addrDataM of
            Just addrData -> lift . lift $ getNewAddress nm epochSlots addrData
            Nothing       -> lift . lift $ getFakeChangeAddress nm epochSlots
        let changeAddr = fromMaybe newAddr changeAddrM
        let txOut = TxOutGD changeAddr trRemainingMoney
        pure $ TxOutAux txOut : trOutputs
