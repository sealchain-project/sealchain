-- | Utility functions working on Utxo.

module Pos.Chain.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , getTotalCoinsInUtxo
       , getTotalGDsInUtxo
       , utxoToStakes
       , utxoToAddressCoinPairs
       , utxoToAddressCoinMap
       , utxoToAddressGDPairs
       , utxoToAddressGDMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import           Pos.Chain.Genesis (GenesisWStakeholders)
import           Pos.Chain.Txp.Base (addrBelongsTo, addrBelongsToSet,
                     txOutStake)
import           Pos.Chain.Txp.Toil.Types (Utxo)
import           Pos.Chain.Txp.Tx (TxOut (..), isOriginTxOut, isGDTxOut)
import           Pos.Chain.Txp.TxOutAux (TxOutAux (..))
import           Pos.Core (Address, Coin, GoldDollar, StakesMap, 
                     sumCoins, unsafeAddCoin, unsafeIntegerToCoin,
                     unsafeAddGoldDollar, sumGoldDollars, unsafeIntegerToGoldDollar)

-- | Select only TxOuts for given address
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Select only TxOuts for given addresses
filterUtxoByAddrs :: [Address] -> Utxo -> Utxo
filterUtxoByAddrs addrs =
    let addrSet = HS.fromList addrs
    in  M.filter (`addrBelongsToSet` addrSet)

-- | Get total amount of coins in given Utxo
getTotalCoinsInUtxo :: Utxo -> Coin
getTotalCoinsInUtxo =
    unsafeIntegerToCoin . 
    sumCoins .
    map (txOutValue . toaOut) . 
    filter (isOriginTxOut . toaOut) . 
    toList

-- | Get total amount of gds in given Utxo
getTotalGDsInUtxo :: Utxo -> GoldDollar
getTotalGDsInUtxo =
    unsafeIntegerToGoldDollar . 
    sumGoldDollars .
    map (txOutGD . toaOut) . 
    filter (isGDTxOut . toaOut) . 
    toList

-- | Convert 'Utxo' to 'StakesMap'.
utxoToStakes :: GenesisWStakeholders -> Utxo -> StakesMap
utxoToStakes bootStakeholders = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, TxOutAux txOut) =
        foldl' plusAt hm (txOutStake bootStakeholders txOut)

utxoToAddressCoinPairs :: Utxo -> [(Address, Coin)]
utxoToAddressCoinPairs utxo = combineWith unsafeAddCoin txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

    txOuts :: [(Address, Coin)]
    txOuts = map processTxOutAux utxoOriginTxOuts

    utxoOriginTxOuts :: [TxOutAux]
    utxoOriginTxOuts = filter (isOriginTxOut . toaOut) $ M.elems utxo

    processTxOutAux :: TxOutAux -> (Address, Coin)
    processTxOutAux (TxOutAux txOut) = (txOutAddress txOut, txOutValue txOut)

utxoToAddressGDPairs :: Utxo -> [(Address, GoldDollar)]
utxoToAddressGDPairs utxo = combineWith unsafeAddGoldDollar txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

    txOuts :: [(Address, GoldDollar)]
    txOuts = map processTxOutAux utxoGDTxOuts

    utxoGDTxOuts :: [TxOutAux]
    utxoGDTxOuts = filter (isGDTxOut . toaOut) $ M.elems utxo

    processTxOutAux :: TxOutAux -> (Address, GoldDollar)
    processTxOutAux (TxOutAux txOut) = (txOutAddress txOut, txOutGD txOut)

utxoToAddressCoinMap :: Utxo -> HashMap Address Coin
utxoToAddressCoinMap = HM.fromList . utxoToAddressCoinPairs

utxoToAddressGDMap :: Utxo -> HashMap Address GoldDollar
utxoToAddressGDMap = HM.fromList . utxoToAddressGDPairs
