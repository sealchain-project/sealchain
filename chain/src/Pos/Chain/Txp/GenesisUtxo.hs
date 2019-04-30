-- | Runtime propagation of genesis data (stakes & utxo).

module Pos.Chain.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Chain.Genesis (GenesisData (..), getGenesisNonAvvmBalances)
import           Pos.Chain.Txp.Toil (Utxo, utxoToStakes)
import           Pos.Chain.Txp.Tx (TxIn (..), TxOut (..))
import           Pos.Chain.Txp.TxOutAux (TxOutAux (..))
import           Pos.Core (Address, Coin, StakesMap)
import           Pos.Crypto (unsafeHash)


genesisStakes :: GenesisData -> StakesMap
genesisStakes genesisData =
    utxoToStakes (gdBootStakeholders genesisData) $ genesisUtxo genesisData

genesisUtxo :: GenesisData -> Utxo
genesisUtxo genesisData =
    let
        preUtxo :: [(Address, Coin)]
        preUtxo = HM.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances genesisData

        utxoEntry :: (Address, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, coin) =
            (TxInUtxo (unsafeHash addr) 0, TxOutAux (TxOut addr coin))
    in
        Map.fromList $ utxoEntry <$> preUtxo
