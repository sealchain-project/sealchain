-- | Runtime propagation of genesis data (stakes & utxo).

module Pos.Chain.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Chain.Genesis (GenesisData (..),
                     GenesisProtocolConstants (..), GDIssuer (..),
                     getGenesisAvvmBalances, getGenesisNonAvvmBalances)
import           Pos.Chain.Txp.Toil (Utxo, utxoToStakes)
import           Pos.Chain.Txp.Tx (TxIn (..), TxOut (..))
import           Pos.Chain.Txp.TxOutAux (TxOutAux (..))
import           Pos.Core (Address, Coin, StakesMap, 
                     makeRedeemAddress, mkGoldDollar)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (unsafeHash)


genesisStakes :: GenesisData -> StakesMap
genesisStakes genesisData =
    utxoToStakes (gdBootStakeholders genesisData) $ genesisUtxo genesisData

genesisUtxo :: GenesisData -> Utxo
genesisUtxo genesisData =
    let
        networkMagic :: NetworkMagic
        networkMagic = makeNetworkMagic $
                       gpcProtocolMagic $
                       gdProtocolConsts genesisData

        preUtxo :: [(Address, Coin)]
        preUtxo =
            (first (makeRedeemAddress networkMagic) <$> HM.toList
                    (getGenesisAvvmBalances $ gdAvvmDistr genesisData)
                )
                <> (HM.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances
                       genesisData
                   )

        utxoEntry :: (Address, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, coin) =
            (TxInUtxo (unsafeHash addr) 0, TxOutAux (TxOut addr coin))

        stateUtxo :: (TxIn, TxOutAux)
        stateUtxo = 
            let addr = getIssuer $ gdGDIssuer genesisData
            in ( TxInUtxo (unsafeHash addr) maxBound
               , TxOutAux $ TxOutState addr (mkGoldDollar 0) (fromString "Putting Wall Street on the Chain")
               )
    in
        Map.fromList $ (utxoEntry <$> preUtxo) <> [stateUtxo]
