{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction.Trace
    ( --test
    ) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, waitNSlots, runEmulatorTraceIO, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..), EmulatorTrace, payToWallet, setSigningProcess )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..) )
import           Plutus.Contract.Trace (defaultDistFor)
import           Ledger.Value     as Value (singleton, TokenName, CurrencySymbol)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada

import           Prelude      (IO)
import           Data.Default (def)
{-
import Utility         (wallet, wpkh)
import Auction.Offchain (endpoints)
import Auction.Types    (StartParams(..), NFTSale(..))

nftEx1 :: StartParams
nftEx1 = StartParams
    { sPrice = 100
    , sTn    = "Martify"
    , sCs    = "d7da10988747e39eb350427eb3832bf703027019cec98e58ec4dbab7"
    }

nfts1 :: NFTSale
nfts1 = NFTSale (wpkh 3) 100 (sCs nftEx1) (sTn nftEx1)

test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 100_000_000
                                         <> Value.singleton (sCs nftEx1) (sTn nftEx1) 1)
                            , (wallet 4, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 6, Ada.lovelaceValueOf 100_000_000)
                            ]
    --let dist = Map.fromList $ zip wallets funds
    let emCfg = EmulatorConfig { _initialChainState = Left dist
                               , _params = def }
    runEmulatorTraceIO' def emCfg lendingTrace


lendingTrace :: EmulatorTrace ()
lendingTrace = 
    do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        h4 <- activateContractWallet (wallet 4) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h3 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 nfts1
        void $ Emulator.waitNSlots 1
-} 
