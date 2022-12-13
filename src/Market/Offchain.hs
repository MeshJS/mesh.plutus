{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Market.Offchain
    ( endpoints
    )
    where

import qualified Data.Map                  as Map
import           Data.Monoid               as Mnd ( (<>), mconcat )
import           Control.Monad             ( void, forever, (>>) )
import           Data.Aeson                (ToJSON)
import           Data.Text                 (Text)
import           Prelude                   (String, fromIntegral, ceiling, Float, (*), (-), (/), show, and, const)


import Plutus.Contract as Contract
import PlutusTx.Prelude as Plutus
import PlutusTx.AssocMap as AM
import PlutusTx (toBuiltinData)
import Ledger
import Plutus.V2.Ledger.Tx as V2.Tx
import Ledger.Constraints as Constraints
import Ledger.Value as Value
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Playground.TH
import Playground.Types
import Plutus.Contract.Request (utxosTxOutTxAt)
import qualified Ledger.Constraints as Constraint

import           Market.Types               (NFTSale(..), SaleAction(..), SaleSchema, StartParams(..))
import           Market.Onchain             ( Sale, typedBuyValidator, buyValidator, buyValidatorHash, scriptAddress )
import           Utility                    (wallet, wpkh)
import qualified Plutus.V2.Ledger.Api as Plutus


startSale :: StartParams -> Contract w SaleSchema Text ()
startSale sp = do
    pkh <- Contract.ownFirstPaymentPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
    let val     = Value.singleton (sCs sp) (sTn sp) 1 Mnd.<> Value.singleton adaSymbol adaToken 2000000
        nfts    = NFTSale { nSeller = unPaymentPubKeyHash pkh, nToken = sTn sp, nCurrency = sCs sp, nPrice = sPrice sp}
        lookups = Constraints.unspentOutputs utxos Mnd.<>
                  Constraints.typedValidatorLookups typedBuyValidator
        tx      = Constraints.mustPayToTheScript nfts val
    ledgerTx <- submitTxConstraintsWith @Sale lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String "startSale transaction confirmed"


buy :: NFTSale -> Contract w SaleSchema Text ()
buy nfts = do
    pkh <- Contract.ownFirstPaymentPubKeyHash 
    sale <- findSale (nCurrency nfts, nToken nfts)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Buy
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1 Mnd.<> Value.singleton adaSymbol adaToken 2000000
                valAdaS = Value.singleton adaSymbol adaToken (nPrice nfts) Mnd.<> Value.singleton adaSymbol adaToken 2000000
            let lookups = Constraints.typedValidatorLookups typedBuyValidator Mnd.<>
                          Constraints.otherData (Datum (Plutus.toBuiltinData nfts)) Mnd.<>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r           Mnd.<>
                          Constraints.mustPayToPubKey pkh val                Mnd.<>
                          Constraints.mustBeSignedBy pkh                     Mnd.<>
                          Constraints.mustPayToPubKey (PaymentPubKeyHash $ nSeller nfts) valAdaS 
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "buy transaction confirmed"


update :: (NFTSale, Integer) -> Contract w SaleSchema Text ()
update (nfts, newprice) = do
    pkh <- Contract.ownFirstPaymentPubKeyHash 
    sale <- findSale (nCurrency nfts, nToken nfts)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1 Mnd.<> Value.singleton adaSymbol adaToken 2000000
                nfts'   = nfts { nPrice = newprice }
                lookups = Constraints.typedValidatorLookups typedBuyValidator Mnd.<>
                          Constraints.otherData (Datum (Plutus.toBuiltinData nfts)) Mnd.<>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r Mnd.<>
                          Constraints.mustBeSignedBy pkh                     Mnd.<>
                          Constraints.mustPayToTheScript nfts' val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "Price updated"


close :: NFTSale -> Contract w SaleSchema Text ()
close nfts = do
    pkh <- Contract.ownFirstPaymentPubKeyHash 
    sale <- findSale (nCurrency nfts, nToken nfts)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1 Mnd.<> Value.singleton adaSymbol adaToken 200000
                lookups = Constraints.typedValidatorLookups typedBuyValidator Mnd.<>
                          Constraints.otherData (Datum (Plutus.toBuiltinData nfts)) Mnd.<>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r       Mnd.<>
                          Constraints.mustBeSignedBy pkh                     Mnd.<>
                          Constraints.mustPayToPubKey (PaymentPubKeyHash $ nSeller nfts) val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "close transaction confirmed"


findSale :: (AsContractError e, ToJSON e) => (CurrencySymbol, TokenName) -> Contract w SaleSchema e (Maybe (TxOutRef, DecoratedTxOut))
findSale (cs, tn) = do
    utxos <- Map.filter f <$> utxosTxOutTxAt scriptAddress
    return $ case Map.toList utxos of
        [(oref, (o, _))] -> Just (oref, o)
        _           -> Nothing

  where
    f :: (DecoratedTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
    f (o, _) = valueOf (V2.Tx.txOutValue $ toTxInfoTxOut o) cs tn == 1


endpoints :: Contract () SaleSchema Text ()
endpoints = forever
          $ handleError logError
          $ awaitPromise
          $ start' `select` buy1
                   `select` update1
                   `select` close'
  where
    start'          = endpoint @"start"          $ \nfts       -> startSale nfts
    buy1            = endpoint @"buy"            $ \nfts       -> buy nfts
    update1         = endpoint @"update"         $ \(nfts, i)       -> update (nfts, i)
    close'          = endpoint @"close"          $ \nfts       -> close nfts
