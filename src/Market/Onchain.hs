{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Market.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , scriptAddress
    , buyValidatorHash
    --, nftDatum
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus
import           Ledger 
    (Address(Address), 
     Validator, 
     ValidatorHash,
     PubKeyHash(..), 
     unValidatorScript,
     scriptHashAddress,
     Script,
     Datum(..),
     DatumHash(..))
import           Plutus.V1.Ledger.Credential (Credential(ScriptCredential))
import           Ledger.Typed.Scripts (ValidatorTypes(..))
import           Plutus.Script.Utils.V2.Typed.Scripts 
    (TypedValidator, 
     mkTypedValidator, 
     mkUntypedValidator, 
     validatorScript, 
     validatorHash)
import Plutus.V2.Ledger.Contexts 
    (ScriptContext(..), 
     TxInfo(..),
     TxOut(..),
     txSignedBy,
     txOutDatum,
     txInInfoResolved,
     ownHash,
     txInfoSignatories,
     valuePaidTo)
import           Plutus.V2.Ledger.Tx (OutputDatum(..))
import           Ledger.Value (valueOf, adaSymbol, adaToken)


import Market.Types    (NFTSale(..), SaleAction(..))


{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts r ctx = case r of
    Buy     -> traceIfFalse "1" (valueOf (valuePaidTo info sig) (nCurrency nfts) (nToken nfts) == 1) &&
               traceIfFalse "2" (checkSellerOut (nSeller nfts) (nPrice nfts)) &&
               traceIfFalse "3" checkSingleBuy
    Close   -> traceIfFalse "4" (txSignedBy (scriptContextTxInfo ctx) (nSeller nfts))

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash
    
    checkSingleBuy :: Bool
    checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs info), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
        length is == 1
    
    checkSellerOut :: PubKeyHash -> Integer -> Bool
    checkSellerOut seller price = valueOf (valuePaidTo info seller) adaSymbol adaToken >= price


data Sale
instance ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: TypedValidator Sale
typedBuyValidator = go where
    go = mkTypedValidator @Sale
         $$(PlutusTx.compile [|| mkBuyValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

buyValidator :: Validator
buyValidator = validatorScript typedBuyValidator

buyValidatorHash :: ValidatorHash
buyValidatorHash = validatorHash typedBuyValidator

scriptAddress :: Address
scriptAddress = scriptHashAddress buyValidatorHash

buyScript :: Script
buyScript = Ledger.unValidatorScript buyValidator

buyScriptAsShortBs :: SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise $ buyScript

apiBuyScript :: PlutusScript PlutusScriptV2
apiBuyScript = PlutusScriptSerialised buyScriptAsShortBs
