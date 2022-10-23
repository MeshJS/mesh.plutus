{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}

module MultiSig.Onchain
    ( apiScript
    , scriptAsShortBs
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus
import           Ledger
    (Validator,
     unValidatorScript,
     Script)
import           Ledger.Typed.Scripts (ValidatorTypes(..))
import           Plutus.Script.Utils.V2.Typed.Scripts 
    (TypedValidator, 
     mkTypedValidatorParam, 
     mkUntypedValidator, 
     validatorScript)
import Plutus.V2.Ledger.Contexts 
    (ScriptContext(..), txInfoSignatories)

import MultiSig.Types (MultiSigParams(..))

{-# INLINABLE mkValidator #-}
mkValidator :: MultiSigParams -> () -> () -> ScriptContext -> Bool
mkValidator MultiSigParams{signatories, minSigs} _ _ ctx = length [ sig | sig <- txInfoSignatories (scriptContextTxInfo ctx), sig `elem` signatories ] >= minSigs


data Always
instance ValidatorTypes Always where
    type instance DatumType Always    = ()
    type instance RedeemerType Always = ()


typedValidator :: MultiSigParams -> TypedValidator Always
typedValidator = go where
    go = mkTypedValidatorParam @Always 
         $$(PlutusTx.compile [|| mkValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: MultiSigParams -> Validator
validator = validatorScript . typedValidator

script :: MultiSigParams -> Script
script = Ledger.unValidatorScript . validator

scriptAsShortBs :: MultiSigParams -> SBS.ShortByteString
scriptAsShortBs = SBS.toShort . LB.toStrict . serialise . script

apiScript :: MultiSigParams -> PlutusScript PlutusScriptV2
apiScript = PlutusScriptSerialised . scriptAsShortBs
