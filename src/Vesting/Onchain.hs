{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Vesting.Onchain
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
     Script,
     PubKeyHash)
import           Ledger.Typed.Scripts (ValidatorTypes(..))
import           Plutus.Script.Utils.V2.Typed.Scripts 
    (TypedValidator, 
     mkTypedValidator, 
     mkUntypedValidator, 
     validatorScript)
import Plutus.V2.Ledger.Contexts 
    (ScriptContext(..),
    txSignedBy)


{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> () -> ScriptContext -> Bool
mkValidator pkh _ ctx = txSignedBy (scriptContextTxInfo ctx) pkh


data VestingData
instance ValidatorTypes VestingData where
    type instance DatumType VestingData    = PubKeyHash
    type instance RedeemerType VestingData = ()


typedValidator :: TypedValidator VestingData
typedValidator = go where
    go = mkTypedValidator @VestingData 
         $$(PlutusTx.compile [|| mkValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: Validator
validator = validatorScript typedValidator

script :: Script
script = Ledger.unValidatorScript validator

scriptAsShortBs :: SBS.ShortByteString
scriptAsShortBs = SBS.toShort . LB.toStrict . serialise $ script

apiScript :: PlutusScript PlutusScriptV2
apiScript = PlutusScriptSerialised scriptAsShortBs
