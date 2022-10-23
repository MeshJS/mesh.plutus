{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module MultiSig.Types
    ( MultiSigParams (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)

import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Integer )
import           Ledger                    ( PubKeyHash )


data MultiSigParams = MultiSigParams
    { signatories :: [PubKeyHash]
    , minSigs     :: Plutus.Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MultiSigParams
PlutusTx.makeLift ''MultiSigParams
