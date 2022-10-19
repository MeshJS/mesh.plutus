{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Market.Types
    ( SaleAction (..)
    , SaleSchema
    , StartParams (..)
    , NFTSale (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..))
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Integer )
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash )
import           Plutus.Contract           ( Endpoint, type (.\/) )



data NFTSale = NFTSale
    { nSeller    :: PubKeyHash
    , nPrice     :: Plutus.Integer
    , nCurrency  :: CurrencySymbol
    , nToken     :: TokenName
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NFTSale
PlutusTx.makeLift ''NFTSale


data SaleAction = Buy | Close
    deriving Show

PlutusTx.unstableMakeIsData ''SaleAction
PlutusTx.makeLift ''SaleAction


data StartParams = StartParams
    { sPrice :: Integer
    , sCs    :: CurrencySymbol
    , sTn    :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


type SaleSchema = Endpoint "close" NFTSale
                  .\/
                  Endpoint "buy" NFTSale
                  .\/
                  Endpoint "update" (NFTSale, Integer)
                  .\/
                  Endpoint "start" StartParams
