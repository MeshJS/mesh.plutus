{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Auction.Types
    ( AAction (..)
    , SaleSchema
    , StartParams (..)
    , ADatum (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..), Maybe)
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Integer, Eq(..), (&&) )
import           Plutus.V1.Ledger.Time     (POSIXTimeRange, POSIXTime)
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash )
import           Plutus.Contract           ( Endpoint, type (.\/) )


data ADatum = ADatum
    { aOwner      :: PubKeyHash
    , aMinBid     :: Plutus.Integer
    , aHighBid    :: Maybe (PubKeyHash, Integer)
    , aInterval   :: POSIXTimeRange
    , aToken      :: (CurrencySymbol, TokenName)
    } deriving (Generic, ToJSON, FromJSON)

instance Eq ADatum where
    x == y = aOwner x    == aOwner y &&
             aMinBid x   == aMinBid y &&
             aInterval x == aInterval y &&
             aToken x    == aToken y

PlutusTx.unstableMakeIsData ''ADatum
PlutusTx.makeLift ''ADatum


data AAction = Bid | Close | End
    deriving Show

PlutusTx.unstableMakeIsData ''AAction
PlutusTx.makeLift ''AAction


data StartParams = StartParams
    { sMinBid   :: Integer
    , sCs       :: CurrencySymbol
    , sTn       :: TokenName
    , sDeadline :: POSIXTime 
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


type SaleSchema = Endpoint "close" ADatum
                  .\/
                  Endpoint "bid" (ADatum, Integer)
                  .\/
                  Endpoint "start" StartParams
