{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module Auction.Onchain
    ( apiAuctionScript
    , auctionScriptAsShortBs
    , typedAuctionValidator
    , Auction
    , auctionValidator
    , scriptAddress
    , auctionValidatorHash
    , auctionDatum
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus
import           Ledger
    (Address,
     Validator,
     ValidatorHash,
     POSIXTimeRange,
     POSIXTime(..),
     interval,
     contains,
     LowerBound(..),
     UpperBound(..),
     Extended(..),
     ivFrom,
     ivTo,
     unValidatorScript,
     scriptHashAddress,
     Script,
     Datum(..),
     DatumHash(..), 
     POSIXTime)
import           Ledger.Typed.Scripts (ValidatorTypes(..))
import           Plutus.Script.Utils.V2.Typed.Scripts 
    (TypedValidator, 
     mkTypedValidatorParam, 
     mkUntypedValidator, 
     validatorScript,
     validatorHash)
import Plutus.V2.Ledger.Contexts 
    (ScriptContext(..), 
     TxInfo(..),
     TxOut(..),
     txSignedBy,
     findDatum,
     getContinuingOutputs,
     valuePaidTo)
import           Plutus.V2.Ledger.Tx (OutputDatum(..))
import           Ledger.Value (valueOf, adaSymbol, adaToken, singleton)


import Auction.Types    (ADatum(..), AAction(..))


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ADatum
auctionDatum o f = case txOutDatum o of
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh -> case f dh of
        Just (Datum d) -> PlutusTx.fromBuiltinData d
        Nothing -> Nothing
    NoOutputDatum -> Nothing

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: () -> ADatum -> AAction -> ScriptContext -> Bool
mkAuctionValidator _ adat r ctx = case r of
    Bid   -> traceIfFalse "1.1" checkTime &&
             traceIfFalse "1.2" checkNewBid
    Close -> traceIfFalse "2.1" (txSignedBy info (aOwner adat)) &&
             traceIfFalse "2.2" checkPayOldBidder &&
             traceIfFalse "2.3" checkTime &&
             traceIfFalse "2.4" checkAuctionLeft
    End   -> traceIfFalse "3.1" checkTimePast &&
             traceIfFalse "3.2" checkWinBid
            
  where
    
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validInterval :: POSIXTimeRange
    validInterval = interval (POSIXTime $ fst $ aInterval adat) (POSIXTime $ snd $ aInterval adat)

    checkNewBid :: Bool
    checkNewBid = let cos = [ co | co <- getContinuingOutputs ctx, uncurry (valueOf (txOutValue co)) (aToken adat) == 1 ] in
        case cos of
            [co] -> let newBid = valueOf (txOutValue co) adaSymbol adaToken in
                case auctionDatum co (`findDatum` info) of
                    Just dat -> case aHighBid dat of
                        Just (newBidder, newHbid) -> case aHighBid adat of
                            Just (oldBidder, oldHbid) -> newHbid == newBid && newBid > oldHbid && oldBidder /= newBidder && dat == adat &&
                                                         (valuePaidTo info oldBidder == singleton adaSymbol adaToken oldHbid)
                            Nothing                   -> newHbid == newBid && newBid > aMinBid adat && dat == adat


    checkPayOldBidder :: Bool
    checkPayOldBidder = case aHighBid adat of
        Nothing -> True
        Just (pkh, i) -> valuePaidTo info pkh == singleton adaSymbol adaToken i

    checkAuctionLeft :: Bool
    checkAuctionLeft = null $ getContinuingOutputs ctx

    checkWinBid :: Bool
    checkWinBid = case aHighBid adat of
        Nothing -> True
        Just (pkh, i) -> valuePaidTo info (aOwner adat) == singleton adaSymbol adaToken i &&
                         uncurry (valueOf (valuePaidTo info pkh)) (aToken adat) == 1

    checkTime :: Bool
    checkTime = validInterval `contains` txInfoValidRange info

    txBegin :: LowerBound POSIXTime -> Extended POSIXTime
    txBegin (LowerBound posx _) = posx 

    auctionEnd :: UpperBound POSIXTime -> Extended POSIXTime
    auctionEnd (UpperBound posx _) = posx 

    checkTimePast :: Bool
    checkTimePast = auctionEnd (ivTo validInterval) < txBegin (ivFrom $ txInfoValidRange info)


data Auction
instance ValidatorTypes Auction where
    type instance DatumType Auction    = ADatum
    type instance RedeemerType Auction = AAction


typedAuctionValidator :: () -> TypedValidator Auction
typedAuctionValidator = go where
    go = mkTypedValidatorParam @Auction
         $$(PlutusTx.compile [|| mkAuctionValidator ||])
         $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

auctionValidator :: () -> Validator
auctionValidator = validatorScript . typedAuctionValidator

auctionValidatorHash :: () -> ValidatorHash
auctionValidatorHash = validatorHash . typedAuctionValidator

scriptAddress :: () -> Address
scriptAddress = scriptHashAddress . auctionValidatorHash

auctionScript :: () -> Script
auctionScript = Ledger.unValidatorScript . auctionValidator

auctionScriptAsShortBs :: () -> SBS.ShortByteString
auctionScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . auctionScript

apiAuctionScript :: () -> PlutusScript PlutusScriptV2
apiAuctionScript = PlutusScriptSerialised . auctionScriptAsShortBs
