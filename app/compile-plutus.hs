{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Cardano.Api.Shelley
    ( writeFileTextEnvelope,
      Error(displayError),
      PlutusScript,
      PlutusScriptV2,
      ScriptData(ScriptDataNumber),
      toAlonzoData )
import Cardano.Ledger.Alonzo.Data as Alonzo ( Data(Data) )
import qualified Plutus.V2.Ledger.Api as Plutus
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)

import Prelude
import Control.Exception
import System.Environment ( getArgs )
import qualified Data.ByteString.Short as SBS

import AlwaysTrue.Onchain (apiScript, scriptAsShortBs)

vasilPV :: Plutus.ProtocolVersion
vasilPV = Plutus.ProtocolVersion 7 0

main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let scriptnum = if nargs > 0 then read (head args) else 42
    let scriptname = if nargs > 1 then args!!1 else  "alwaystrue.plutus"
    putStrLn $ "Writing output to: " ++ scriptname
    writePlutusScript scriptnum scriptname apiScript scriptAsShortBs


writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV2 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting vasilPV Plutus.Verbose (either throw id $ Plutus.mkEvaluationContext m) scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
