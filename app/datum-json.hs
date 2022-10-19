{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import System.Environment ( getArgs )
import Prelude
import Data.String (fromString)

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx
import Ledger (PubKeyHash(..))
import PlutusTx.AssocMap as AM (fromList)


main :: IO ()
main = do
  [] <- getArgs
 -- let datum = PubKeyHash 
 -- writeData "reserve_datum.json" datum
  putStrLn "Done"


writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

