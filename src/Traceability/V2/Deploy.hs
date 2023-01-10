{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Traceability.V2.Deploy
    ( main
    ) where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise                      (serialise)
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Char8                as B (ByteString)
import qualified Data.ByteString.Base16               as B16 (decode)
import qualified Data.ByteString.Lazy                 as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short                as SBS(ShortByteString, toShort)
import           Data.Functor                         (void)
import qualified Ledger.Address                       as Address
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PTSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx                             (toBuiltinData)
import           PlutusTx.Prelude                     (BuiltinByteString, BuiltinData, Either(..), 
                                                       emptyByteString , Integer, Maybe(..), return,  
                                                       toBuiltin, ($))
import           Prelude                              (IO, String, (.))
import           Traceability.V2.Types
import           Traceability.V2.OnChain


-------------------------------------------------------------------------------------
-- START - traceability validator parameters 
-------------------------------------------------------------------------------------
-- These are dummy values and need to be replaced with real values for
-- the appropriate enviornment (eg devnet, testnet or mainnet)
-- **************** WARNING ************************
-- Any changes will require a new deployment of the ETValidator plutus script
-- 1) cd to top level of project
-- 2) nix-shell
-- 3) cabal repl
-- 4) Deploy> main
-- 5) Deploy> q:
-- 6) cd scripts/cardano-cli/[preview|preprod|mainnet]/
-- 7) ./init-tx.sh [preview|preprod|mainnet]
-- 8) Wait for tx to be confirmed on the blockchain before proceeding
-- 9) update the next.js app with the new earthtrust validator address located here:
--    scripts/cardano-cli/[preview|preprod|mainnet]/data/earthtrust-validator.addr

-------------------------------------------------------------------------------------

-- Version number
version :: Integer
version = 1

-- Split of the order total amount between merchant and donor
amountSplit :: Integer
amountSplit = 90   -- 90% goes to the merchant, 10% goes to the donor

-- Service fee to cover tx costs
serviceFee :: Integer
serviceFee = 500000 

-- Merchant Pkh
merchantPubKeyHashBS :: B.ByteString
merchantPubKeyHashBS = "3d62bfdff66855d150b6cf97e4509ef78f5ea6245f642adf7629338c"

-- Donor public key payment hash
donorPubKeyHashBS :: B.ByteString
donorPubKeyHashBS = "b2b0a5ceaf7bc9a56fe619819b8891e6bafeff5c2cb275e333f97a9f"

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682"

-- Refund public key payment hash
refundPubKeyHashBS :: B.ByteString
refundPubKeyHashBS = "290f5ab67005518d393ef65908ab0efd8a70b1225ce875de79c86852"


-------------------------------------------------------------------------------------
-- END - traceability validator parameters 
-------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------
-- START - Derived values
-------------------------------------------------------------------------------------

merchantPaymentPkh :: Address.PaymentPubKeyHash
merchantPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex merchantPubKeyHashBS)

donorPaymentPkh :: Address.PaymentPubKeyHash
donorPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex donorPubKeyHashBS)

adminPaymentPkh :: Address.PaymentPubKeyHash
adminPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex adminPubKeyHashBS)

refundPaymentPkh :: Address.PaymentPubKeyHash
refundPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex refundPubKeyHashBS)


etvParams :: ETValidatorParams
etvParams = ETValidatorParams 
                {
                  etvVersion = version
                , etvSplit = amountSplit
                , etvServiceFee = serviceFee
                , etvMerchantPkh = merchantPaymentPkh
                , etvDonorPkh = donorPaymentPkh
                , etvAdminPkh = adminPaymentPkh
                , etvRefundPkh = refundPaymentPkh
                }

-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------
 
main::IO ()
main = do

    -- Generate redeemers
    writeRedeemerETSpend
    writeRedeemerETRefund

    -- Gerneate the datum
    writeDatum

    -- Generate plutus scripts and hashes
    writeETValidator
    writeETValidatorHash

    return ()

writeDatum :: IO ()
writeDatum = 
    let etDatum = ETDatum 
            {   etdOrderAmount = 100000000                                         
            ,   etdOrderId = "123"
            ,   etdAdaUSDPrice = "0.41546"
            }
        dat = PlutusTx.toBuiltinData etDatum
    in
        LBS.writeFile "deploy/datum-earthtrust.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData dat)


writeRedeemerETSpend :: IO ()
writeRedeemerETSpend = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData Spend
    in
        LBS.writeFile "deploy/redeemer-earthtrust-spend.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerETRefund :: IO ()
writeRedeemerETRefund = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ Refund
    in
        LBS.writeFile "deploy/redeemer-earthtrust-refund.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeETValidator :: IO ()
writeETValidator = void $ writeFileTextEnvelope "deploy/earthtrust-validator.plutus" Nothing serialisedScript
  where
    script :: BuiltinData -> PSU.V2.Validator
    script = etValidator

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ script $ PlutusTx.toBuiltinData etvParams

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS

writeETValidatorHash :: IO ()
writeETValidatorHash = 
    LBS.writeFile "deploy/earthtrust-validator.hash" $ encode $ PlutusTx.toBuiltinData $ PTSU.V2.validatorHash $ typedETValidator $ PlutusTx.toBuiltinData etvParams


-- | Decode from hex base 16 to a base 10 bytestring is needed because
--   that is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                --putStrLn $ "Tx name: " ++ show t
                toBuiltin(decHex)  
            Left _ -> do
                --putStrLn $ "No Token name: " ++ show e
                emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS
