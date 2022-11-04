{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-} 
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Traceability.V2.OnChain 
    (
      ETDatum(..)
    , etHash
    , etValidator
    , typedETValidator
    ) where


import           Data.Aeson                                     (FromJSON, ToJSON)
import           GHC.Generics                                   (Generic)
import qualified Ledger.Ada as Ada                              (lovelaceValueOf)
import qualified Ledger.Address as Address                      (Address, pubKeyHashAddress, PaymentPubKeyHash(..))
import qualified Plutus.Script.Utils.Typed as Typed             (Any, validatorScript)
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2       (Validator, ValidatorHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2 (TypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2 (unsafeMkTypedValidator, 
                                                                 validatorHash)
import qualified Ledger.Value as Value                          (Value)
import qualified Plutus.V2.Ledger.Contexts as ContextsV2        (ScriptContext, TxInfo(..), TxInInfo(..),  
                                                                txInfoOutputs, TxOut(..), txOutValue, txSignedBy)
import qualified Plutus.V2.Ledger.Api as PlutusV2               (mkValidatorScript, scriptContextTxInfo,  
                                                                 unsafeFromBuiltinData)
import qualified PlutusTx                                       (applyCode, compile, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                               (Bool(..), BuiltinData, BuiltinByteString, check, divide, Integer, Maybe(..), 
                                                                 otherwise, traceIfFalse, (&&), (==), ($), (-), (*), (+))
import           Prelude                                        (Show (..))
import           Traceability.V2.Types                          (ETRedeemer(..), ETValidatorParams(..))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | ETDatum is used to record the total amount of the order, the order id,
--   the service fee and the refund address if required.
data ETDatum = ETDatum
    {   etdAmount           :: Integer 
    ,   etdOrderId          :: BuiltinByteString
    ,   etdServiceFee       :: Integer
    ,   etdRefundPkh        :: Address.PaymentPubKeyHash                                                                                                            
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ETDatum [('ETDatum, 0)]
PlutusTx.makeLift ''ETDatum
                                                      
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (ContextsV2.txOutAddress x == scriptAddr) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | Check that the value is there for the provided outputs
{-# INLINABLE validOutput #-}
validOutput :: Value.Value -> [ContextsV2.TxOut] -> Bool
validOutput _ [] = False
validOutput txVal (x:xs)
    | (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutput txVal xs


-- | Check to see if the buy token is in the list of inputs locked at an address
{-# INLINABLE validInput #-}
validInput :: Value.Value -> [ContextsV2.TxInInfo] -> Bool
validInput _ [] = False
validInput txVal (x:xs)
    | validOutput txVal [ContextsV2.txInInfoResolved x] = True
    | otherwise = validInput txVal xs


-- | mkETValidator is the minting policy is for creating an Earthtrust order token when
--   an order is submitted.
{-# INLINABLE mkETValidator #-}
mkETValidator :: ETValidatorParams -> ETDatum -> ETRedeemer -> ContextsV2.ScriptContext -> Bool
mkETValidator params dat red ctx = 
    case red of
        Spend -> traceIfFalse "ETV1" checkMerchantOutput 
                && traceIfFalse "ETV2" checkDonorOutput 
                && traceIfFalse "ETV3" checkInput  
                && traceIfFalse "ETV4" signedByAdmin
        Refund amt -> (traceIfFalse "ETV5" $ checkRefundOutput amt)
                && (traceIfFalse "ETV6" $ checkRefundInput amt)
                && traceIfFalse "ETV7" signedByAdmin
                
    where
        info :: ContextsV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx  

        split :: Integer
        split = etvSplit params

        adaAmount :: Integer
        adaAmount = etdAmount dat

        serviceFee :: Integer
        serviceFee = etdServiceFee dat

        merchantAddress :: Address.Address
        merchantAddress = Address.pubKeyHashAddress (etvMerchantPkh params) Nothing

        merchantAmount :: Value.Value
        merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

        donorAddress :: Address.Address
        donorAddress = Address.pubKeyHashAddress (etvDonorPkh params) Nothing

        refundAddress :: Address.Address
        refundAddress = Address.pubKeyHashAddress (etdRefundPkh dat) Nothing

        donorAmount :: Value.Value
        donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)

        -- | Admin signature required to run the smart contract
        signedByAdmin :: Bool
        signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (etvAdminPkh params)

        -- | Check that both the split amount value is correct and at the correct
        --   address for the merchant     
        checkMerchantOutput :: Bool
        checkMerchantOutput = validOutputs merchantAddress merchantAmount (ContextsV2.txInfoOutputs info)

        -- | Check that both the split amount value is correct and at the correct
        --   address for the donor  
        checkDonorOutput :: Bool
        checkDonorOutput = validOutputs donorAddress donorAmount (ContextsV2.txInfoOutputs info)

        -- | Checks that the amount in the datum matches the actual amount in the input
        --   transaction
        checkInput :: Bool
        checkInput = validInput (Ada.lovelaceValueOf (adaAmount + serviceFee)) (ContextsV2.txInfoInputs info)

        -- | Checks that the amount in the datum matches the actual amount in the input
        --   transaction
        checkRefundInput :: Integer -> Bool
        checkRefundInput refundAmt = validInput (Ada.lovelaceValueOf (refundAmt + serviceFee)) (ContextsV2.txInfoInputs info)

        -- | Check that the refund output contains the correct ada amount and
        --   goes to the refund address
        checkRefundOutput :: Integer -> Bool
        checkRefundOutput refundAmt = validOutputs refundAddress (Ada.lovelaceValueOf refundAmt) (ContextsV2.txInfoOutputs info)


-- | Creating a wrapper around littercoin validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapETValidator #-}
wrapETValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapETValidator params dat red ctx =
   check $ mkETValidator (PlutusV2.unsafeFromBuiltinData params) (PlutusV2.unsafeFromBuiltinData dat) (PlutusV2.unsafeFromBuiltinData red) (PlutusV2.unsafeFromBuiltinData ctx)


untypedETValidator :: BuiltinData -> PSU.V2.Validator
untypedETValidator params = PlutusV2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapETValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedETValidator :: BuiltinData -> PSU.V2.TypedValidator Typed.Any
typedETValidator params =
  ValidatorsV2.unsafeMkTypedValidator $ untypedETValidator params


etValidator :: BuiltinData -> PSU.V2.Validator
etValidator params = Typed.validatorScript $ typedETValidator params


etHash :: BuiltinData -> PSU.V2.ValidatorHash
etHash params = ValidatorsV2.validatorHash  $ typedETValidator params
