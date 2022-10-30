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

module Traceability.V1.OnChain 
    (
      minAda
    , nftCurSymbol
    , nftPolicy
    , nftTokenValue
    , typedLockTokenValidator
    , lockTokenValidator
    ) where

import           Traceability.V1.Types              (LockTokenValParams(..), NFTMintPolicyParams(..), 
                                                     MintPolicyRedeemer(..))
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (Address, pubKeyHashAddress)
import qualified Ledger.Scripts as Scripts          (mkValidatorScript, Validator)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (MintingPolicy, TypedValidator, validatorScript, 
                                                     wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData)
import qualified PlutusTx                           (applyCode, compile, liftCode)
import           PlutusTx.Prelude                   (Bool(..), BuiltinData, check,  
                                                     divide, Integer, Maybe(..), otherwise, 
                                                     traceIfFalse, (&&), (==), ($), (-), (*))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

                            
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [Tx.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (Tx.txOutAddress x == scriptAddr) && (Tx.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | mkNFTPolicy is the minting policy is for creating the order token NFT when
--   an order is submitted.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity orderId adaAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount
                && traceIfFalse "NFTP2" checkMerchantOutput 
                && traceIfFalse "NFTP3" checkDonorOutput 
                
        False ->   False   -- no burning allowed

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  

    split :: Integer
    split = nftSplit params

    merchantAddress :: Address.Address
    merchantAddress = Address.pubKeyHashAddress (nftMerchantPkh params) Nothing

    merchantAmount :: Value.Value
    merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

    donorAddress :: Address.Address
    donorAddress = Address.pubKeyHashAddress (nftDonorPkh params) Nothing

    donorAmount :: Value.Value
    donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)


    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == orderId && amt == 1
        _               -> False
          
    -- | Check that both the split amount value is correct and at the correct
    --   address for the merchant     
    checkMerchantOutput :: Bool
    checkMerchantOutput = validOutputs merchantAddress merchantAmount (txInfoOutputs info)

    -- | Check that both the split amount value is correct and at the correct
    --   address for the donor  
    checkDonorOutput :: Bool
    checkDonorOutput = validOutputs donorAddress donorAmount (txInfoOutputs info)


-- | Wrap the minting policy using the boilerplate template haskell code
nftPolicy :: NFTMintPolicyParams -> TScripts.MintingPolicy
nftPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> TScripts.wrapMintingPolicy $ mkNFTPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE nftCurSymbol #-}
nftCurSymbol :: NFTMintPolicyParams -> Value.CurrencySymbol
nftCurSymbol mpParams = scriptCurrencySymbol $ nftPolicy mpParams 

-- | Return the value of the nftToken
{-# INLINABLE nftTokenValue #-}
nftTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1


-- | Always Fail validator to lock order token
{-# INLINABLE mkLockTokenValidator #-}
mkLockTokenValidator :: LockTokenValParams -> BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkLockTokenValidator _ _ _ _ = False


-- | Creating a wrapper around traceability validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapLockTokenValidator #-}
wrapLockTokenValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapLockTokenValidator params dat red ctx =
   check $ mkLockTokenValidator (unsafeFromBuiltinData params) (unsafeFromBuiltinData dat) (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx)


untypedLockTokenValidator :: BuiltinData -> Scripts.Validator
untypedLockTokenValidator params = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapLockTokenValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedLockTokenValidator :: BuiltinData -> TScripts.TypedValidator TypeUtils.Any
typedLockTokenValidator params =
  Validators.unsafeMkTypedValidator $ untypedLockTokenValidator params


lockTokenValidator :: BuiltinData -> Scripts.Validator
lockTokenValidator params = TScripts.validatorScript $ typedLockTokenValidator params


