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
      etCurSymbol
    , etPolicy
    , etTokenValue
    ) where

import           Traceability.V2.Types                          (ETMintPolicyParams(..), MintPolicyRedeemer(..))
import qualified Ledger.Ada as Ada                              (lovelaceValueOf)
import qualified Ledger.Address as Address                      (Address, pubKeyHashAddress)
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2       (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2 (mkUntypedMintingPolicy)
import qualified Ledger.Value as Value                          (flattenValue, singleton, TokenName(..), Value)
import qualified Plutus.V2.Ledger.Contexts as ContextsV2        (ScriptContext, scriptContextTxInfo, TxInfo(..), txInfoMint, 
                                                                txInfoOutputs, TxOut(..), txOutValue)
import qualified Plutus.V2.Ledger.Api as PlutusV2               (CurrencySymbol, MintingPolicy, 
                                                                 mkMintingPolicyScript)
import qualified PlutusTx                                       (applyCode, compile, liftCode)
import           PlutusTx.Prelude                               (Bool(..), divide, Integer, Maybe(..), otherwise, 
                                                                traceIfFalse, (&&), (==), ($), (-), (*))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

                            
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (ContextsV2.txOutAddress x == scriptAddr) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | mkNFTPolicy is the minting policy is for creating the order token NFT when
--   an order is submitted.
{-# INLINABLE mkETPolicy #-}
mkETPolicy :: ETMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkETPolicy params (MintPolicyRedeemer polarity adaAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount
                && traceIfFalse "NFTP2" checkMerchantOutput 
                && traceIfFalse "NFTP3" checkDonorOutput 
                
        False ->   False   -- no burning allowed

  where
    info :: ContextsV2.TxInfo
    info = ContextsV2.scriptContextTxInfo ctx

    tn :: Value.TokenName
    tn = etpTokenName params  

    split :: Integer
    split = etpSplit params
    
    merchantAddress :: Address.Address
    merchantAddress = Address.pubKeyHashAddress (etpMerchantPkh params) Nothing

    merchantAmount :: Value.Value
    merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

    donorAddress :: Address.Address
    donorAddress = Address.pubKeyHashAddress (etpDonorPkh params) Nothing

    donorAmount :: Value.Value
    donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)


    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (ContextsV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False
          
    -- | Check that both the split amount value is correct and at the correct
    --   address for the merchant     
    checkMerchantOutput :: Bool
    checkMerchantOutput = validOutputs merchantAddress merchantAmount (ContextsV2.txInfoOutputs info)

    -- | Check that both the split amount value is correct and at the correct
    --   address for the donor  
    checkDonorOutput :: Bool
    checkDonorOutput = validOutputs donorAddress donorAmount (ContextsV2.txInfoOutputs info)


-- | Wrap the minting policy using the boilerplate template haskell code
etPolicy :: ETMintPolicyParams -> PlutusV2.MintingPolicy
etPolicy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkETPolicy mp' 


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE etCurSymbol #-}
etCurSymbol :: ETMintPolicyParams -> PlutusV2.CurrencySymbol
etCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ etPolicy mpParams 


-- | Return the value of the nftToken
{-# INLINABLE etTokenValue #-}
etTokenValue :: PlutusV2.CurrencySymbol -> Value.TokenName -> Value.Value
etTokenValue cs' tn' = Value.singleton cs' tn' 1


