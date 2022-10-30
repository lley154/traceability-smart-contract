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
    ) where

import           Traceability.V1.Types                          (NFTMintPolicyParams(..), MintPolicyRedeemer(..))
--import           Ledger                                       (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
--                                                              TxInfo(..))
import qualified Ledger.Ada as Ada                              (lovelaceValueOf)
import qualified Ledger.Address as Address                      (Address, pubKeyHashAddress)
--import qualified Ledger.Scripts as Scripts                      (mkValidatorScript, Validator)                                                  
--import qualified Ledger.Tx as Tx                                (TxOut(..))
--import qualified Ledger.Typed.Scripts.Validators as Validators  (unsafeMkTypedValidator)
--import qualified Ledger.Typed.TypeUtils as TypeUtils            (Any)

import qualified Plutus.Script.Utils.Typed as Typed             (Any)
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2       (scriptCurrencySymbol)
--import qualified Ledger.Typed.Scripts as TScripts               (MintingPolicy, wrapMintingPolicy)
import qualified Ledger.Value as Value                          (flattenValue, singleton, Value)
import qualified Plutus.V2.Ledger.Contexts as ContextsV2        (ScriptContext, scriptContext, TxInfo(..), txInfoMint, 
                                                                txInfoOutputs, TxOut(..), txOutValue)
import qualified Plutus.V2.Ledger.Api as PlutusV2               (CurrencySymbol,  
                                                                MintingPolicy, mkMintingPolicyScript, wrapMintingPolicy,
                                                                TokenName(..))
import qualified PlutusTx                                       (applyCode, compile, liftCode)
import           PlutusTx.Prelude                               (Bool(..), BuiltinData, check,  
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
validOutputs :: Address.Address -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (ContextsV2.txOutAddress x == scriptAddr) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | mkNFTPolicy is the minting policy is for creating the order token NFT when
--   an order is submitted.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ContextV2.ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity orderId adaAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount
                && traceIfFalse "NFTP2" checkMerchantOutput 
                && traceIfFalse "NFTP3" checkDonorOutput 
                
        False ->   False   -- no burning allowed

  where
    info :: ContextV2.TxInfo
    info = ContextV2.scriptContextTxInfo ctx  

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
    checkMintedAmount = case PlutusV2.flattenValue (ContextV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == orderId && amt == 1
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
nftPolicy :: NFTMintPolicyParams -> PlutusV2.MintingPolicy
nftPolicy mpParams = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> PlutusV2.wrapMintingPolicy $ mkNFTPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE nftCurSymbol #-}
nftCurSymbol :: NFTMintPolicyParams -> PlutusV2.CurrencySymbol
nftCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ nftPolicy mpParams 

-- | Return the value of the nftToken
{-# INLINABLE nftTokenValue #-}
nftTokenValue :: PlutusV2.CurrencySymbol -> PlutusV2.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1

