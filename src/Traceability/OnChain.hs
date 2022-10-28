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

module Traceability.OnChain 
    (
      intToBBS
    , minAda
    , nftCurSymbol
    , nftPolicy
    , nftTokenValue
    ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Traceability.Types                 (NFTMintPolicyParams(..), MintPolicyRedeemer(..))
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..),  txSignedBy, TxId(getTxId ))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (Address, PaymentPubKeyHash(..))
import qualified Ledger.Contexts as Contexts        (getContinuingOutputs, ownCurrencySymbol, scriptCurrencySymbol, spendsOutput, TxOut)
import qualified Ledger.Scripts as Scripts          (Datum(..), DatumHash, mkMintingPolicyScript, mkValidatorScript, Script, Validator, ValidatorHash, validatorHash)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..), TxOutRef(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (MintingPolicy, TypedValidator, validatorScript, validatorHash, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData, unValidatorScript)
import qualified PlutusTx                           (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, BuiltinData, check, consByteString, divide, emptyByteString, find, Integer, Maybe(..), negate, otherwise, snd, sha2_256, traceIfFalse, traceError, (&&), (==), ($), (<=), (>=), (<>), (<$>), (-), (*), (+))
import qualified Prelude as Haskell                 (Show)

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

-- | Create a BuitinByteString from an Integer
{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS y = consByteString (y + 48) emptyByteString -- 48 is ASCII code for '0'


-- | Check that the NFT value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [Contexts.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | Tx.txOutValue x == txVal = True
    | otherwise = validOutputs txVal xs
                             

-- | mkNFTPolicy is the minting policy is for creating the order token NFT when
--   an order is submitted.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity orderId) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount 
                
        False ->   False   -- no burning allowed

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  

    tn :: Value.TokenName
    tn = Value.TokenName $ sha2_256 $ intToBBS orderId

    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False
          

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


{-# INLINABLE nftTokenValue #-}
nftTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1

