{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Traceability.OffChain    
    (   
        TokenSchema
    ,   useEndpoint
    ,   TokenParams (..)
    ) where

import           Traceability.OnChain               (intToBBS, nftCurSymbol, nftPolicy, nftTokenValue)
import           Traceability.Types                 (NFTMintPolicyParams(..), MintPolicyRedeemer(..))
import           Control.Lens                       (review)
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS              (append, ByteString)
import qualified Data.Map as Map                    (singleton, toList, keys)
import           Data.Monoid                        (Last (..))
import qualified Data.Text as T                     (unpack, Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, mapError, select, submitTxConstraintsWith, tell, throwError, type (.\/), utxosAt)
import           PlutusTx                           (fromBuiltinData, toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe (..), ($))
import           Ledger                             (getCardanoTxId, PubKeyHash(..), ValidatorHash(..))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (Address(..), PaymentPubKeyHash(..), pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, mustPayToPubKey, mustPayToTheScript, mustPayToOtherScript, mustSpendPubKeyOutput, mustSpendScriptOutput, otherScript, typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Datum(..), Redeemer(..))
import qualified Ledger.Tx as Tx                    (ChainIndexTxOut (_ciTxOutValue,_ciTxOutDatum), TxOutRef(..))
import           Ledger.TxId as TxId                (TxId(..))  
import           Ledger.Value as Value              (CurrencySymbol, singleton, split, TokenName(..), valueOf)
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           Plutus.Contract.Wallet as Wallet   (getUnspentOutput)
import           PlutusPrelude                      (void)
import           PlutusTx.Prelude                   (abs, BuiltinString, divide, encodeUtf8, fromBuiltin, indexByteString, lengthOfByteString, otherwise, sha2_256, quotRem, sliceByteString, toBuiltin, zero, (<), (<>), (.), (+), (-), (++), (==), (*))
import qualified Prelude as Haskell                 (Either(..), return, Semigroup ((<>)), Show (..), String)
import           Text.Printf                        (printf)


-- | TokenParams are parameters that are passed to the endpoints
data TokenParams = TokenParams
    { 
      tpVersion                     :: !Integer  
    , tpSplit                       :: !Integer
    , tpMerchantPkh                 :: !Address.PaymentPubKeyHash
    , tpDonorPkh                    :: !Address.PaymentPubKeyHash
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


-- | mintNFT mints the order token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintNFTToken :: Integer -> TokenParams -> Contract.Contract () TokenSchema T.Text ()
mintNFTToken orderId tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let orderIdHash = sha2_256 $ intToBBS orderId
                tn = Value.TokenName $ orderIdHash
                --orderIdAddress = scriptHashAddress vHash 
                --(_, nftVal) = Value.split(nftTokenValue nftCurSymbol tn)
                milkCost = 100000000 :: Integer
                merchSplit = milkCost * (tpSplit tp)
                donorSplit = milkCost * (100 - (tpSplit tp))
                merchAmount = divide merchSplit 100 :: Integer
                donorAmount = divide donorSplit 100 :: Integer
 
                orderIdHashHex = encodeHex orderIdHash  -- returns BuiltinString
                orderIdHashHexString = T.unpack $ fromBuiltin orderIdHashHex -- returns String
                orderIdHashHextByteString = strToBS orderIdHashHexString -- returns ByteString
   
                orderPk = PubKeyHash $ toBuiltin orderIdHashHextByteString  -- need BuiltinByteString
                orderPkh = PaymentPubKeyHash orderPk
                orderAddress = Address.pubKeyHashAddress orderPkh Nothing
                ownAddress = Address.pubKeyHashAddress ownPkh Nothing 

                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpOrderId = orderId 
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftVersion = tpVersion tp
                    ,   nftSplit = tpSplit tp
                    ,   nftMerchantPkh = tpMerchantPkh tp
                    ,   nftDonorPkh = tpDonorPkh tp
                    }
                dat = Datum $ toBuiltinData ()

            Contract.logInfo @Haskell.String $ printf "orderIdHash %s" (Haskell.show orderIdHash)
            Contract.logInfo @Haskell.String $ printf "orderIdHashHex %s" (Haskell.show orderIdHashHex)
            Contract.logInfo @Haskell.String $ printf "orderPkh %s" (Haskell.show orderPkh)
            Contract.logInfo @Haskell.String $ printf "ownPkh %s" (Haskell.show ownPkh)
            Contract.logInfo @Haskell.String $ printf "orderAddress %s" (Haskell.show orderAddress)
            Contract.logInfo @Haskell.String $ printf "ownAddress %s" (Haskell.show ownAddress)

            let nftVal  = Value.singleton (nftCurSymbol mintParams) tn 1
                lookups = Constraints.mintingPolicy (nftPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red nftVal Haskell.<> 
                          Constraints.mustPayToPubKey (tpMerchantPkh tp) (Ada.lovelaceValueOf merchAmount) Haskell.<> 
                          Constraints.mustPayToPubKey (tpDonorPkh tp) (Ada.lovelaceValueOf donorAmount) Haskell.<> 
                          Constraints.mustPayToPubKey (orderPkh) nftVal Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintNFT: Forged %s" (Haskell.show nftVal)
            Contract.logInfo @Haskell.String $ printf "mintNFT: Token params %s" (Haskell.show mintParams)



-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintNFT" (Integer, TokenParams)

useEndpoint :: Contract.Contract () TokenSchema T.Text ()
useEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"mintNFT" $ \(id, tp) -> mintNFTToken id tp


-- | Slow digit-by-digit hex to string converter
intToHexString :: Integer -> BuiltinString
intToHexString i
  | i < zero = "-" <> (go . abs $ i)
  | i == zero = "0"
  | otherwise = go i
  where
    go :: Integer -> BuiltinString
    go arg =
      let (q, r) = arg `quotRem` 16
       in if q == zero
            then hexDigitToString r
            else intToHexString q <> hexDigitToString r

-- | We render bytestrings as their individual code points
encodeHex :: BuiltinByteString -> BuiltinString
encodeHex bbs =
  let len = lengthOfByteString bbs
   in if len == zero
        then ""
        else
          let byte = indexByteString bbs zero
           in "" <> intToHexString byte <> (encodeHex . sliceByteString 1 (len - 1) $ bbs)


-- | This is unsafe, but only ever called internally.
hexDigitToString :: Integer -> BuiltinString
hexDigitToString i
  | i == 0 = "0"
  | i == 1 = "1"
  | i == 2 = "2"
  | i == 3 = "3"
  | i == 4 = "4"
  | i == 5 = "5"
  | i == 6 = "6"
  | i == 7 = "7"
  | i == 8 = "8"
  | i == 9 = "9"
  | i == 10 = "a" 
  | i == 11 = "b" 
  | i == 12 = "c" 
  | i == 13 = "d"
  | i == 14 = "e"
  | otherwise = "f"


strToBS :: Haskell.String -> BS.ByteString
strToBS = C.pack 