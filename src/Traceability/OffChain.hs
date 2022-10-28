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
import qualified Data.Map as Map                    (singleton, toList, keys)
import           Data.Monoid                        (Last (..))
import           Data.Text as T                     (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, mapError, select, submitTxConstraintsWith, tell, throwError, type (.\/), utxosAt)
import           PlutusTx                           (fromBuiltinData, toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe (..), ($))
import           Ledger                             (getCardanoTxId)
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (Address(..), PaymentPubKeyHash(..), pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, mustPayToPubKey, mustPayToTheScript, mustSpendPubKeyOutput, mustSpendScriptOutput, otherScript, typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Datum(..), Redeemer(..))
import qualified Ledger.Tx as Tx                    (ChainIndexTxOut (_ciTxOutValue,_ciTxOutDatum), TxOutRef(..))
import           Ledger.TxId as TxId                (TxId(..))  
import           Ledger.Value as Value              (CurrencySymbol, singleton, split, TokenName(..), valueOf)
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           Plutus.Contract.Wallet as Wallet   (getUnspentOutput)
import           PlutusPrelude                      (void)
import           PlutusTx.Prelude                   (divide, sha2_256, (+), (-), (++), (==), (*))
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
mintNFTToken :: Integer -> TokenParams -> Contract.Contract () TokenSchema Text ()
mintNFTToken orderId tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName $ sha2_256 $ intToBBS orderId
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpOrderId = orderId 
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftVersion = tpVersion tp
                    ,   nftSplit = tpSplit tp
                    ,   nftMerchantAddress = pubKeyHashAddress (tpMerchantPkh tp) Nothing
                    ,   nftDonorAddress = pubKeyHashAddress (tpDonorPkh tp) Nothing
                    }

            let val     = Value.singleton (nftCurSymbol mintParams) tn 1
                lookups = Constraints.mintingPolicy (nftPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintNFT: Forged %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "mintNFT: Token params %s" (Haskell.show mintParams)



-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintNFT" (Integer, TokenParams)

useEndpoint :: Contract.Contract () TokenSchema Text ()
useEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"mintNFT" $ \(id, tp) -> mintNFTToken id tp


{--
-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
useEndpoint :: Contract.Contract () TokenSchema Text ()
useEndpoint = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise mintNFT 
      
    where
        mintNFT = Contract.endpoint @"mintNFT" $ \(id, tp) -> mintNFT id tp 

--}


