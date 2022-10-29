{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Traceability.V1.OffChain    
    (   
        TokenSchema
    ,   useEndpoint
    ,   TokenParams (..)
    ,   RedeemerParams (..)
    ) where

import           Traceability.V1.OnChain            (intToBBS, nftCurSymbol, nftPolicy, typedLockTokenValidator, 
                                                     lockTokenValidator)
import           Traceability.V1.Types              (LockTokenValParams(..), NFTMintPolicyParams(..), MintPolicyRedeemer(..))
import           Control.Lens                       (review)
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (keys)
import qualified Data.Text as T                     (Text)
import           GHC.Generics                       (Generic)
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mintingPolicy, mustMintValueWithRedeemer, 
                                                     mustPayToPubKey, mustPayToTheScript, 
                                                     mustSpendPubKeyOutput, otherScript, 
                                                     typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Redeemer(..))
import           Ledger.Value as Value              (singleton, TokenName(..))
import           Playground.Contract as Playground  (ToSchema)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), awaitPromise, 
                                                     Contract, Endpoint, endpoint, handleError, logError, 
                                                     logInfo, mapError, utxosAt)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           PlutusTx                           (toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), Integer, Maybe (..), ($), divide, 
                                                     sha2_256, (-), (++), (*))
import qualified Prelude as Haskell                 (Semigroup ((<>)), Show (..), String)


-- | TokenParams are parameters that are passed to the endpoints
data TokenParams = TokenParams
    { 
      tpVersion                     :: !Integer  
    , tpSplit                       :: !Integer
    , tpMerchantPkh                 :: !Address.PaymentPubKeyHash
    , tpDonorPkh                    :: !Address.PaymentPubKeyHash
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


data RedeemerParams = RedeemerParams
    { 
      rpPolarity                  :: !Bool    -- True = Mint, False = Burn
    , rpOrderId                   :: !Integer -- The order number
    , rpAdaAmount                 :: !Integer -- The total amount of the order
    , rpSplit                     :: !Integer -- used for testing 
    , rpMerchantPkh               :: !Address.PaymentPubKeyHash -- used for testing 
    , rpDonorPkh                  :: !Address.PaymentPubKeyHash -- used for testing 
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


-- | mintNFT mints the order token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintNFTToken :: RedeemerParams -> TokenParams -> Contract.Contract () TokenSchema T.Text ()
mintNFTToken rp tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let orderIdHash = sha2_256 $ intToBBS $ rpOrderId rp
                tn = Value.TokenName $ orderIdHash
                merchSplit = (rpAdaAmount rp) * (rpSplit rp)
                donorSplit = (rpAdaAmount rp) * (100 - (tpSplit tp))
                merchAmount = divide merchSplit 100
                donorAmount = divide donorSplit 100
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpOrderId = rpOrderId rp
                     ,  mpAdaAmount = rpAdaAmount rp
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftVersion = tpVersion tp
                    ,   nftSplit = tpSplit tp
                    ,   nftMerchantPkh = tpMerchantPkh tp
                    ,   nftDonorPkh = tpDonorPkh tp
                    }
                vParams = LockTokenValParams
                    {   
                        ltvOrderId = rpOrderId rp
                    }
                dat = PlutusTx.toBuiltinData ()
  
            let nftVal  = Value.singleton (nftCurSymbol mintParams) tn 1
                lookups = Constraints.typedValidatorLookups (typedLockTokenValidator $ PlutusTx.toBuiltinData vParams) Haskell.<> 
                          Constraints.otherScript (lockTokenValidator $ PlutusTx.toBuiltinData vParams) Haskell.<> 
                          Constraints.mintingPolicy (nftPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustPayToTheScript dat nftVal Haskell.<> 
                          Constraints.mustMintValueWithRedeemer red nftVal Haskell.<> 
                          Constraints.mustPayToPubKey (rpMerchantPkh rp) (Ada.lovelaceValueOf merchAmount) Haskell.<> 
                          Constraints.mustPayToPubKey (rpDonorPkh rp) (Ada.lovelaceValueOf donorAmount) Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref

            utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            Request.submitTxConfirmed adjustedUtx
            Contract.logInfo $ "mintNFT: tx submitted successfully= " ++ Haskell.show adjustedUtx


-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintNFT" (RedeemerParams, TokenParams)

useEndpoint :: Contract.Contract () TokenSchema T.Text ()
useEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"mintNFT" $ \(rp, tp) -> mintNFTToken rp tp


