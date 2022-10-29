{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V1.Types 
(
     MintPolicyRedeemer(..)
   , NFTMintPolicyParams(..)
   , LockTokenValParams(..)

)where

import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), BuiltinByteString, Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { 
      mpPolarity                  :: !Bool              -- True = Mint, False = Burn
    , mpOrderId                   :: !BuiltinByteString -- The order number
    , mpAdaAmount                 :: !Integer           -- The total amount of the order 
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The NFT minting policy params passes parameters 
--   into the minting poicy which will make the NFT policy unique
data NFTMintPolicyParams = NFTMintPolicyParams
    { 
      nftVersion                 :: !Integer  
    , nftSplit                   :: !Integer
    , nftMerchantPkh             :: !Address.PaymentPubKeyHash
    , nftDonorPkh                :: !Address.PaymentPubKeyHash
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''NFTMintPolicyParams [('NFTMintPolicyParams,0)] 
PlutusTx.makeLift ''NFTMintPolicyParams

-- | ValParams is used to pass the admin pkh, NFT & Littercoin token names as a parameter to the 
--   littercoin validator script
data LockTokenValParams = LockTokenValParams
    {   
      ltvOrderId                 :: !BuiltinByteString
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''LockTokenValParams [('LockTokenValParams,0)] 
PlutusTx.makeLift ''LockTokenValParams
