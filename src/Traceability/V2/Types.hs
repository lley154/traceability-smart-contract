{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V2.Types 
(
     LockMintPolicyParams(..)
   , MintPolicyRedeemer(..)
   , NFTMintPolicyParams(..)

)where

import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    Ledger.Value as Value               (TokenName(..))
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { 
      mpPolarity                  :: Bool              -- True = Mint, False = Burn
    , mpOrderId                   :: Value.TokenName   -- The order number
    , mpAdaAmount                 :: Integer           -- The total amount of the order 
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The NFT minting policy params passes parameters 
--   into the minting poicy which will make the NFT policy unique
data NFTMintPolicyParams = NFTMintPolicyParams
    { 
      nftVersion                 :: Integer  
    , nftSplit                   :: Integer
    , nftMerchantPkh             :: Address.PaymentPubKeyHash
    , nftDonorPkh                :: Address.PaymentPubKeyHash
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''NFTMintPolicyParams [('NFTMintPolicyParams,0)] 
PlutusTx.makeLift ''NFTMintPolicyParams


-- | The Lock minting policy params passes parameters 
--   into the minting poicy which will make the lock minting policy unique
data LockMintPolicyParams = LockMintPolicyParams
    { 
      lmpOrderId                  :: Value.TokenName   -- The order number
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''LockMintPolicyParams [('LockMintPolicyParams,0)] 
PlutusTx.makeLift ''LockMintPolicyParams


