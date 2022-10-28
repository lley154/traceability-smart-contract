{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.Types 
(
     MintPolicyRedeemer(..)
   , NFTMintPolicyParams(..)

)where

import              Data.Aeson                          (FromJSON, ToJSON)  
import              GHC.Generics                        (Generic)
import qualified    Ledger.Address as Address           (Address, PaymentPubKeyHash(..))
import qualified    Ledger.Value as Value               (TokenName(..), Value)
import qualified    Ledger.Tx as Tx                     (TxOutRef(..))
import              Playground.Contract as Playground   (ToSchema)
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { 
      mpPolarity                  :: !Bool    -- True = Mint, False = Burn
    , mpOrderId                   :: !Integer -- The order number 
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The NFT minting policy params passes parameters 
--   into the minting poicy which will make the NFT policy unique
data NFTMintPolicyParams = NFTMintPolicyParams
    { 
      nftVersion                     :: !Integer  
    , nftSplit                       :: !Integer
    , nftMerchantAddress             :: !Address.Address
    , nftDonorAddress                :: !Address.Address
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''NFTMintPolicyParams [('NFTMintPolicyParams,0)] 
PlutusTx.makeLift ''NFTMintPolicyParams
