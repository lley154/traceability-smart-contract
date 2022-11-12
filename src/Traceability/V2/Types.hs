{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V2.Types 
(
  ETRedeemer(..)
, ETValidatorParams(..)

)where

import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Integer)
import qualified    Prelude as Haskell                  (Show)

-- | The earthtrust validator parameters that are used to hard code
--   key parameters into the earthtrust smart contract
data ETValidatorParams = ETValidatorParams
    { 
      etvVersion                 :: Integer  
    , etvSplit                   :: Integer
    , etvServiceFee              :: Integer
    , etvMerchantPkh             :: Address.PaymentPubKeyHash
    , etvDonorPkh                :: Address.PaymentPubKeyHash
    , etvAdminPkh                :: Address.PaymentPubKeyHash
    , etvRefundPkh               :: Address.PaymentPubKeyHash
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ETValidatorParams [('ETValidatorParams,0)] 
PlutusTx.makeLift ''ETValidatorParams

-- | The ETRedemeer used to indicate if the action is to spend or refund the
--   the Ada locked at the smart contract
data ETRedeemer = 
       Spend            -- spend earthtrust locked Ada and send to merchant and donor 
     | Refund           -- refund locked Ada to customer

    deriving Haskell.Show

PlutusTx.makeIsDataIndexed
  ''ETRedeemer
  [ ('Spend, 0),
    ('Refund, 1)
  ]
PlutusTx.makeLift ''ETRedeemer

