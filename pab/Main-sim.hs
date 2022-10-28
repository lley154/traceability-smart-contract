{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main
    ( main
    ) where
        
import           Traceability.OffChain       
import           Control.Monad                          (void)
import           Control.Monad.Freer                    (interpret)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Aeson                             (Result (..), fromJSON)
import           Data.Default                           (def)
import qualified Data.Monoid as Monoid
import           Ledger.Address                         (PaymentPubKeyHash)
import qualified Ledger.CardanoWallet as CW
import           Ledger.Value as Value                  (TokenName(..))
import           PabContract                            (Contracts(..))
import           Plutus.PAB.Effects.Contract.Builtin    (Builtin, BuiltinHandler(contractHandler), handleBuiltin)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import           Wallet.Emulator.Wallet                 (knownWallet)


userPkh :: PaymentPubKeyHash
userPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

merchantPkh :: PaymentPubKeyHash
merchantPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 2)

donorPkh :: PaymentPubKeyHash
donorPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 3)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        orderId = 123 :: Integer
        tp = TokenParams
            {   tpVersion = 1 :: Integer
            ,   tpSplit = 95 :: Integer
            ,   tpMerchantPkh = merchantPkh
            ,   tpDonorPkh = donorPkh
            }


    --setLocaleEncoding utf8
    Simulator.logString @(Builtin Contracts) "Starting PAB webserver on port 8080"
    shutdown <- PAB.Server.startServerDebug   

    Simulator.logString @(Builtin Contracts) "********* PAB server is running *********"
    Simulator.logString @(Builtin Contracts) "To start PAB simulation press return"
    void $ liftIO getLine
        
    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 1"
    h1 <- Simulator.activateContract w1 UseContract


    -- Mint the order token
    Simulator.logString @(Builtin Contracts) "Calling mintNFT endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "mintNFT" (orderId, tp)

    Simulator.waitNSlots 5    

    balances_nft <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_nft

    Simulator.logString @(Builtin Contracts) "Order token minted for wallet 1, press return to continue"
    void $ liftIO getLine


    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

