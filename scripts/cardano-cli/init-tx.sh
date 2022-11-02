#!/usr/bin/env bash

##############################################################
# You must do these steps first before running this script
##############################################################
#
# Step 1.   Find the admin UTXO you will use
# Step 2.   Update src/Traceability/V2/Deploy.hs if there has been any 
#           changes in the merchant and donor wallets, if the 
#           split has changed and any version number changes.
# Step 3.   nix-shell, cabal repl, main
# Step 4.   Copy deploy/* scripts/cardano-cli/[devnet|preview|preprod|mainnet]/data
# Step 5.   Update scripts/cardano-cli/[devnet|preview|preprod|mainnet]/global-export-variables.sh
#           with the UTXO to be used for admin collateral
##############################################################


# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "init-tx.sh:  Invalid script arguments"
    echo "Usage: init-tx.sh [devnet|preview|preprod|mainnet]"
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/global-export-variables.sh

if [ "$ENV" == "mainnet" ];
then
    network="--mainnet"
else
    network="--testnet-magic $TESTNET_MAGIC"
fi

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"
ls -al "$CARDANO_NODE_SOCKET_PATH"

mkdir -p $WORK
mkdir -p $WORK-backup
rm -f $WORK/*
rm -f $WORK-backup/*

# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
token_mint_script="$BASE/scripts/cardano-cli/$ENV/data/token-minting-policy.plutus"
token_mint_script_addr=$($CARDANO_CLI address build --payment-script-file "$token_mint_script" $network)

echo "starting traceability init script"

################################################################
# Mint the threadtoken and attach it to the littercoin contract
################################################################

admin_utxo_in=$ADMIN_UTXO
admin_utxo_addr=$ADMIN_CHANGE_ADDR


# Step 2: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-in "$admin_utxo_in" \
  --tx-out "$token_mint_script_addr+$MIN_ADA_OUTPUT_TX_REF" \
  --tx-out-reference-script-file "$token_mint_script" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/init-tx-alonzo.body


echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/init-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/init-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/init-tx-alonzo.tx $network
