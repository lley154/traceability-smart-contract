#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "add-ada-tx.sh:  Invalid script arguments"
    echo "Usage: add-ada-tx.sh [devnet|preview|preprod|mainnet]"
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
validator_script="$BASE/scripts/cardano-cli/$ENV/data/earthtrust-validator.plutus"
validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$validator_script" $network)
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-earthtrust.json"
admin_pkh=$(cat $ADMIN_PKH)

ada_amount=100000000

################################################################
# Spend the earthtrust UTXO
################################################################

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for spending of the token
# and one uxto for collateral
if [ "$ENV" == "devnet" ];
then
    admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
    $CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json
    cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
    readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
    admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')
else
    admin_utxo_in=$ADMIN_UTXO
    admin_utxo_addr=$ADMIN_CHANGE_ADDR
fi


# Step 2: Get the earthtrust smart contract utxos
$CARDANO_CLI query utxo --address $validator_script_addr $network --out-file $WORK/validator-utxo.json

# Pull the utxo with the thread token in it
#lc_validator_utxo_tx_in=$(jq -r 'to_entries[] 
#| select(.value.value."'$thread_token_mph'"."'$thread_token_name'") 
#| .key' $WORK/lc-validator-utxo.json)

validator_utxo_tx_in=c4e996b158df519c49d4a0794cb00637d56a38fdea278bbfab89454925b9ea0e#0 


# Pull the datum with the thread token attached to the utxo
#lc_validator_datum_in=$(jq -r 'to_entries[] 
#| select(.value.value."'$thread_token_mph'"."'$thread_token_name'") 
#| .value.inlineDatum' $WORK/lc-validator-utxo.json)

#validator_datum_in={"constructor":0,"fields":[{"int":100000000}]}


# Save the inline datum to disk
#echo -n "$lc_validator_datum_in" | jq 'to_entries[] | .value.inlineDatum' > $WORK/lc-datum-in.json
#echo -n "$lc_validator_datum_in" > $WORK/lc-datum-in.json
cp $BASE/scripts/cardano-cli/$ENV/data/datum-in.json $WORK/datum-in.json


# get the total order amount in Ada
total_ada=$(jq -r '.fields[0].int' $WORK/datum-in.json)
merchant_ada=$((total_ada * 95 / 100))
donor_ada=$((total_ada * 5 / 100))


# Update the littercoin datum accordingly
#cat $WORK/lc-datum-in.json | \
#jq -c '
#  .fields[0].int   |= '$new_total_ada'' > $WORK/lc-datum-out.json


# Upate the redeemer with the amount of add being added
#cat $redeemer_file_path | \
#jq -c '
#  .fields[0].int          |= '$ada_amount'' > $WORK/redeemer-add-ada.json


# Step 3: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$ADMIN_COLLATERAL" \
  --tx-in "$admin_utxo_in" \
  --tx-in "$validator_utxo_tx_in" \
  --spending-tx-in-reference "$VAL_REF_SCRIPT" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$redeemer_file_path" \
  --tx-out "$MERCHANT_ADDR+$merchant_ada" \
  --tx-out "$DONOR_ADDR+$donor_ada" \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/add-ada-tx-alonzo.body

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/add-ada-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/add-ada-tx-alonzo.tx

echo "tx has been signed"

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/add-ada-tx-alonzo.tx $network



