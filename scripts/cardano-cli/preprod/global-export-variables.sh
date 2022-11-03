#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/var/snap/docker/common/var-lib-docker/volumes/cardano-ipc/_data/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/admin.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/admin.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/admin.hash
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export ADMIN_UTXO=029166d2594117b8768462e0981ea35442a403550ca39db8d2b5c14d1142dbb2#0
export ADMIN_COLLATERAL=c0d8386d6c888f369b731002e32785100a09bd53b9f03ad8f299eb7bf89445c7#0
export ADMIN_CHANGE_ADDR=addr_test1vzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqspgdxn7
export MERCHANT_ADDR=addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rqug2pds
export DONOR_ADDR=addr_test1vzetpfww4aaunft0ucvcrxugj8nt4lhltsktya0rx0uh48cqghjfg
export VAL_REF_SCRIPT=c4e996b158df519c49d4a0794cb00637d56a38fdea278bbfab89454925b9ea0e#1