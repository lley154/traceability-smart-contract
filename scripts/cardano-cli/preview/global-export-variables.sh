#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/Downloads/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/.cardano-testnet-node/db/node.socket
export TESTNET_MAGIC=2
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/admin.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/admin.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/admin.hash
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export ADMIN_UTXO=10c6afcadf42e165bb1ec2c5b52aecdd251c81c37b20f5a5cdd2a1ee5211ffaa#0
export ADMIN_COLLATERAL=a53a467a299f0cf664a535e78257b38051b1e9aa7d6e7a98020e6052b556f57a#0
export ADMIN_CHANGE_ADDR=addr_test1vzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqspgdxn7
