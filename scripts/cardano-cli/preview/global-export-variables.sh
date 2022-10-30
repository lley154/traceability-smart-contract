#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/traceability
export CARDANO_CLI=/home/lawrence/.local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=$BASE/node.socket
export TESTNET_MAGIC=2
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/key.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/key.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/key.hash
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export ADMIN_UTXO=817a13fcb42769be322c9f1063cb9783aaacdb31f4e295d5bb97ff17e250d7c7#0
export ADMIN_COLLATERAL=e722b61f665a486baf81335774ca1f213bbb68b8905e7441417d5610e4a24a9a#0
export ADMIN_CHANGE_ADDR=addr_test1vzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqspgdxn7



