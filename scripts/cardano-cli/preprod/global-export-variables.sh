#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/src/traceability-smart-contract/node.socket
#export CARDANO_NODE_SOCKET_PATH=/var/snap/docker/common/var-lib-docker/volumes/cardano-ipc/_data/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/key.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/key.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/key.pkh
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export MIN_REF_ADA=30000000
export MERCHANT_ADDR=addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rqug2pds
export DONOR_ADDR=addr_test1vzetpfww4aaunft0ucvcrxugj8nt4lhltsktya0rx0uh48cqghjfg
export REFUND_ADDR=addr_test1vq5s7k4kwqz4rrfe8mm9jz9tpm7c5u93yfwwsaw708yxs5sm70qjg
export VAL_REF_SCRIPT=6ba4eb0d742d9db3787798b143f803be104cc2f9fc444547f46778114e8405d9#1
export SPLIT=75
