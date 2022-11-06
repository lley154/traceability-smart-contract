#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export BECH32=/usr/local/bin/bech32
export CARDANO_NODE_SOCKET_PATH=/var/snap/docker/common/var-lib-docker/volumes/cardano-ipc/_data/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/admin.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/admin.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/admin.hash
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export MERCHANT_ADDR=addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rqug2pds
export DONOR_ADDR=addr_test1vzetpfww4aaunft0ucvcrxugj8nt4lhltsktya0rx0uh48cqghjfg
export VAL_REF_SCRIPT=087e8cfaeaee4d5eb0cc90187229a899d68ca282b9dd2eea61612b00c0c0a522#1
export SPLIT=95
export BLOCKFROST_API_KEY=preprodxg6GaNVZoHWUfQd7HQcgUg8epWhE1aMi
export BLOCKFROST_API=https://cardano-preprod.blockfrost.io/api/v0