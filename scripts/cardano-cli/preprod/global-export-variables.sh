#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/lawrence/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export CARDANO_NODE_SOCKET_PATH=/home/lawrence/.cardano-preprod-node/db/node.socket
export TESTNET_MAGIC=1
export ADMIN_VKEY=/home/lawrence/.local/keys/testnet/admin/admin.vkey
export ADMIN_SKEY=/home/lawrence/.local/keys/testnet/admin/admin.skey
export ADMIN_PKH=/home/lawrence/.local/keys/testnet/admin/admin.hash
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export MERCHANT_ADDR=addr_test1vq7k907l7e59t52skm8e0ezsnmmc7h4xy30kg2klwc5n8rqug2pds
export DONOR_ADDR=addr_test1vzetpfww4aaunft0ucvcrxugj8nt4lhltsktya0rx0uh48cqghjfg
export REFUND_ADDR=addr_test1vzs24vjh8salzqt5pahgvr34ewfwagxaxr5pz7eswugzn2gmw4f5w
export VAL_REF_SCRIPT=85ac254e30cd8326742085b4bce562888423aa7ca81f7e37f490d3b24dcdfe3d#1
export SPLIT=99
export BLOCKFROST_API_KEY=preprodxg6GaNVZoHWUfQd7HQcgUg8epWhE1aMi
export BLOCKFROST_API=https://cardano-preprod.blockfrost.io/api/v0
export NEXT_PUBLIC_SHOP="https://test1-2803.myshopify.com/"            # Your test store URL
export NEXT_PUBLIC_ACCESS_TOKEN="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"

