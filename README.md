# The Traceability Smart Contract

- [Smart Contract Design](#smart-contract-design)
- [Process Flows](#process-flows)
- [System Implementation](#system-implementation)
- [Setup to rebuild plutus scripts](#setting-up-to-re-build-the-plutus-scripts)

## Smart Contract Design
The traceability smart contract consists of a single smart contract where Ada is locked and unlocked.   The Ada is locked during the buying a product and paying with Ada through a web browser wallet.  The locking Ada transaction also includes a datum that contains key information about the order which is used during the smart contract validation.  The unlocking of Ada is a transaction that is executed by an admin shell script using the cardano-cli tool.  The admin only has the ability to execute the smart contract and cannot arbitrarily spend the Ada aside from what is defined in the smart contract.   The smart contract has parameterized values for the merchant, donor and refund address.   In addition, the smart contract also validates that the amount of the donation matches the amount defined in the smart contract.    

Below is the smart contract design for the traceability smart contract

![Traceability Smart Design](/images/smart-contract-design.png)



## Process Flows
The following Ada payment flow shows both the spend and refund smart contract execution scenarios.

![Ada Payment Flow](/images/ada-payment-flow.png)


## System Implementation
Below is a simplified view of the components deployed as part of the traceability smart contract launch

![System Component View](/images/system-components.png)



## Setting up to re-build the plutus scripts

Clone the repo using the following command
```
git clone https://github.com/lley154/traceability-smart-contract.git
cd traceability-smart-contract/
```


## Deploying the smart contract
### Update Deploy.hs
Update the correct pkh values for the wallets in src/V2/Traceability/Deploy.hs
```
-- Merchant Pkh
merchantPubKeyHashBS :: B.ByteString
merchantPubKeyHashBS = "a8376ad675ab3c86f945fd1f7c5773888269dced9b9368c7d7a113efd4495d6cb7a846ec2be6a23fe1991ef3c507cab3cdaba326d5263cf5"

-- Donor public key payment hash
donorPubKeyHashBS :: B.ByteString
donorPubKeyHashBS = "f485f3526ffd1569f3ace37e89317380297ec15dc09549356db1cd04"

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "c06bfbb7bb62004d21754f75a84249ad5527a585d576f70225e564c1"

-- Refund public key payment hash
refundPubKeyHashBS :: B.ByteString
refundPubKeyHashBS = "4d4e81a25ca2bac69a0d0f8990a2030ec72110e56001f4981f5713d2"
```


### Cabal+Nix build

Use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
nix-shell
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).


### Run cabal repl to generate the plutus scripts

```
[nix-shell:~/src/traceability-smart-contract]$ cabal repl
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - traceability-0.1.0.0 (lib) (ephemeral targets)
Preprocessing library for traceability-0.1.0.0..
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
...
[1 of 3] Compiling Traceability.V2.Types ( src/Traceability/V2/Types.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/Types.o )
[2 of 3] Compiling Traceability.V2.OnChain ( src/Traceability/V2/OnChain.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/OnChain.o )
[3 of 3] Compiling Traceability.V2.Deploy ( src/Traceability/V2/Deploy.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/Deploy.o )
Ok, three modules loaded.
Prelude Traceability.V2.Deploy> main
Prelude Traceability.V2.Deploy> :q
Leaving GHCi.

```
The new plutus scripts will be created in the traceability-smart-contract/deploy directory

```
[nix-shell:~/src/traceability-smart-contract]$ ls -l deploy/*
-rw-rw-r-- 1 lawrence lawrence   92 Nov 15 09:21 deploy/datum-earthtrust.json
-rw-rw-r-- 1 lawrence lawrence   62 Nov 15 09:21 deploy/earthtrust-validator.hash
-rw-rw-r-- 1 lawrence lawrence 7291 Nov 15 09:21 deploy/earthtrust-validator.plutus
-rw-rw-r-- 1 lawrence lawrence   29 Nov 15 09:21 deploy/redeemer-earthtrust-refund.json
-rw-rw-r-- 1 lawrence lawrence   29 Nov 15 09:21 deploy/redeemer-earthtrust-spend.json

```

1) Copy the resultant files to the data directory for your respective environment

```
[nix-shell:~/src/traceability-smart-contract]$ cp deploy/* scripts/preprod/data
```
2) Then run the init-tx.sh to load the smart contract onto the blockchain
cd scripts/cardano-cli/
./init-tx.sh preprod

3) Copy the address of the validator from the script output

```
...
/usr/local/bin/cardano-cli transaction build --babbage-era --cardano-mode --testnet-magic 1 --change-address addr_test1vzu6hnmgvageu2qyypy25yfqwg222tndt5eg3d6j68p8dqspgdxn7 --tx-in-collateral 0b90e315e1829bf4a9225de6a7fc238f33c12dca1c34ce2a9faa24eb8e56ab29#0 --tx-in fa51bf0804a8dcf1c0fceef354912a4308021584545fffb2bcc46d333529d1a9#0 --tx-out addr_test1wpv8838yxz5yu58jq50qh86sh34my4m3df7frn7xhj2ltnsgnv855+20000000 --tx-out-reference-script-file /home/lawrence/src/traceability-smart-contract/scripts/cardano-cli/preprod/data/earthtrust-validator.plutus --protocol-params-file /home/lawrence/src/traceability-smart-contract/work/pparms.json --out-file /home/lawrence/src/traceability-smart-contract/work/init-tx-alonzo.body
 submit --tx-file /home/lawrence/src/traceability-smart-contract/work/init-tx-alonzo.tx --testnet-magic 1
Transaction successfully submitted.
...
```

4) Update the .bashrc file with the validator script address
```
export NEXT_PUBLIC_EARTHTRUST_VAL_ADDR="addr_test1wpv8838yxz5yu58jq50qh86sh34my4m3df7frn7xhj2ltnsgnv855"
```

5) Query the address to find the UTXO of the validator script
```
lawrence@lawrence-iMac:~$ cardano-cli query utxo --address addr_test1wpv8838yxz5yu58jq50qh86sh34my4m3df7frn7xhj2ltnsgnv855 --cardano-mode --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
50d7cd3ed45beb91d9f58593ca988b7cdaa199ab860d50bd26fa598366c9aacc     1        20000000 lovelace + TxOutDatumNone
```

6) Update preprod/global-export-varaibles.sh with the correct validator refefence UTXO
```
export VAL_REF_SCRIPT=50d7cd3ed45beb91d9f58593ca988b7cdaa199ab860d50bd26fa598366c9aacc#1
```

Once funds have been locked at the smart contract address, the spend-tx.sh shell script can be run to split the order amount to the merchant and donor respectively.  Be sure that your source your ~/.bashrc so you have the correct NEXT_PUBLIC_SHOP and NEXT_PUBLIC_ACCESS_TOKEN variables correct.   The refund-tx.sh script is used if there was an error processing the order locked at the script address.  (eg invalid donor wallet address).













