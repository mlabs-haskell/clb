# CLB

Welcome to the CLB! (Pronounced /klʌb/).

CLB is in its early development stage and the next generation of
[PSM library](https://github.com/mlabs-haskell/plutus-simple-model).

The undergoing development is funded by Catalyst Fund 10 [project]
(https://milestones.projectcatalyst.io/projects/1000121).
Milestone reports are available in [`docs/reports`]
(https://github.com/mlabs-haskell/clb/tree/master/docs/reports).

See `test/smoke/smoke-test.hs` for a usage example.

# Development

Currently, we use `input-output-hk/devx` shell with `cabal`:

```bash
$ cd clb
$ direnv allow
$ cabal run smoke-test
```

# mimic cardano-node args

 cabal run socket-emulator -- run --config /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/configuration.yaml --topology /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/topology.json --database-path /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/db --shelley-kes-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/kes.skey --shelley-vrf-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/vrf.skey --byron-delegation-certificate /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/byron-delegation.cert --byron-signing-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/byron-delegate.key --shelley-operational-certificate /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/opcert.cert --socket-path ./socket/pool2/sock --port 43159 --host-addr 127.0.0.1

 run
 --config /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/configuration.yaml
 --topology /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/topology.json
 --database-path /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/db
 --shelley-kes-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/kes.skey
 --shelley-vrf-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/vrf.skey
 --byron-delegation-certificate /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/byron-delegation.cert
 --byron-signing-key /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/byron-delegate.key
 --shelley-operational-certificate /tmp/nix-shell.KrrSIE/testnet-test-d0616bdcba37af74/pools-keys/pool2/opcert.cert
 --socket-path ./socket/pool2/sock
 --port 43159
 --host-addr 127.0.0.1

cardano-test command to run:

export CARDANO_NODE=/home/euonymos/src/mlabs/clb/dist-newstyle/build/x86_64-linux/ghc-9.6.6/socket-emulator-1.0.0/x/cardano-node-socket-emulator/build/cardano-node-socket-emulator/cardano-node-socket-emulator; cabal run cardano-testnet -- cardano --conway-era --testnet-magic 764824073 --nodeLoggingFormat text

# TODO

use network magic from CLI args