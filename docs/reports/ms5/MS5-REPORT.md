# 1000121 MLabs - Enhancing and Evolving the Plutus Simple Model (PSM) Test Library - Milestone 5 Report

_Prepared  by MLabs on November 22, 2024_\
_Ilia Rodionov, ilia@mlabs.city_

## Overview

Milestone five was all about making CLB accessible from non-Haskell environments.
Being writen in Haskell, CLB can be used as a lightweight library to run  Cardano
ledger. This is how we used CLB in the previous milestones with Atlas.

But this is not always a way to go. Many environments can't make use of
a library as it is. At MLabs we have two such project, CTL written in PureScript
and Tx-village written in Rust. We chose the former since is way more mature.

The idea was to wrap CLB emulator into a standalone process that implements some
Ouroboros mini-protocols and can mimic cardano-node. CTL uses cardano-testnet from
cardano-node repository to spin up a private network to run tests. So we made the
socket emulator compatible with cardano-testnet by supporting all CLI parameters
and just replaced a real nodes with emulated ones. The rest of the stack (Ogmios
and Kupo) are still in place - no chanhes are required.

This setting has an important distinctions from a regular cardano-testnet with
cardano-node - there is no network and consensus overhead. Which might be good
or bad depending on features required. It is quicker and  lighter, so if your
tests don't rely on mentioned things, you can safely use CLB to get some speed
gain in your tests.

To showcase the use of CLB with CTL, a bet-ref example from Atlas (particularly
its Aiken-based version) was ported into CTL testnet test-suite.

## Milestone Outputs

Outputs of the milestone can be found:

* PRs in [mlabs-haskell/clb](https://github.com/mlabs-haskell/clb) repository:
  * [Node socket emulator](https://github.com/mlabs-haskell/clb/pull/49)
  * [feat: mimic cardano-node CLI args + multiple fixes](https://github.com/mlabs-haskell/clb/pull/53)
  * [Last never-forking era, get chain points query](https://github.com/mlabs-haskell/clb/pull/56)

* PRs in [Plutonomicon/cardano-transaction-lib](https://github.com/Plutonomicon/cardano-transaction-lib) repository:
  * [Integration with CLB emulator and bet-ref example](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1655)

## Acceptance Criteria and Evidence

* Traces of the ported bet-ref run with CTL can be found
[here](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms5/bet-ref-ctl.out).
They witness that CLB node emulator is capable to mimic Cardano node
behavior for transaction submission and state querying
and that CTL can run a test-suite against the CLB node emulator.

* Updated CTL's documentation that covers the use of CLB with CTL can be found
in the PR [here](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1655/files#diff-807b1fd747bf984d301c342513212276a2462b41f08f0852e544bc0bd0e8262b).

* The milestone report - this document.
