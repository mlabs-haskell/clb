# 1000121 MLabs - Enhancing and Evolving the Plutus Simple Model (PSM) Test Library - Milestone 2 Report

_Prepared  by MLabs on August 25, 2024_\
_Ilia Rodionov, ilia@mlabs.city_

## Overview

In this milestone we made required changes to Atlas PAB:
* Replaces PSM with CLB.
* Unified testign by allowing running same tests against either an emulator (CLB)
or a private cardano network.

Before this milestone Atlas used a semi-manual way to spin up a private network
based on WoofPool's `cardano-private-testnet-setup` repository.
This couldn't contibute much to good devs experience
and was definitely problematic to use within CI environments,
so we considered alternative options:
* Plutip was a default way to spin up a private network at MLabs for a while,
but its maintenance was complicated due to the fact it used some modified code
from `cardano-wallet` repository which changed significantly over last months.
* `cardano-testnet` from `cardano-node` was another alternative we finally decided to use.
Despite its unnecessary use of `hedgehog` library to run the cluster
(see [PR](https://github.com/IntersectMBO/cardano-node/issues/5848) for details)
which complicates things this is the best way to go for the time being.

Having both ledger backends - CLB as an emulator
and `cardano-testnet` as a private test network
we proceeded with unifying the way tests can be run against different backends
by abstracting the API with a set of existing and new typeclass within
Atlas (see `GYTxGameMonad` class).

A `bet-ref` example then was adopted
and can be run from `test-unified` folder in Atlas repository.

We updated Atlas' docs accordingly to help devs to start using unified testing.
The test suite itself was significantly improved in terms
how the code is organized.

### Atlas' docs update

* First of all we spent some time on rewriting the bet-ref test-suite example:
  * Clean separation of concerns between operations, runners, and tests.
  * Make use of proper monads (use the least powerful modad possible).
  * Improve readability (type aliases, additional type signatures, comments).
  * Remove dead code.

## Outputs

Outputs of the milestone can be found:

* PRs in [geniusyield/atlas](https://github.com/geniusyield/atlas) repository (mostly merged):
  * [Unified testing mechanism for Privnet <-> CLB](https://github.com/geniusyield/atlas/pull/324)
  * [Fix missing Default (TxBuilderStrategy m) constraint on GYTxBuilderMonad](https://github.com/geniusyield/atlas/pull/325)
  * [Redesign GYTxMonad and its instances to be able to submit transactions](https://github.com/geniusyield/atlas/pull/322)
  * [Overhaul Privnet machinery to use cardano-testnet (from cardano-node): cardano-node@8.9.2](https://github.com/geniusyield/atlas/pull/317)
  * [Replace PSM with CLB and Update to GHC 9.6.5; cardano-api 8.38, cardano-wallet v2024-03-27](https://github.com/geniusyield/atlas/pull/313)
  * [chore: clean up unified tests](https://github.com/geniusyield/atlas/pull/337)
* PR with updated chapter on testing in [Atlas' docs](https://github.com/geniusyield/atlas-docs/pull/90)

## Acceptance criteria and evidence

* A report that describes the improvements and the updated parts of CLB/Atlas documentation:
  * this document.

* bet-ref example test-suite that can be run against a private network and CLB with no modifications:
  * [test-unified](https://github.com/mlabs-haskell/atlas/tree/main/tests-unified).

* Traces of bet-ref test suite run against a real node and CLB
that shows the same outcome and a report that analyzes differences in traces (if they exist):
  * [CLB traces](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms3/bet-ref-emulator.out)
  * [cardano-testnet traces](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms3/bet-ref-privnet.out)

* A milestone report on the work done:
  * this document.