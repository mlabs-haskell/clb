# 1000121 MLabs - Enhancing and Evolving the Plutus Simple Model (PSM) Test Library - Milestone 4 Report

_Prepared  by MLabs on October 7, 2024_\
_Ilia Rodionov, ilia@mlabs.city_

## Overview

In this milestone, we were focused on two main tasks:
* Updating CLB and Atlas dependencies to the latest Conway-compatible versions,
doing all changes this update necessitates along the way.
* Reimplementing the onchain script from bet-ref example using Aiken language
and wiring it into bet-ref test suite.

## Conway Update

### CLB

The nature of changes in CLB mostly boils down to make all necessary places
polymorphic in era. Only one era is supported by any time, but it might be
any post-Alonzo era starting with Babbage. The default era is set to Conway.
Default Conway parameters have been added.

### Atlas

Since Atlas provides its own types for all transaztion components, several
new types have been defined along with functions to convert to/from cardano-api
counterpars:
  1. GYDRep
  2. GYDelegatee
  3. New certificates

Then, support for Plutus V3 has been added.

We've got some change for free just by updating dependencies, including
new fees for using reference scripts.

## Aiken-based example

This work demonstrates the use of Aiken-based scripts with Atlas PAB/CLB.
There are two main concerns when doing this:
 * Ensuring that the encoding of boundary types matches.
 * Loading external script and applying their arguments.

Both topics are covered in the milestone close-out
[video](https://www.youtube.com/watch?v=o_47ItaL8kQ) in details.

The current solution based on the approach of finding the proper
encoding by writing proper types in Aiken for parameters, datums,
and redeemers to match them.

While this sounds like a viable option, an additional improvement
to Atlas PAB outside the scope of the current project has been made
recently. It brings support for
[CIP-57 blueprints](https://github.com/geniusyield/atlas/pull/356)
to Atlas.
It will simplify the use of Aiken-based scripts by allowing
automatic generating of types for parameters and applying them in a more
type-safe manner.

## Milestone Outputs

Outputs of the milestone can be found:

* PRs in [geniusyield/atlas](https://github.com/geniusyield/atlas) repository:
  * [feat: update dependencies in sync with node v9.1.0 & Conway support](https://github.com/geniusyield/atlas/pull/327)
  * [feat: aiken-based bet-ref example](https://github.com/geniusyield/atlas/pull/358)

* PRs in [mlabs-haskell/clb](https://github.com/mlabs-haskell/clb) repository:
  * [Conway support](https://github.com/mlabs-haskell/clb/pull/41)
  * [Upon chase/conway](https://github.com/mlabs-haskell/clb/pull/44)

A new repository [bet-ref-aiken](https://github.com/mlabs-haskell/bet-ref-aiken)
with the Aiken-based implementation of bet-ref example.

Currently we are using the version of the contracts compiled with Aiken 1.0.29
located in this [PR](https://github.com/mlabs-haskell/bet-ref-aiken/pull/1).

## Acceptance Criteria and Evidence

* Traces of the bet-ref example after updating dependencies to
Conway-compatible versions are available
[here](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms3/bet-ref.out).

* The section "Conway Update" of the current document provides the report on the
Conway part of the work as was requires by the PoA statement.

* All tests from bet-ref suite are passed with Aiken-based contract, traces can be found
[here](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms3/bet-ref-aiken.out).
Instructions to reporoduce tests can be found in this
[PR](https://github.com/geniusyield/atlas/pull/358).

* A close-out video on the milestone can be found [here](https://www.youtube.com/watch?v=o_47ItaL8kQ).

* The milestone report - this document.