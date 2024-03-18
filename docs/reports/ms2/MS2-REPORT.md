# 1000121 MLabs - Enhancing and Evolving the Plutus Simple Model (PSM) Test Library - Milestone 2 Report

_Prepared  by MLabs on March 15, 2024_\
_Ilia Rodionov, ilia@mlabs.city_

## Overview

In this milestone, we worked on the minimal implementation of the CLB emulator.
To demonstrate it works we took `bet-ref` example from the original Atlas'
[examples repository](https://github.com/geniusyield/atlas-examples) and get it to work in two variants:
* PSM-based version using original Atlas and Atlas' fork of PSM;
* CLB-based version based on brand-new CLB emulator.

## Outputs

Outputs of the milestone can be found here:
* CLB implementation in the `master` branch in [mlabs-haskell/clb](https://github.com/mlabs-haskell/clb) repository;
* A fork of Atlas PAB that uses CLB instead of PSM along with the test suite in [`clb-ms2`](https://github.com/mlabs-haskell/atlas/pull/1) branch in `mlabs-haskell/atlas` repository.

Additionally:
* The version of `bet-ref` example we used to get PSM traces is in the branch `[bet-psm`](https://github.com/mlabs-haskell/atlas/pull/2) in `mlabs-haskell/atlas` repository.

## Acceptance criteria and evidence

Traces obtained with [PSM](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms2/bet-ref-psm.out)
and [CLB](https://github.com/mlabs-haskell/clb/tree/master/docs/reports/ms2/bet-ref-clb.out) witness that both emulators works. Also, they demonstrate the advantages of CLB in fees and storage costs calculations. The number of tests doesn't allow us to interpret results as a full-fledged benchmark, but it's clear that CLB is not slower than PSM (and we still use `StadardCrypto`, so we can eliminate some cryptographic calculations to boost the performance).


## Notes

Additionally, we decided to start using CLB
in another Catalyst Fund10 project 1000118 CEM Script.
Initially, the project chose PSM for testing,
but now it has become obvious we want to switch to the new emulator.

We believe it will be fruitful for both projects.
Currently, we maintain a separate version of CLB for CEM, 
since versions of core libraries used by CEM are more recent than the ones currently used by Atlas. 
Once we update versions in Atlas we will be able to merge them.
