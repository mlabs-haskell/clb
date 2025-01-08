# CLB - Multi-purpose Cardano emulator that works

Cardano | Written By Ilia Rodionov

## Introduction

Funnily enough, there were many attempts to build an emulator for Cardano.
One of the earliest was one from [`plutus-apps`](https://github.com/IntersectMBO/plutus-apps)
repository, which is now officially archived as a whole, but the emulator
was budded off as a [standalone project](https://github.com/IntersectMBO/cardano-node-emulator)
though to our knowing it ain't in a fully working state by the time of publishing.

Without a working emulator, devshops were left for their own devices, and different
solutions started to proliferate. MLabs was no exception and
[PSM library](https://github.com/mlabs-haskell/plutus-simple-model)
was developed to fill the lack of testing capabilities
and catched on as the primary testing tool for quite a long period of time.

PSM was very lean in terms of dependency footprint, very fast and easy to use.
It gave accurate estimates for resource usage too, since it was based on the
`plutus-ledger-api`. Another peculiarity of PSM was use of its own ledger chain
management. This is where discrepancies with the real rules began to accumulate
as time went on. As a result PSM started to suffer from many issues.
Despite that fact PSM was chosen as a testing backend for a new PAB
(Plutus Application Backend) solution Atlas developed by consortisum lead by
GeniusYield.

As the next step on the way on pushing PSM to new horizons, MLabs submitted a
[proposal](https://cardano.ideascale.com/c/cardano/idea/106705) focused on
improving the existing PSM library, but during the work on the first milestone
submitted a [change request](https://drive.google.com/file/d/1b6A0w-YGZs1oGC9ZLPGgvl0LmFg2jLjl/view)
that asked for a pivot towards the development of a new emulator.
The curious reader can find more information in the
[Milestone 1 report](https://github.com/mlabs-haskell/clb/blob/master/docs/reports/ms1/MS1-REPORT.md)
which goes to great lengths motivating the change request.

In this article we are presenting the results of our work by show casing
three examples of using CLB which should give a reader good understanding
of tasks can be accomplished using a new member of MLabs' core tools family.

Let's go!

# Using CLB as a backend for property testing: CEM Script case study

# Unified testing with Atlas: betting application

# Emulating cardano-node: betting app on CTL

# Links

* [CLB repository]() on GitHub
* [CLB docs web-site]()
* [Atlas repository]() on GitHub
* [Atlas docs web-site]()
* [CTL]()
* [PSM repository]() on GitHub (archived)

TODO: link article in the documenatation
TODO: update Changelog
TODO: add badges to README