# 1000121 MLabs - Enhancing and Evolving the Plutus Simple Model (PSM) Test Library - Milestone 1 Report and Proof of Achievement

_Prepared by MLabs on Dec 15, 2023_\
_Ilia Rodionov, ilia@mlabs.city_\
_George Flerovsky, george@mlabs.city_

## Overview

In the current report that concludes Milestone 1 of Catalyst Fund 10 project 1000121 we:
* Give a short overview of the history of PSM library;
* Present analysis of existing forks that shows the current state of the library;
* Describe and justify the way to opt to accomplish the statements of the project.

## PSM: brief history recap
Before addressing the existing forks, we’d like to set some initial context of the PSM library. Initially based on some ideas from the Sundae Swap project, PSM was released in late 2021 and since then the codebase has been developed and maintained by MLabs. As a separate tool, PSM was successfully used in several client/audit projects at MLabs, including Gero Wallet, Kwarxs, Equine, Indigo, Optim, JPG.Store and Atrium.

PSM was created as a simple tool to evaluate Plutus scripts and budgets within more or less realistic transactions. This cannot be done by employing solely on-chain testing: we want to ensure the transactions as a whole fit the limits. This task sounded like a perfect fit for the Putus trace emulator, but unfortunately, back then it was not capable of evaluating Alonzo transactions with Plutus scripts correctly enough: it didn't use the real Plutus evaluator which led to wrong estimates. From day one PSM has been using the very same evaluator from `plutus-ledger-api` that the ledger and the node use; this allowed PSM to gain accuracy in estimates dApps developers needed.

At the same time, PSM has never made use of `cardano-ledger` to manage the state and transitions. Instead, PSM rolled out its own simplified state where only some basic checks (like the existence of UTxOs and signatures) took place. It was mostly fine for the initial tasks of budget estimation, but later some projects started using PSM as a full-fledged Cardano emulator: either for testing their happy paths or for auditing purposes.

The fact that PSM did never depend on `plutus-apps` repository which is notoriously falling behind the release cadence of main cardano repos allowed it to add support for Babbage approximately in lock-step with its launch on mainnet. It was beneficial for projects that were planning to hit the mainnet around that time. But not all Babbage era rules were implemented (ada per word being an example). This increased the divergence in behavior. Developers who were using PSM as an emulator started noticing that transactions that succeeded in PSM failed in the real network, reducing their overall trust in the testing framework. However, developers continued using it (while keeping these divergences in mind) because few other tools could beat its speed and responsiveness, which are important because quick testing frameworks lead developers to write more tests and run them more frequently.

In 2023 the Atlas PAB team decided to use PSM as the foundation for unit-testing capabilities. This task forced the team to fork PSM’s repo and make a non-compatible fork. Although they kept the core part of PSM, this unfortunate fragmentation led to some maintenance issues down the line, when it was time to update Cardano dependency versions.

Preparing for upcoming Conway era, MLabs decided to advance the development of the PSM library. This report marks the first step in that direction as a part of Fund 10.

## Use of PSM: PRs and Forks

Most projects that made use of PSM successfully upstreamed their changes back into the main codebase (some changes are still open PRs). The notable exception is a fork that is used by Atlas PAB. This is the reason we decided to split up this part of our analysis into two parts:

* Changes upstreamed by projects;
* The fork of PSM that is used in GeniusYield’s Atlas PAB.

### Project Contributions

Those changes highlight the issues that devs encountered while using the library within their projects.

* Indigo PR: https://github.com/mlabs-haskell/plutus-simple-model/pull/116

* Kwarx PR: https://github.com/mlabs-haskell/plutus-simple-model/pull/118

* Other contributions.
Even though the following changes already have been merged into the main PSM codebase, we decided to list them here to represent a more detailed understanding of how projects use PSM:
  * Atrium PR 1: https://github.com/mlabs-haskell/plutus-simple-model/pull/113
   * Atrium PR 2: https://github.com/mlabs-haskell/plutus-simple-model/pull/114

### Atlas PAB Fork

__Fork__: https://github.com/geniusyield/plutus-simple-model\
__PR (not intended for merging)__: https://github.com/mlabs-haskell/plutus-simple-model/pull/112

The Atlas fork is important to us because it reveals a new perspective regarding what PSM might look like in the future.

Atlas is a Plutus PAB developed by GeniusYield in collaboration with MLabs, Well-Typed, and Plank. In terms of testing Atlas supports running tests against a real or a private network and an emulated ledger based on a wrapped PSM library. Atlas introduces its own set of types that incorporate many invariants cardano-api and plutus-ledger-api misses. It simplifies transaction building by eliminating values that don’t exist in the wild — see https://github.com/input-output-hk/cardano-ledger/issues/2941.

The are several reasons why this fork exists:

* The way PSM handles reference scripts is somewhat opinionated: rather than storing scripts internally, it allows users to say they want to use a script as a reference one (which effectively stores only the script hash in a Plutus `TxOut` value) and continues asking for the entire script body every time (a tx won’t include the script wintess, but the API is weird). To improve this the Atlas’ fork adds a separate field in the state to store reference scripts and uses it to implement full-fledged `utxoAtTxOutRef` which returns a value of common UTxO type that contains scripts as well;

* There was a dependency clash on Haskell Plutus libs (`plutus-core`, `plutus-tx`, and `plutus-ledger-api`) caused by Plutarch, a dependency of the original PSM which is still incompatible with need version 1.6.0.0. But when PSM is used with Atlas it makes no sense to depend on Plutarch (since it’s supposed you use Atlas to load your scripts), so the team decided to drop Plutarch support completely;

* Original PSM provides so-called fake coins to allow the use of native assets. They are backed by Plutus V1 minting policy written in Plutarch. Since Atlas needed V2 policy and the support for Plutarch has been dropped, those were reworked in Plutus TX;

* Outdated dependencies: since PSM maintenance has lagged somewhat, some code required updates to be compatible with the Cardano libraries that Atlas uses (mostly `cardano-ledger-*` libs);

* There were two minor bugs (mlabs-haskell/plutus-simple-model#109 and mlabs-haskell/plutus-simple-model#110) that the fork fixes.

Using PSM with Atlas instead of standalone PSM has several advantages:

* Existing off-chain code can be reused in tests; there is no need to rewrite transaction-building logic again to be used with tests only;

* It also means you don’t need to care about the ways you are loading your scripts in PSM anymore: if you have your scripts in an Atlas-based project  - it is sufficient;

* Atlas executes more checks than PSM when it comes to building transactions. By leveraging Atlas’ `GYTxMonad` you are guaranteed you won’t end up with malformed transactions. This might be a controversial advantage for some types of tests.

At the same time, there is one obvious thing to be improved in the future, namely _test unification_. Though you already can reuse off-chain code, the way you write tests against a real node and PSM differs. It would be nice if the same test file could be run against either a  test network or an emulated state (of course, not all kinds of tests can be run against an emulated state and vice-versa). These discrepancies are dictated merely by leaking implementation details in the initial setup and transaction submission logic (say, the node expects just signed transactions whereas PSM wants all signing keys at hand and does signing internally). Such things can be aligned by making PSM more compatible with a real Cardano node API.

Significant efforts would be required to merge the current Atlas fork of PSM into the main codebase assuming that we want to preserve PSM’s current feature set. The main reason for this is that the main codebase depends on Plutarch to provide first-class support for Plutarch-based scripts, whereas the fork has dropped Plutarch support. This codebase divergence makes it harder to port changes to support new Cardano features in the future. Currently, the fork supports cardano node version 8.1, while the main branch supports cardano node version 1.35, which is significantly behind the latest versions and will need to be updated to support the Conway era.

## The Current State

This section describes our understanding of the current state of affairs, based on the information we obtained during project contributions, fork analysis, and some feedback received from users of the library (including the GeniusYield team).

Two years later since its creation, PSM is still in use. Among the main reasons behind that (which reflect developers’ needs in fact) we’d like to highlight:

* __Small dependency footprint.__ PSM (unlike other tools, for example, IOG’s `cardano-node-emulator` or Tweag’s `cooked-validators`) is very peaky when it comes to its dependencies: it doesn’t depend on `plutus-apps`; furthermore, it doesn’t make use of `cardano-api` either. As we see in the project-specific branches section, most projects succeeded in bumping PSM dependencies according to their setup with minimal modifications in the code;

* __Keeping up with cardano updates.__ As a direct outcome of the previous point, up to the moment, it was feasible to add new Cardano features pretty quickly as they were implemented in the core libraries;

* __Lightweight and speedy.__ These advantages are of paramount importance - especially for audit projects where fuzzy testing is actively used. Fuzzy testing tends to require orders of magnitude more instances of each test (with randomized inputs) to be evaluated, which slows down to a crawl if there are significant memory and execution overheads to set up each instance;

* __Simplicity and small codebase.__ The way PSM works is simple. You can start confidently reasoning how it works in no time - being less than 6k line in Haskell most teams were able to start using PSM pretty quickly;

* __Convenient options for testing.__ PSM takes into account the real needs of developers. For example, it easily allows running tests with transactions that exceed execution limits, reporting the over-budget statistics. Being able to test your protocol independently of working on improving on-chain scripts turns out to be very useful.

The same motivation convinced Atlas’ team to choose PSM. Atlas’ fork would help us to efficiently meet some of the modernization goals of the PSM improvement project, but it is not very compatible with the main branch or other goals of the project. Thus, integration with Atlas raises more fundamental questions:

1. Do we need PSM as a standalone tool at all or shall it be coupled with a PAB like Atlas or CTL to be used? Reusing off-chain code is a huge benefit, but the integration was not as easy and stable as we would like to have.

2. If so, can we drop PSM’s built-in way of writing off-chain code completely in favor of a PAB of one’s choice? If so, should we drop PSM’s way of writing off-chain code so that developers can re-use off-chain code that they’ve already written in the PAB frameworks in their test suites?

3. Do we want to keep PSM as a universal testing backend for all types of tests? If so, how flexible should it be? For example, while Atlas and other PABs does many checks when building transactions, do we want to allow developers skip some checks in PSM?

4. What’s the best way for non-haskell PAB frameworks like CTL to use PSM?

5. Can we reuse some indexing and query facilities that are traditionally used in PABs to enhance PSM’s testing capabilities?

The upcoming updates in Cardano might pose a big challenge (and the main subject for the current project). PSM still suffers from some unimplemented Babbage introductions and upcoming Conway introduces new concepts that are way harder to implement in the current architecture. In a word, we see an increasing technical debt here and we should address it.

## Next steps: The Future of PSM

Based on the understanding we gained during this milestone, we are proposing the following path for pushing PSM towards the goals we initially stated in Fund 10. The main goal has not changed - we aimed at (and still do) bringing support for the Conway era. But the particular way we see optimal changed significantly. It will require diving into how PSM is built.

PSM can be thought of as consisting of five things:

* __The ledger.__ The lightweight blockchain mock (emulator) handles the state similar to what the Cardano node does;

* __User and blockchain parameter management.__ Provides a way to set up users (i.e. key pairs) and initial fund distribution;

* __On-chain development tools interfaces__ (for PlutusTx and Plutarch). Allow import of on-chain scripts at the Haskell level (which means one can re-use types that the scripts use for datums, redeemers, and parameters and just write your custom scripts in a testing project);

* __Off-chain code facilities:__

  * __The query layer.__  On top of the emulator, this minimal indexer provides information needed for transaction building and verifying balance-related test assertions;

  * __The transaction skeleton builders.__ Set of monoidal functions to build transaction skeletons;

* __Balance checkers.__ Used for specifying balance-related test logic in concise form.

The original plan in our _November statement of milestones_ was to preserve all those components and even extend them:

* Upgrade platform stack in milestone 2.
* Fix known issues (mostly in the ledger and off-chain code facilities) in milestone 3.
* Extend on-chain development tools interfaces to support Aiken in milestone 4.
* Add support for Conway transactions in milestone 5.

However, over the course of our analysis leading up to Milestone 1, we’ve realized that a pivot in our plan will more effectively achieve all of the goals of the PSM Catalyst project, while significantly reducing the future maintenance burden and allowing PSM to react more quickly to upstream Cardano updates.

The following sections articulate the main decisions we have made during this milestone. We will submit our actual SOM change request in a follow-up document.

### Leveraging cardano-ledger to handle the blockchain state

Many issues (ada per word and other missing ledger checks, wrong fees due to relaxed transaction building/applying) are rooted in the fact PSM uses its simplified state rather than reusing one from `cardano-ledger`. It leads to discrepancies which often bite developers as they approach a testnet or a local network.

Besides this takes the library into a position where it is doomed to catch up forever. Instead, if PSM could reuse the ledger’s state it would guarantee the same transaction validation logic and ability to reuse new features instead of reimplementing them.

Of course, it’s not a straightforward use and we foresee some issues we might run into. Let’s consider the use of `applyTx` from the ledger: it unconditionally runs scripts in the restricted mode as part of UTXOS rules. To keep the PSM’s feature of running transaction flows that don’t fit budgets we will need to handle this somehow (for example by bumping limits and doing some calculations afterward to get normalized values). But the fact that this approach is successfully used by the Hydra project and some others gives us enough confidence we can find proper ways to circumvent any possible pitfalls down that way.

From the practical perspective, it means that the ledger component of PSM is going to be reworked from scratch (in milestone 2 presumably). PSM will keep running some auxiliary state, but the core part of it, namely UTxO set, and the delegation certificates (and likely more in Conway) will be fully managed by the ledger.

We did some very preliminary measures to verify that this decision won’t affect the advantages of PSM, especially the performance, and haven’t identified any issues so far.

In terms of original goals covering, this part of the work will bring us most of the results of original milestone 3:
* Better UTxO TX support: min ada calculations
* Network fees calculation
* Improving script budgets calculations

At the same time, being granted we use Conway ledger we will meet most requirements from milestone 5 (Conway support).

### Off-chain code is not the responsibility of an emulator

Using PSM standalone implies developers repeat their off-chain code logic solely for testing's sake. While it was considered a norm (at the least among backend developers) before well-designed PABs like Atlas emerged, now it seems to be a relic of the early days of Plutus development. There is no reason to duplicate it as such (at the least for happy-path testing).

It’s sometimes argued that there should exist a more relaxed way of writing off-chain code that allows specific things. As an example imagine you want to test a Plutus script, that should prevent transactions that output to scripts without datums. If your off-chain code facilities (ironically this is exactly the case with PSM) prevent you from building such a transaction you have no way to go.

Indeed, there might be situations when your negative tests might require things like that; but our point is still the same: off-chain code is not the responsibility of an emulator. It’s a separate concern and should be addressed accordingly. For happy-path testing, the set of transactions your PAB/tx building library like lucid can produce should be sufficient and will save up a significant amount of time and gain more confidence there are no discrepancies in the way you build transactions in the real app and for testing. For negative tests and audit purposes, there should exist a separate mode or tool that allows building malicious transactions that should be declined.

From the practical point of view this means we are going to deprecate and fully stop using several parts of PSM, namely:
* On-chain development tools interfaces;
* Off-chain code facilities.

Those parts will stay in the current version for compatibility reasons, but all next releases will require an external tool to build transaction. We are going to start with Atlas and add support for CTL in the scope of the current Fund 10 project. Integrations with other tools will follow naturally if PSM succeeds in doing its share of the job well, which we believe should be emulating the ledger state and providing effective ways to work with it at different layers (more on that in the final section).

This decision effectively pushed some of our initial goals from milestones 3 and 4 to Atlas (or another off-chain facilities provider):
* Comprehensive support for Tx outputs selectors like boxAt;
* Adding support for stronger validator abstractions;
* Support for Aiken-based scripts.

### The outcome: A modern emulator that works

In this section we’d like to elaborate on what PSM should finally be all about. As it was just described after reworking the ledger part of PSM and dropping off-chain code facilities we will get a completely brand-new product which we term Cardano Ledger Backend, CLB for short, pronounced as “club” (/klʌb/).

In its heart, it will be a cardano-ledger-based emulator that will behave like a node when it comes to transaction validation and will be easier to maintain in lock-step with cardano-ledger. It also going to be very rapid since it completely sidesteps all network and consensus operations, meaning it’s effectively a pure state. We are also going to keep our strategy of avoiding dependencies that do (or likely may) fall behind, nothing besides cardano-ledger, cardano-api (and some base cardano libraries) will be on the list.

Now, to make it useful we need to consider the ways the emulator can be used by projects. We believe that different tools that may want to use CLB will have different requirements. Our shortlist currently contains Atlas, a Haskell-based PAB, and CTL, a PureScript-based library that can be used like a PAB or as a library for building dApps in browsers. Both have similar goals, but we can easily imagine a testing framework like cooked-validators that needs to run many tests in parallel. There are four important kinds of interactions we want to address:
* Querying the emulator’s state;
* Submitting transactions;
* Calling emulator-specific handles;
* Spawning instances of the emulator and initial fund distribution.

#### Querying the emulator

There are different ways to query the state we want to support:

* __Ouroborous Local State Query mini-protocol.__ It doesn’t require any indexing and handling fancy requests. This option is a good choice for non-haskell clients who can access the emulator using a node-style socket and also for testing custom indexing solutions. Preliminary we are planning to use this for CTL integration. In place of a real Cardano node behind the standard CTL query stack (Ogmios + Kupo) CLB will be used. It will require some additional interaction with the emulator, see “Calling emulator-specific handles” section;

* __Built-in Simple Index.__ In addition to the raw ledger state (which more or less can cover mini-protocol) CLB will optionally run an additional built-in index API that can be used to implement an adapter to the GYQueryUTxO handle. In addition to mini-protocol, this API provides more ways to get UTxOs by address and so on. This way of querying will be available as a Haskell API and as an HTTP/WebSocket API in the future. This channel will be used with Atlas;

* __Test-specific API.__ Test assertions may require handling an additional state and providing some API to query it especially when it comes to calculating balances; in this case, this will be implemented as a Haskell API (and probably exposed together with emulator-specific handles as HTTP/WebSocket API).

#### Submitting transactions

We are going to support two ways of submitting transactions:

* __Ouroborous Transaction Submission Protocol v2__ that operates on the wire level through the socket for use for CTL and other non-haskell clients;
* __Submitting using Haskell API__ based on types from cardano-api (plus an adapter for GYSubmitTx handle) to use with Atlas.

#### Calling emulator-specific handles

There are several areas where CLB cannot behave as a real node with the most obvious being the time. Without consensus, there is no clock and the emulator needs additional clues to move time forward. Off-chain operations like gyWaitUntilSlot won't work unless we instruct them to adjust the clock (slot) manually.

When using Haskell API it might be backed into the integration code itself, but poses a problem if we want to work with an emulator as a pretend node using mini-protocols. To address this issue we propose exposing an additional mini-protocol (either as a separate API or multiplexed along with the existent mini-protocols) that allows the interaction with those emulator-specific handles from non-haskell environments.

#### Spawning instances of the emulator and initial fund distribution

Finally, we need a way of setting up an emulator instance(s). Apart from physically running it from Haskell code or spinning up an external process, it requires some preliminary and post-initialization steps which include:
* Building protocol parameters;
* Populating genesis UTxO and probably running a genesis transaction to distribute funds;
* Minting or bootstrapping some additional tokens.

As we saw, divergence in how these tasks are supposed to be done for different backends led to problems with test unification and we want to address this. We haven’t spent much time in this milestone on this topic; from what we know now we need to unify or abstract this part so backends can use them interchangeably.

In most cases, audits (and regular testing alike) necessitate or benefit from running multiple instances, so we might want to provide some built-in machinery for spawning multiple cells that share the same configuration (parameters and genesis), but work in isolation.





















