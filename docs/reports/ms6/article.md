# Testing dApps on Cardano with CLB emulator

Cardano | Written By Ilia Rodionov



In this article we take a look at **CLB** - Cardano emulator developed by MLabs
and show how CLB and related tools can be used together
in various testing approaches when building dApps on Cardano.

This article consludes our work on CLB Catalyst
[project](https://milestones.projectcatalyst.io/projects/1000121)
ans presents the results.
We give an overview of CLB and showcase three examples of its use.
This should give a reader comprehensive overview of tasks
that can be accomplished using a new member of MLabs' core tools family.

We start our journey looking at `clb` library,
which is the core part of the emulator.
Then we examine a particular example of **CEM Script** project,
that demostrates how standalone `clb` library can be used for
rather specific kind of _mutation-based property testing_.

Then we present a more common show case that covers testing
a demo betting dApp with CLB and [**Atlas PAB**](https://atlas-app.io/).

Finally, we give an overview of the **node emulator mode**
which is the most universal way to use CLB.
Being language-agnostic, it makes possible to make use of the emulator
from non-Haskell environments.
Particularly we introduce a case-study that shows the use of CLB
from a Purescript-based library -
[CTL](https://github.com/Plutonomicon/cardano-transaction-lib).

Let's commence!

## A bit of history

Historically when Cardano entered Alonzo era
and the introduction of Plutus language blazed the trail
to building dApps on Cardano
there existed no usable emulator.
The emulator from IOG's _Plutus Application Framework_ [^1]
didn't play well and tended to fall behind
the cadence of major Cardano releases.

That way, without a working emulator, developes were left for their own devices,
and various solutions started to proliferate to fill the gap.
MLabs was no exception and
[PSM library](https://github.com/mlabs-haskell/plutus-simple-model)
was developed to compensate the lack of testing capabilities.
It catched on as a primary testing tool for quite a long period of time.

PSM was very lean in terms of dependency footprint and very fast.
It gave accurate estimates for resource usage too,
since it was based on the `plutus-ledger-api`.
One peculiarity of PSM was their own ledger state management,
i.e. a simplified set of rules for handling transactions
and stepping the ldger state.
It was close to `cardano-ledger` rules, but not exactly.

This is where discrepancies with the real rules quickly began to accumulate
as time went on, so PSM started to suffer from many issues which deteriorated
developer experience or made some checks impossible.

Despite this fact PSM was chosen as a testing backend for a new
PAB (Plutus Application Backend) solution called
[**Atlas**](https://atlas-app.io/)
developed by consortisum led by GeniusYield.

As the next step on the way of pushing PSM to new horizons,
MLabs submitted a Catalyst
[proposal](https://cardano.ideascale.com/c/cardano/idea/106705)
focused on improving the existing PSM library.
During the work on the first milestone we submitted a
[change request](https://drive.google.com/file/d/1b6A0w-YGZs1oGC9ZLPGgvl0LmFg2jLjl/view)
where we asked for a pivot towards the development of a new emulator.
A curious reader can find more information in the
[Milestone 1 report](https://github.com/mlabs-haskell/clb/blob/master/docs/reports/ms1/MS1-REPORT.md)
which goes to great lengths to motivate that change.

# Emulator core: `clb` library

The core part of the emulator is Haskell `clb` library which is self-sufficient
for use cases that solely require a pure ledger state. Unlike PSM, CLB uses
`cardano-ledger` to maintain the state which guarantees it bahves exactly
the same way the real ledger does (being properly configured).

The API provided by `clb` library is very simple
and mostly defined over types from `cardano-api`
which makes the client code highly compatible with a real node.
It implements a _pure state_ that holds a ledger instance
one can easily and cheaply spin-up,
getting the access to corresponding signing keys that controls funds.
Among core supported operations:
* transaction submitting
* querying UTxO state
* jumping to a future slot

To use such an API, the client should be able to build transactions,
including coin selection and blancing.
Though it may be not the most common case, we believe this ability is important,
since it plays very well with the idea of modularity.
On the one hand, one can use whatever tool for transactions building,
with the core of the emulator being agnostic on the client's off-chain code.
On the other hand, as was already mentioned one can easily switch from
the emulator to a real node.

Being just a ledger state, `clb` library has many limitations.
Most importantly, the blockchain and consesnsus are not involved
which means there is no notion of:
* time, slots and epochs
* blocks

The former poses a problem for testing, since quite often the logic relies
on time. Moreover, testing such contracts against a real network is also
problematic without scaling time periods:
waiting for even a non-significant time spans is prohibitevely slow.

Being a pure state, to alleviate this impediment `clb` offers
a way to travel in the future
immediatley by speciying the target slot and
providing facilities to go forth and back between wall clock time and slots.

Also there is a way to crank slots on per-transaction basis.
Whenever a transaction comes `clb` switches automatically to the next slot.
This approach significantly improves readability of logs
and simplifies debugging.

For more detailed description of the API please refer to the
[CLB docs](https://mlabs-haskell.github.io/clb-docs/use/lib).

# CEM Script: CLB as a backend for mutation-based property testing

The crucial property of CLB we wanted to preserve was speed
since it becomes critical for property-based testing.
In this section we present the case of **CEM Script** project
which is a good example of how validators on Cardano could be tested
and how CLB comes handy in doing that.

## CEM Machines

dApps on Cardano consist of validators also known as smart-contracts, and a
validator can be thought of as a state machine of a specific form [^2]
called _Constraint Emitting Machines_. A valid transition in a CEM corresponds
to a single valid transaction on the chain (though one transaction can span over
several scripts). The name comes from the way transitions are defined over
state and the input:

```haskell
transition :: State → Input → Maybe (State, TxConstraints)
```
To ease the digestion of the idea pretend that `State` is a datum holding the state
and the `Input` is a redeemer providing the transition siganal. CEM emitts a set of
`TxConstraints` along with the target state.

Having a definition of such a state machine we can derive different parts of an
application including:
* On-chain validator that ensures that we are transitioning to a valid
target state via a transaction that satisfies the emitted constraints.
* Off-chain code that can build the transactions.
* Indexing gadgets to filter out related transactions and extract
transitions happened on-chain.

Now, how can we check that the definition of a state machine is consistent
and on-chain part is in sync with off-chain? And what does it mean for a
state machine definition to be consistent in first place? Turns out that
CEM machines are very prolific in terms of testing possibilities they crack open.

But let's take a short break and dive into how the DSL for defining CEM
machines is organized and familiarize ourselves with an example definition
for simple acutioning dApp.

## DSL for defining CEM Machines

## Auctioning dApp

First we define **states**, specifying the data that should be known in some
states.
We skip the definition of `Bid` for brevety's sake:

```haskell
data SimpleAuctionState
  = NotStarted
  | CurrentBid
      { bid :: Bid -- the latest bid
      }
  | Winner
      { bid :: Bid -- the winning bid
      }
```

Then we define a set of possible **transitions**, that moves the state.
Some of them requires arguments that should be passed when a transition happens:

```haskell
data SimpleAuctionTransition
  = Create
  | Start
  | MakeBid
      { bid :: Bid
      }
  | Close
  | Buyout
```

## Mutation-based property testing

Link to Hydra article

IIIRC state machine does two things:

1. OnChain code works same as OffChain code
2. Checking custom user invariants for OffChain code

So first one is covered by mutation testing and second state-machine testing.

As mutation testing ultimately finds only CEM Script internal problems one can disable it with doMutationTesting field.


# Unified testing with Atlas

## Betting dApp

The idea of reusing the off-cahin code led us to
implementing [**unified testing**](https://atlas-app.io/getting-started/testing#overview-of-unified-testing-in-atlas) feature in Atlas.
It allows running the same test-suite against the emulator
and a real cardano privnet.


## ? Atlas: using REPL to execute contracts

-- https://stackoverflow.com/questions/42425939/how-can-i-use-repl-with-cps-function

newtype CPSControl b = CPSControl (MVar b)

startDebugCps :: ((a -> IO b) -> IO b) -> IO (a, CPSControl b)
startDebugCps cps = do
  cpsVal <- newEmptyMVar
  stopAndRet <- newEmptyMVar
  void . forkIO $
    void . cps $ \c -> putMVar cpsVal c >> readMVar stopAndRet
  s <- takeMVar cpsVal
  return (s, CPSControl stopAndRet)

stopDebugCps :: CPSControl b -> b -> IO ()
stopDebugCps (CPSControl stopAndRet) = putMVar stopAndRet

testCps :: GYCoreConfig -> (GYProviders -> IO a) -> IO a
testCps c = withCfgProviders c "test"

eval' :: GYProviders -> GYTxGameMonadIO a -> IO a
eval' = runGYTxGameMonadIO GYTestnetPreview




cfg <- coreConfigIO "atlas-config.json"
(providers, ctrl) <- startDebugCps $ testCps cfg
5:57
Now, with access to providers I can run in GYTxGameMonad code (such as my contracts) using the eval' utility above.
I usually alias eval = eval' providers in my REPL

# Emulating cardano-node: betting dApp on CTL

when the emulator mimics a real `cardano-node`
by maintaining an IPC socket
that supports a subset of Ouroboros mini-protocols.

# Useful Links

* [CLB repository]() on GitHub
* [CLB docs web-site]()
* [Atlas repository]() on GitHub
* [Atlas docs web-site]()
* [CTL]()
* [PSM repository]() on GitHub (archived)
* [The extended UTxO model] ()
* [Mutation-based TDD](https://abailly.github.io/posts/mutation-testing.html)

TODO: link article in the documenatation

TODO: update Changelog

TODO: add badges to README

[^1]: Now the whole [_Plutus Application Framework_](https://github.com/IntersectMBO/plutus-apps)
is officially archived, but the emulator budded off as a
[standalone project](https://github.com/IntersectMBO/cardano-node-emulator).

[^2]: As ["The Extended UTXO Model"](https://iohk.io/en/research/library/papers/the-extended-utxo-model/)
paper explains, validators can be modeled as silghtly alternated
[Mealy machines](https://en.wikipedia.org/wiki/Mealy_machine)


Tags: blockchain | cardano | testing