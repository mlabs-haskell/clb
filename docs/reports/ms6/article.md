# Testing dApps on Cardano with CLB emulator

Cardano | Written By Ilia Rodionov


In this article we take a look at **CLB** - Cardano emulator developed by MLabs.
We show how CLB and related tools can be used together
in various testing approaches
when building dApps on Cardano.

This article consludes our work on CLB Catalyst
[project](https://milestones.projectcatalyst.io/projects/1000121)
ans presents the results.
We give an overview of CLB and showcase three examples of its use.
This should give a reader a comprehensive overview of tasks
that can be accomplished using a new member of MLabs' core tools family.

We start our journey looking at `clb` library,
which is the core part of the emulator.
Then we examine the case of **CEM Script** project,
that demostrates how standalone `clb` library can be used for
rather specific kind of _mutation-based property testing_.

Then we present a more common show case that covers testing
a demo betting dApp with CLB and [**Atlas PAB**](https://atlas-app.io/).

Finally, we give an overview of the **node emulator mode**
which is the most universal way to use CLB.
Being language-agnostic, it makes possible using of the emulator
from non-Haskell environments.
Particularly, we introduce a case-study that shows the use of CLB
within a Purescript-based library for dApp development -
[CTL](https://github.com/Plutonomicon/cardano-transaction-lib).

Let's commence!

## A bit of history

Historically when Cardano entered Alonzo era
and the introduction of Plutus language blazed the trail
to building dApps on Cardano
there existed no usable emulator.
The emulator from IOG's _Plutus Application Framework_ [^1]
didn't play well and tended to fall behind
the cadence of the following major Cardano releases.

Without a working emulator, developes were left to their own devices,
and various solutions started to proliferate to fill that gap.
MLabs was no exception and
[PSM library](https://github.com/mlabs-haskell/plutus-simple-model)
was developed to compensate the lack of testing capabilities.
It catched on as a primary testing tool
and has been used for quite a long period of time.

PSM was very lean in terms of dependency footprint and very fast.
It gave accurate estimates for resource usage too,
since it was based on the `plutus-ledger-api`.
One peculiarity of PSM was their own ledger state management,
i.e. a simplified set of rules for handling transactions
and stepping the ldger state.
It was close to `cardano-ledger` rules, but not exactly the same.

This is where discrepancies with the real rules quickly began to accumulate
as time went on, so PSM started to suffer from many issues which deteriorated
developer experience or made some checks impossible.

Despite this fact PSM was chosen as a testing backend for a new
PAB (Plutus Application Backend) [**Atlas**](https://atlas-app.io/)
developed by consortisum led by GeniusYield.

As the next step on the way of pushing PSM to new horizons,
MLabs submitted a Catalyst
[proposal](https://cardano.ideascale.com/c/cardano/idea/106705)
focused on improving the existing PSM library.
During the work on the first milestone we submitted a
[change request](https://drive.google.com/file/d/1b6A0w-YGZs1oGC9ZLPGgvl0LmFg2jLjl/view)
which asked for a pivot towards the development of a new emulator called CLB,
which stands for _Cardano ledger backend_ and pronounced /klʌb/.
A curious reader can find more information in the
[Milestone 1 report](https://github.com/mlabs-haskell/clb/blob/master/docs/reports/ms1/MS1-REPORT.md)
which goes to great lengths to motivate that turn-around.

# Emulator core: `clb` library

The core part of the emulator is Haskell `clb` library which is self-sufficient
for use cases that solely require a pure ledger state. Unlike PSM, CLB uses
`cardano-ledger` to maintain the state which guarantees it bahaves exactly
the same way the real ledger does (being properly configured).

The API provided by `clb` library is very simple
and mostly defined over types from `cardano-api`
which makes the client code highly compatible with a real node.
It implements a _pure state_ that holds a ledger instance
that one can easily and cheaply spin-up,
getting the access to corresponding signing keys that controls genesis funds.
Among core supported operations:
* transaction submitting
* querying UTxO state
* jumping to a future slot

To use such an API, the client should be able to build transactions,
including coin selection stage and blancing.
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
Scalign also comes at some price and potentially could hide subtle bugs.

Being a pure state, to alleviate this impediment `clb` offers
a way to travel in the future
immediatley by speciying the target slot number and
providing facilities to go forth and back between wall clock time and slots.

Also there is a way to crank slots on per-transaction basis.
Whenever a transaction comes, `clb` switches automatically to the next slot.
This approach significantly improves readability of logs
and simplifies debugging.

For more detailed description of the API please refer to the
[CLB docs](https://mlabs-haskell.github.io/clb-docs/use/lib).

# CEM Script: CLB as a backend for mutation-based property testing

The crucial property of CLB we wanted to preserve was speed
since it becomes critical for property-based testing.
In this section we present the case of **CEM Script** project
which brings an interesting example of how dApps on Cardano
could be tested and how CLB comes handy in doing that.

## CEM Machines

dApps on Cardano consist of validators also known as smart-contracts, and a
validator can be thought of as a state machine of a specific form
called _Constraint Emitting Machines_ [^2].
A valid transition in a CEM corresponds
to a single valid transaction on the chain
(though one transaction can span over several scripts).
The name _Constraints Emitting Machines_ comes from the way
transitions are defined as a function of the _state_ and the _input_
that emitts a set of `TxConstraints` along with the new state:

```haskell
transition :: State → Input → Maybe (State, TxConstraints)
```

Having a definition of such a state machine, **CEM Script** machinery can
derives different parts of an dApp including:
* On-chain validator that ensures that we are transitioning to a valid
target state via a transaction that satisfies the emitted constraints.
* Off-chain code that is capable of building the corresponding transactions.
* Indexing gadgets to filter out related transactions and extract
information about transitions that happened on-chain.

Now, a question arises: how to check the viability of the application
and how to ensure that the generated on-chain scripts
and off-chain mechanics match each other?
It turns out that CEM machines are very prolific
in terms of testing possibilities that they crack open.
We can not only check that two parts go well together
but also turn the off-chain machinery into a model
to use model-based testing.

But let's take a short break and have a detour towards the DSL
for defining CEM machines to understand what exactly
those `TxConstraints` may look like
and to familiarize ourselves with an example
definition for a simple auctioning dApp.

## Detour: DSL to define CEM machines

### Buliding blocks

In this section we are exploring the approach for defining CEM machines within
**CEM Script** project. We start with the simplest building blocks and move up to
higher-lever structures, going from the bottom to the top.

As me mentioned above, the DSL is used to generate both on-chain and off-chain
components.

**CEM Script** constraint language is based on `ConstraintDSL` GADT which
allows expressing terms of the language (some constructors are elided):

```haskell
data ConstraintDSL script value where
  Pure :: ... ConstraintDSL script value
  Ask :: ... ConstraintDSL script datatype
  GetField :: ... ConstraintDSL script value
  UnsafeUpdateOfSpine :: ... ConstraintDSL script value
  Eq :: forall x script. (Eq x) =>
    ConstraintDSL script x ->
    ConstraintDSL script x ->
    ConstraintDSL script Bool
  LiftPlutarch ::
    forall px py script.
    (PlutarchData px, PlutarchData py) =>
    (ClosedTerm (px :--> py)) ->
    ConstraintDSL script (PLifted px) ->
    ConstraintDSL script (PLifted py)
  ...
```

From the definition you can get the rough idea that one can lift constants
into the language with `Pure`, access the machine state with `Ask`,
doing deconstructions with `GetField`, build values with `UnsafeUpdateOfSpine`
and lift Plutarch functions with `LiftPlutarch`.

DSL terms can be further specified as either being regular __values__ or __patterns__
for pattern matching. The way those are used differs for on-chain and off-chain.
That differnce is captured by the following type families.
Here, `script` is an uninhabited type that is used to tie together
different things associated with a particular machine,
and `resolved` is a type-level `Bool` that differentiates
source (`False`) and "compiled" (`True`) terms
only within the off-chain translation process realm:

```haskell
type family DSLValue (resolved :: Bool) script value where
  DSLValue False script value = ConstraintDSL script value
  DSLValue True _script value = value

type family DSLPattern (resolved :: Bool) script value where
  DSLPattern False script value = ConstraintDSL script value
  DSLPattern True _ value = Void
```

As we can see, during the **off-chain translation**, regular __values__ are
evaluates to `value` type parameter of the source `ConstraintDSL script value`
and the __patterns__ are completely eliminated.
It's possible because the state is always known so all condition
and pattern matching branches can be resolved during the translation
and all values can be eventually evaluated.
For __on-chain__ compilation these difference doesn't play any role and
actual computation will be performed by an on-chain script.

Now that we have these terms, we can define the data type for transaction
constraints:

```haskell
data TxConstraint (resolved :: Bool) script
  = Utxo
      { kind :: UtxoKind
      , spec :: Utxo resolved script
      , value :: DSLValue resolved script Value
      }
  ...
  | MainSignerNoValue (DSLValue resolved script PubKeyHash)
  | If ...
  | MatchBySpine ...
  | Error Text
  | Noop
```

Using some additional data types like `Utxo` an `UtxoKind`  different conditions
that a particular transition require can be expressed:
1. `Utxo` allows describing particular inputs and/or outputs, including  script's
continuing output.
2. `MainSignerNoValue` (and some similar) allows to require a signature.
3. Finally, `If` and `MatchBySpine` bring ability to dynamic constraints.
Here we can naturally observe `DSLPattern` being used as the first argument and
`TxConstraint` being used recursively:
4. `Error` and `Noop` come in very handy together with `If` and `MatchBySpine`
(see later).

``haskell`
| If
    -- | Condition
    (DSLPattern resolved script Bool)
    -- | Then block
    (TxConstraint resolved script)
    -- | Else block
    (TxConstraint resolved script)
```

The definition of the CEM state machine is introduced as an instance of
`CEMScript` type class by providing a value for type `CEMScriptSpec` defined
as a map of possible transitions and constraints they impose
(slightly simplified here for readability) :

```haskell
type CEMScriptSpec resolved script =
  ( Map
      (Transition script)
      [TxConstraint resolved script]
  )
```

### Auctioning dApp example

Now we are ready to define a sample one-script dApp using with **`CEM Script`**
by defining a corresponding CEM state machine.
We are going to build a simple English auction application.
Let's start with **states**.
For any state an arbitrary (on-chain representable) data can be attached,
and in our case we are going to define and use `Bid` data type that contains
the bid amount and the bidder public key:

```haskell
data Bid = MkBet
  { bidder    :: PubKeyHash
  , bidAmount :: Integer
  }
```

So states starting with `CurrentBid` bear a `Bid`, being an initial zero bid
for `CurrentBid` and the winning bid for `Winner` state.

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

Now that we have states defined we can define a set of possible **transitions**,
that move the state.
Some of them may require arguments that should be passed when a transition happens:

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

`Start` transition brings the starting zero bid already mentioned, whereas every
`MakeBid` one takes a new bid run by a bidder. Notice that transitions themselves
don't reference states. They are bound together via constraints using a value of
a underlying map within `CEMScriptSpec` type as a part of `CEMScript instance
(in practice smart constructors are used to make instances of `TxConstraint`
not bare constructors we saw in the previous section):

```haskell
  transitionSpec :: CEMScriptSpec False SimpleAuction
  transitionSpec =
    let
      buyoutBid = ctxState.bid

      initialBid =
        cOfSpine
          MkBetSpine
          [ #bidder ::= ctxParams.seller
          , #bidAmount ::= lift 0
          ]

      auctionValue = cMinLovelace @<> ctxParams.lot
     in
      Map.fromList
        [
          ( CreateSpine
          ,
            [ spentBy ctxParams.seller cMinLovelace cEmptyValue
            , output (ownUtxo $ withNullaryState NotStartedSpine) auctionValue
            ]
          )
        ,
          ( StartSpine
          ,
            [ input (ownUtxo $ inState NotStartedSpine) auctionValue
            , output (ownUtxo $ withState CurrentBidSpine [#bid ::= initialBid]) auctionValue
            , signedBy ctxParams.seller
            ]
          )
        ...
```

If we ignore the word __spine__ and an exotic operator in this code,
it should be quite self-explaining. The first transition requires that a `seller`
(from script parameters, the part we omitted to keep the article shorter)
should sign the transaction and spend `cMinLovelace`, and one output should go
the the script, which is stated by `ownUtxo` with `NonStarted` state and
`acutionValue`. That way, this initial transition doesn't have the source state.
Now, the second `Transition` consumes the output we've just created
and sends it back with the updated state, now it will be `CurrentBid` and
the bid will be set to the intial UTxO. Based on that definition a state
diagram can be generated using CEM Script.
Initial and final states are represented on diagrams explicitly due to `graphviz`
limitations, ans we know they are virtual and don't exist in reality:

TODO: image: auction-state-graph.svg

### The rest of transitions

Let's wrap up the rest of the transitions which are going to be more interesting
and feature more flexibility. Next comes `MakeBid` transition that corresponds
to a bidder playing a new bid:

```haskell
...
( MakeBidSpine
,
  [ input (ownUtxo ...
  , byFlagError
      (ctxTransition.bid.bidAmount @<= ctxState.bid.bidAmount)
      "Bid amount is less or equal to current bid"
  , output (ownUtxo ...
  , signedBy ctxTransition.bid.bidder
  ]
)
...
```

In addition to things we have already seen like `input`, `output`, and `signedBy`
here we can observe an example of a conditional constraint,
that checks that the amount of the new bid
(comes from `ctxTransition`, remember an argument of `MakeBid` constructors)
is strictly bigger then the previous one (which comes from the `ctxState`).
This constraint is morally an error if the condition is not met.
`byFlagError`'s definition justifies the existence of `Noop` constraint:

```haskell
byFlagError ::
  ConstraintDSL script Bool -> Text -> TxConstraint False script
byFlagError flag message = If flag (Error message) Noop
```

`Close` transition won't bring anything new:

```haskell
...
( CloseSpine
,
  [ input ...
  , output
      ( ownUtxo
          $ withState WinnerSpine [#bid ::= ctxState.bid]
      )
      auctionValue
  , signedBy ctxParams.seller
  ]
)
...
```

In contrast, the last transition `Buyout` opens up more tricks:

```haskell
...
( BuyoutSpine
,
  [ input (ownUtxo $ inState WinnerSpine) auctionValue
  , offchainOnly
      ( spentBy
          buyoutBid.bidder
          ( cMkAdaOnlyValue buyoutBid.bidAmount
              @<> cMinLovelace
          )
          cEmptyValue
      )
  , output
      (userUtxo buyoutBid.bidder)
      (cMinLovelace @<> ctxParams.lot)
  , output
      (userUtxo ctxParams.seller)
      (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
  ]
)
...
```

Here we can see that two outputs go to bidder and seller with the `lot` and and
`bidAmount` correspondingly. From on-chain perspective it's not important where
exactly `bidAmount` comes from - probably someone else can pay their money on
bahalf of bidder. At the same time we have to give some intructions to off-chain
machinery how to build the transaction. By using `offchainOnly` helper
(which is defined as `offchainOnly c = If IsOnChain Noop c`) we can delimit
constraints that are supposed to be used only when running transaction building.

## Back to testing: two steps

### Step one: model-based testing

How can we get assurance that the definition we came up with is sound
in terms of bugs it can have which would potentially prevent liveness or
lead to crashes?

Probably, by far the greatest merit of CEM-based approach to developing dApps
we've just seen is the fact that it can easily be turned
into an "ideal world" model for the application.
Indeed, having the list of possible transitions we can
easily generate arbitrary sequnces of them, and then effectively shrink
them down to a small subset of ones that "makes sense".
To do that we need a decision function that can check whether a particular
transition is sound being provided with the current state of execution
and the definition of CEM machine.
With a big enough number of meaningful scenarious at hand, we can execute
them against the model and the real application simultaneously
to check that the both can handle them and that the both do that in the same way.
This approch is widely known as __model-based testing__.
But what is that __real applicaton__ we just mentioned?
In the real world, it is an application deployed on Cardano mainnet or testnet,
but it is utterly ineffective even in terms of traditional happy-path testing.
Private testnets are also cumbersome and slow.
And it becomes literally infeasible when one needs to run thousands of scenarious.
like in our case. Here CLB comes into play.
But let's get back to the decision function and other bits we need.





sequence should be verified by running  makes sense by replaying it and trying by checking whether corresponding constraints
can be interpreted.

```haskell
data CEMAction script = MkCEMAction (Params script) (Transition script)
```


### Mutation-based property testing: testing CEM Scripts internals

Link to Hydra article

or make stealing of funds possible

The correct definition of the `Buyout`

```haskell
...
( BuyoutSpine
,
  [ input (ownUtxo $ inState WinnerSpine) auctionValue
  , offchainOnly
      ( spentBy
          buyoutBid.bidder
          ( cMkAdaOnlyValue buyoutBid.bidAmount
              @<> cMinLovelace
          )
          cEmptyValue
      )
  , if'
      (ctxParams.seller `eq'` buyoutBid.bidder)
      ( output
          (userUtxo ctxParams.seller)
          (cMinLovelace @<> ctxParams.lot)
      )
      ( output
          (userUtxo buyoutBid.bidder)
          (cMinLovelace @<> ctxParams.lot)
      )
  , if'
      (ctxParams.seller `eq'` buyoutBid.bidder)
      noop
      ( output
          (userUtxo ctxParams.seller)
          (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
      )
  ]
)
...
```


## Unused

To ease the digestion of the idea imagine that `State` is a datum holding the state
and the `Input` is a redeemer providing the transition signal. .



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