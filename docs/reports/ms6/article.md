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

## Emulator core: `clb` library

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

## CEM Script: CLB as a backend for mutation-based property testing

The crucial property of CLB we wanted to preserve was speed
since it becomes critical for property-based testing.
In this section we present the case of **CEM Script** project
which brings an interesting example of how dApps on Cardano
could be tested and how CLB comes handy in doing that.

### CEM Machines

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

### Detour: DSL to define CEM machines

#### Buliding blocks

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

```haskell
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

#### Auctioning dApp example

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

#### The rest of transitions

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
      auctionValue
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
bahalf of the bidder.
At the same time we have to give some intructions to off-chain
machinery how to build this transaction. By using `offchainOnly` helper
(which is defined as `offchainOnly c = If IsOnChain Noop c`) we can delimit
constraints that are supposed to be used only when running transaction building
making them invisible for the on-chain code compiler.

### Back to testing: two steps

In the next two sections we are moving back to the testing,
starting with reagulr model-based aproach
and then augmenting it with mutations.

#### Step one: model-based testing

How can we get assurance that the definition of the state machine we came up with
is sound in terms of bugs which would potentially prevent liveness or lead to crashes?

Probably, by far the greatest merit of CEM-based approach to developing dApps
we've just seen is the fact that its parts can easily be turned
into an "ideal world" model for the application.
Indeed, having a list of possible transitions we can easily generate
arbitrary sequnces of them,
and then effectively shrink them down to a realtively small set of
those that "make sense".
With a big enough number of meaningful scenarious at hand, we can execute
them against the model and the real application
and check that the both can handle them
and even that the both do it the same way[^ TODO: this is not tre case for now].
This approch is widely known as __model-based testing__.

Let's first focus on the question of how we can discern meaningful
sequences of transitions from a whole bunch of random ones we generate.
To do that we need a decision function that can check whether a particular
transition is sound being provided with the current state of execution
and the definition of CEM machine.
In [quckcheck-dynamic](TODO:) library it's known as
`precondition` method of `StateModel` type-class
that takes the current state, the action in question and tells
whether it's a meningful one over the state provided:

```haskell
precondition ::
  (CEMScriptArbitrary script) =>
  ScriptState script ->
  Action (ScriptState script) a ->
  Bool
```

Turns out we can implement it in a very straightforward manner by reusing
some functions from off-chain machinery of **CEM Script**. Let's see how it works.
The entry point called `resolveTx` takes a specification for a prospective transaction and
tries to build a transaction ready for submitting. You can think of `TxSpec`
data type of a list of __transitions__ and `ResolvedTx` as final recipe
to build a `cardano-api` transaction:

```haskell
resolveTx ::
  forall m.
  (MonadQueryUtxo m) =>
  TxSpec ->
  m (Either TxResolutionError ResolvedTx)
```

This function works in four steps:

1. Compiling constraints. For every transition within the specification
(as we mentioned a transaction can operate over several scripts
simultaneously and independently, and each script runs its own transition):
  * Finds the definition of the transition, including the list of constraints
as `[TxContraints (resolved :: False)]`.
  * Obtains the current on-chain state of the machine and checks whether it satisfies
relevant constraints, i.e. constraints of form `input ownUtxo` that specifies
the own input of the script.
  * Tries to compile constraints, turning them from
`[TxConstraint (resolved :: False)]` to `[TxConstraint (resolved :: True)]`.
2. Concatenates and deduplicates constraints into one list.
3. Translates each `TxConstraint` on the list into a building block of a future
transaction called `Resolution`, querying blockchain state in some cases.
4. Finally, use the list of `Resolution` to build the `ResolvedTx`.

Whereas both steps (1) and (3) may fail, exactly step (1) contains the logic
we are interested in. It ensures that the constraints can be translated in a
meaningful way which is exactly the criterion of the transition's viability.
So if can provide step (1) with the current state we will be able to know
whether a particular transition could be valid. That way we can
boil down the whole ideal world model to the machine state (plus some additions)
which is captured by `ScriptState script` data type[^TODO: link to StateMachine.hs].
Step (1) is implemented as `compileActionConstraints` function, so we can write
`precondition` method additionally checking that the tar:

```haskell
precondition
  (ScriptState {dappParams, state, finished})
  (ScriptTransition transition) =
    let
      cemAction = MkCEMAction (params dappParams) transition
      compiled = compileActionConstraints state cemAction
      in isRight compiled
```

The rest of the `StateModel` instance is pretty straightforward to implement.
Now that we have the model, what is that __real applicaton__ we mentioned?
In the real world, it is an application deployed on Cardano mainnet or testnet,
but it is utterly ineffective even when it comes to more traditional testing.
Private testnets are also cumbersome and slow.
And it becomes literally infeasible when one needs to run thousands of scenarious
like in our case. Here CLB comes into play.
Since we have off-chain part that can build transactions being provided with
a simple query layer we can use bare `clb` library.
In `quickcheck-dynamic` there is a type class `RunModel` that represents a real
model you want to test. By wiring `clb` library methods into `RunModel` instance
we can accomplish our goal. At its core there is one methid called `perform`
with rather convoluted signature which we are omitting here though an interested
reader can find it in CEM Script sources [TODO: links].

Finally, we can define the property which states that any meaningful sequence
should succeed:

```haskell
dynamicSpec = describe "Quickcheck Dynamic" $ do
  it "Auction random trace works on CLB" $ do
    quickCheckDLScript $ do
      anyActions_
  where
    quickCheckDLScript :: DL (ScriptState SimpleAuction) () -> IO ()
    quickCheckDLScript dl = do
      actors <- execClb getTestWalletSks
      result <- quickCheckResult $ withMaxSuccess 100 $ runDLScript $ do
        _ <- action $ SetupConfig $ MkTestConfig { actors }
        dl
      isSuccess result `shouldBe` True

    runDLScript :: DL (ScriptState SimpleAuction) () -> Property
    runDLScript dl =
      forAllDL
        dl
        (runActionsInClb @SimpleAuction genesisValue)

    genesisValue = lovelaceToValue 300_000_000_000
```

Let run the test-suite in `cem-script` repository to see whether it passes:

```bash
$ cabal run cem-script-test
...
*** Failed! Falsified (after 88 tests and 5 shrinks):
do action $ SetupConfig
   action $ SetupParams
   action $ ScriptTransition Create
   action $ ScriptTransition Start
   action $ ScriptTransition Close
   action $ ScriptTransition Buyout
   pure ()
...


The machine terminated because of an error...

[ "Matched spine: BuyoutSpine"
, "Checking transition BuyoutSpine"
, "Checking constraint Utxo {kind = Out, spec = UserAddress Ask Params.seller, value = somePlutarchCode (somePlutarchCode (Pure (3000000))) (Ask Params.lot)}"
, "Constraint check failed"])]))
```

And it does not. According the test scenario we see in the output,
if no bids are happened (besides the initial zero bid)
_buying out_ doesn't work as expected.
And indeed, the definition of that transaction
requires two outputs - one for the bidder and one for the seller:

```haskell
...
  , output
      (userUtxo buyoutBid.bidder)
      auctionValue
  , output
      (userUtxo ctxParams.seller)
      (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
...
```

When both addresses are the same output constraints can't be verified since two
outputs get into the way of each other validation.
Also we can notice that `spentBy` constraint is also redudndant in case no bids
are made and we conditionally replace it with simplier `signedBy`
(we cannot use `noop` here since at least one signature is required for a transaction).
So we have to handle this case with a condition
and the final definition of the `Buyout` transition is:

```haskell
...
( BuyoutSpine
  ,
    [ input (ownUtxo $ inState WinnerSpine) auctionValue
      offchainOnly
        (if'
          (ctxParams.seller `eq'` buyoutBid.bidder)
          (signedBy ctxParams.seller)
          (spentBy
            buyoutBid.bidder
            (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
            cEmptyValue
          )
        )
    , output
        (userUtxo buyoutBid.bidder) -- seller is initial zero bidder
        (cMinLovelace @<> ctxParams.lot)
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

So we have just seen how model-based approach can be applied for testing
on-chain scripts generated by **CEM Script** compiler.
With some efforts an external implementation also can be tested this way.
The only requirement is that the corresponding CEM machine
should be defined to serve as a model and an external script implementation
should be used in place of automatically generated one.

This kind of testing can ensure that all sensible sequnces of transitions
work correctly, though other properties can be expressed as well.
In the next section we are going to add mutations to this approach
to cover also negative cases.

#### Mutation-based property testing: negative scenarious and more

Now that we have checked all meaningful sequences, we can go a bit further.
All sequences we used so far were positive in a sense that they
were obtained from the definition of the CEM machine.
Now, we'd like to be sure that the same sequences will fail if we introduce
an arbitrary mutation that makes a transaction invalid.
But how can we turn a valid transaction into invalid one that looks very
close to the original one?
In the land of CEM it's natural to start any experiments with constraints.
A couple of observations based on the fact that each transition emits
a list of constraints for every transition:

1. The order of constrainsts should not matter, if we shuffle them arbitrarily
the result should remain the same.
2. All non-noop constraints are important, if we remove any the sequnce should
stop working.

As a side note, we can go down to the level of individual constraints
and start tweaking them individually with a specific strategies
but this is out of the scope of the article [^3].

So how can we add mutations to the test-suite we already built?
Let's start with defining the following data type
and a couple of additional functions:

```haskell
data TxMutation
  = RemoveConstraint {num :: Int}
  | ShuffleConstraints {shift :: Int}

-- Applies a mutation to a compiled transition
applyMutation :: Maybe TxMutation -> [TxConstraint True script] -> [TxConstraint True script]

-- Determines whether a mutation should break a sequnce
isNegativeMutation :: Maybe TxMutation -> [TxConstraint True script] -> Bool
```

To plug these mutations in the testing machinery we need to do two things:
1. Teach the model to generate mutated scenarious and discern positive and
negative ones.
2. Apply a mutation before submitting a transaction.

First of all we need to extend the definition of `Action state a` associated
type family in `StateModel` which represents _actions_ by adding possible mutation
to its `ScriptTransition` constructor
(the other two constructors represent
two preliminary actions used to set up a script):

```haskell
data Action (ScriptState script) output where
  SetupConfig :: TestConfig -> Action (ScriptState script) ()
  SetupParams :: Params script -> Action (ScriptState script) ()
  ScriptTransition ::
    Transition script ->
    Maybe TxMutation ->
    Action (ScriptState script) ()
```
Now we are ready to teach the model to generate actions with mutations.
Obvisouly, the first step is to add arbitrary mutations to `arbitraryAction`
method. We need to compile constraints first, so again we can use
`compileActionConstraints` function we saw before to do the job
and then just pick a mutation randomly:

```haskell
...
genMutation :: Transition script -> Gen (Maybe TxMutation)
genMutation transition =
  let cemAction = MkCEMAction (params dappParams) transition
    in case compileActionConstraints state cemAction of
        Right cs ->
          Gen.oneof
            [ return Nothing
            , Just . RemoveConstraint
                <$> Gen.chooseInt (0, length cs - 1)
            , Just
                <$> ( ShuffleConstraints
                        <$> Gen.chooseInt (1, length cs)
                    )
            ]
        Left _ -> return Nothing
...
```

Next we have to implement `validFailingAction` method of `StateModel` class.
which is basically a _decision function_ like `precondition` but for actions that
can be meaningfully run but are supposed to fail.
An action will run as a _negative_ if 'precondition' fails for it
and 'validFailingAction' succeeds.
Such actions should not modify the model state
(though there is `failureNextState` method in case they should).
We consider action is negative if its mutated constraints fail to compile (`Left _`)
or mutation is negative and doesn't terminate the machine (`Right cs`):

```haskell
validFailingAction ::
  (CEMScriptArbitrary script) =>
  ScriptState script ->
  Action (ScriptState script) a ->
  Bool
validFailingAction
  (ScriptState {dappParams, finished, state})
  (ScriptTransition transition mutation) =
    let cemAction = MkCEMAction (params dappParams) transition
        cs' = compileActionConstraints state cemAction
      in case cs' of
          Right cs -> isNegativeMutation mutation cs && isJust state && not finished
          Left _ -> True
validFailingAction _ _ = False
```

The last thing is to finally apply a mutation upon submitting a transaction and
handle some corner cases like the deletion of signature contraint by mutation:

```haskell
mutateResolveAndSubmit :: m (Either TxResolutionError TxId)
mutateResolveAndSubmit = runExceptT $ do
  let cemAction = MkCEMAction (params dappParams) transition
  cs' <- ExceptT $ return $ compileActionConstraints state cemAction
  let
    (cs, _) = applyMutation mutation cs'
    mbSignerPKH = getMbMainSigner cs
  specSigner <- case mbSignerPKH of
    Nothing -> ExceptT $ pure $ Left NoSignerError
    Just signerPKH -> pure $ findSkForPKH (actors $ config dappParams) signerPKH
  resolutions <- mapM (process cemAction) cs
  let resolvedTx = (construct resolutions) {signer = specSigner}
  result <- first UnhandledSubmittingError <$> lift (submitResolvedTx resolvedTx)
  let spec = MkTxSpec [MkSomeCEMAction cemAction] specSigner
  lift $ logEvent $ SubmittedTxSpec spec (mapLeft (const ()) result)
  ExceptT $ return result
```

Re-running the test suite with mutations enabled doesn't reveal any issues this time
and reports a fair amount of mutations being used during testing:

```
Mutations (4006 in total):
66.15% JustSpine
33.85% NothingSpine
```

In addition to the task of testing a particular script definition this technique
allowed us to ensure on-chain and off-chain parts match each other when it comes
to negative scenarious and to demostrate that the order of constraints doesn't
matter since it was one of the assumptions we used
during the **CEM Script** development.
Indeed, according `validFailingAction` function a transition is considered
failing when its off-chain compilation fails. By ensuring that it also fails
in a real application we can conclude the both behave the same way.

## Unified testing with Atlas

The next use case of testing dApps with CLB emulator we are going to expore
is more traditional and can be thought of as a replacement for PSM.
The latter had their own machineryfor building transactions.
This fact caused significant duplication of work
since developers needed to repeat off-chain logic twice -
first for test within PSM and then for the application itself.
Moreover, whereas this approach was good for testing on-chain code
the off-chain part (real code that was used within the application)
was effectively untested.
Partialy this problem was solved by Atlas' team and off-chain logic could be
reused in PSM-based test.
But definitions of test-cases for PSM and private testnet were
completely different, which again led to duplication of work or forced developers
to use only one testing backend, an emulator or a private network.
Unified testing feature was conceived as a solution to these problems.

Let's list all the components we deal with when testing an dApp on Cardano
and see how they play in the unified testing:

1. An application under testing, which includes:
  1.1. Smart contracts
  1.2. Off-chain code operations, that build transactions (or their skeletons)
  1.3. Glue code to call off-chain operationd from UI/wallets
2. A test suite, which includes:
  2.1. Actions that can run operations in test environment without UI
  2.2. Test-cases that consists of:
    * A prelude sequence of actions that prepare the state for test-case
    * A test condition to decide whether a test-case pass

In unified testing:
* All components of an application except glue code (1.3) are covered.
* The test suite can be run using different backends, including CLB and a testnet.

In the following section, we are going to introduce briefly
a unified testing by testing a trivial spending transaction.

For a more realistic example please refer to
[**testing**](https://atlas-app.io/getting-started/testing#overview-of-unified-testing-in-atlas)
page on Atlas' docs web site.
You can find sources of the betting application and its test suite in Atlas'
repository [here](https://github.com/geniusyield/atlas/tree/main/tests-unified).

### Trivial transaction example

Unified testing feature provide two functions to build test cases:

```haskell
mkPrivnetTestFor :: Setup -> TestName -> (TestInfo -> GYTxGameMonad a) -> TestTree
mkTestFor ::                 TestName -> (TestInfo -> GYTxGameMonad a) -> TestTree
```

Both functions have similar signatures - they take a name for a test case
and a continuation function of type `TestInfo -> GYTxGameMonad a`.
Then they internally setup testing environment represented with
`TestInfo` data type to run the continuation in it.

`mkPrivnetTestFor` additionally takes a value of type `Setup`
that contains an instance of a private network,
whereas `mkTestFor` that makes a test case backed by CLB - does not.
This highlights an important distinction in thr way they work:

* `mkTestFor` spawns a new instance of CLBb on every call and every test case is
run in a fresh (new) ledger state.
* `mkPrivnetTestFor` is supposed to be run inside a helper function
`withPrivnet` which spins up one and only private testnet and executes
the whole test suite using it.

We are going to write a test that simply sends 100 ADA from
a testing wallet to some arbitrary address.
Our off-chain code works in
[`GYTxQueryMonad` monad](https://atlas-app.io/getting-started/operations#interlude---gytxquerymonad)
that brings notion of __own addresses of a particular wallet__
and yields a transaction skeleton that should has an output and
should be singed by a key of the wallet:

```haskell
mkTrivialTx :: GYTxUserQueryMonad m => m (GYTxSkeleton 'PlutusV2)
mkTrivialTx = do
  -- The first own address
  addr <-
    maybeM
      (throwAppError $ someBackendError "No own addresses")
      pure
      $ listToMaybe <$> ownAddresses
  -- Its public key hash
  pkh <- addressToPubKeyHash' addr
  -- Random address
  let targetAddr = unsafeAddressFromText "addr_test1qr2vfnt..."
  return $
    mustHaveOutput
      ( GYTxOut
          { gyTxOutAddress = targetAddr
          , gyTxOutValue = valueFromLovelace 100_000_000
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      )
      <> mustBeSignedBy pkh
```

Now we can write an test that checks that wallet indeed looses 100 ADA
when the transaction happens. This function effectively combines
the action (2.1) and the test condition (2.2) test for simplicity's sake,
though one should keep those two parts separate since quite often actions
are reused:

TODO: factor out the action as binding in where

```haskell
{- | Trace for a super-simple spending transaction.
-}
simpleTxTest :: GYTxGameMonad m => TestInfo -> m ()
simpleTxTest (testWallets -> Wallets {w1}) = do
  -- The condition
  withWalletBalancesCheckSimple [w1 := valueFromLovelace (-100_000_000)]
    -- The action
    . asUser w1
    $ do
      skeleton <- mkTrivialTx
      gyLogDebug' "" $ printf "tx skeleton: %s" (show skeleton)
      txId <- buildTxBody skeleton >>= signAndSubmitConfirmed
      gyLogDebug' "" $ printf "tx submitted, txId: %s" txId
```

Here we get access to wallet `w1` from the environment and runs the transaction
on its behalf using function `asUser`. Finally, we can make a test with
`mkPrivnetTestFor` or `mkTestFor`:

```haskell
clbSuite :: TestTree
clbSuite = testGroup "Group"
    [ mkTestFor "Simple tx" simpleTxTest
    ]

testnetSuite :: Setup -> TestTree
testnetSuite setup = testGroup "Place Group"
    [ mkPrivnetTestFor_ "Simple tx" simpleTxTest
    ]
 where
  mkPrivnetTestFor_ = flip mkPrivnetTestFor setup
```

Having the ability to switch backends allows developers to start off with
quick and light CLB emulator and later switch transparently to a real testnet
once a need arises if emulator's functionality is not enough.
Otherwise, at the final stage of development the whole test suite can be
run against a real testnet with no need of rewriting any its parts.
Out of couriosity, let's check how long does it take to run our trivial
test suite against the emulator and a real testnet:

```bash
$ time cabal run atlas-unified-tests

Simple tx: OK (0.12s)

  Emulator log :
  --------------
  Slot 0:
    ...
    [DEBUG]  utxosAtAddress, refs: [TxOutRef {txOutRefId = 01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53, txOutRefIdx = 1}]
    [DEBUG]  fee: 178525 lovelace
             validity range: (Nothing,Nothing)
             inputs:
               - ref: 01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#1
                 addr: addr_test1vzgr3pyndlkgdnxnucfnu2y7072skuy6vzlkftc0nhrygyqsj6qx8
                 value: ... + 1000000000000 lovelace
              ...
    [DEBUG]  encoded tx:
              84a400d901028182582001f4b788593d4f...
  Slot 1:
    [DEBUG]  tx submitted, txId: 7d487313607909080f4615edc9961287ee6eaaa761e98d30f9146b307655644b
    ...

All 1 tests passed (0.12s)

real	0m0.847s
user	0m0.727s
sys	  0m0.113s
```

As we can see, the test takes 0.85s to run, which is not instantenous, but
pretty quick in comparison with a testnet where the test itself takes four
times londer and additionally takes more than a minute and a half to start
testnet:

```bash
$ time cabal run atlas-unified-tests -- --testnet
Simple tx: OK (3.05s)
  Querying utxos At Addresses: [unsafeAddressFromText "addr_test1vz..."]
  ownAddr: unsafeAddressFromText "addr_test1vz..."
  GYTxSkeleton {...}
  tx skeleton: GYTxSkeleton {...}
  Querying utxos At Addresses: [unsafeAddressFromText "addr_test1vz..."]

All 1 tests passed (3.05s)

real	1m37.854s
user	0m20.597s
sys  	0m2.654s
```

## Emulating cardano-node: betting dApp on CTL

when the emulator mimics a real `cardano-node`
by maintaining an IPC socket
that supports a subset of Ouroboros mini-protocols.

## Useful Links

* [CLB repository]() on GitHub
* [CLB docs web-site]()
* [Atlas repository]() on GitHub
* [Atlas docs web-site]()
* [CTL]()
* [PSM repository]() on GitHub (archived)
* [The extended UTxO model] ()
*

TODO: link article in the documenatation

TODO: update Changelog

TODO: add badges to README

[^1]: Now the whole [_Plutus Application Framework_](https://github.com/IntersectMBO/plutus-apps)
is officially archived, but the emulator budded off as a
[standalone project](https://github.com/IntersectMBO/cardano-node-emulator).

[^2]: As ["The Extended UTXO Model"](https://iohk.io/en/research/library/papers/the-extended-utxo-model/)
paper explains, validators can be modeled as silghtly alternated
[Mealy machines](https://en.wikipedia.org/wiki/Mealy_machine)

[^3]: For more ideas on how mutation testing can be used to test validators
on Cardano see Arnaud Bailly's article
[Mutation-based TDD](https://abailly.github.io/posts/mutation-testing.html)

Tags: blockchain | cardano | testing