[![Build Status](https://travis-ci.org/bearmug/oleg-migrations.svg?branch=master)](https://travis-ci.org/bearmug/oleg-migrations)

# Oleg ❤ pure databases migrations
This amazing toolkit [main and only](https://en.wikipedia.org/wiki/Unix_philosophy)
purpose. This one is to migrate SQL databases with Erlang. And as a fun
part of the project - it is approached in "no side-effects" mode aka
purely funcitonal.

Please note - library provides migrations engine capabilities only. You
will need to pass two handlers (or functions) there:
 * transaction handler, which manages tx scope
 * queries handler, to run queries against database
For more details see short [specification](https://github.com/bearmug/oleg-migrations/blob/master/src/oleg_engine.erl#L7).

## Quick start
### Usage with epgsql TBD
### Alternative wrappers TBD

## Purely functional approach
Oh, there is more! Library implemented in the [way](https://en.wikipedia.org/wiki/Pure_function),
that all side-effects either externalized or deferred explicitly. Usually,
it is not how side-effects managed in Erlang. So, this part is experimental.
Goals are quite common and well-known:
 * bring side-effects as close to program edges as possible. (And get
 referential transparency, better bugs reproduceability, ...)
 * make unit testing simple as breeze
 * give library users opportunity to re-run idempotent code safely

### Purity tool #1: effects externalization
There are 2 externalized kind of effects:
 * transaction management handler
 * database queries handler
Although, those two can`t be pure in real application, it is failrly
simple to replace them with their pure versions if we would like to
(for debug purposes, or testing, or something else).

### Purity tool #2: make effects explicit
Other effects (file operations, like directory listing or file content
read) are deferred in bulk. This way 2 goals achieved:
 * pure actions sequence built and validated without any impact from
 external world
 * library users decides if regarding moment, when they ready to apply
 changes. Maybe for some reason they would like to prepare execution ->
 change migrations folder content -> run migrations.

### Used functional programming abstractions
Sure, Erlang is deeply funcitonal language. But at the same time, for
obvious reasons ( 1)not much people need tools like these 2)it is deadly
 simple to implement required abstractions on your own), there are no
(at least I did not manage to find) widely used functional primitives
Erlang library.

#### Functions composition
Abstraction quite useful if someone would like to compose two functions
without their actual nested execution (or without their application,
alternatively speaking). This pretty standard routine may look like below
(Scala or Kotlin+Arrow):
```scala
val divideByTwo = (number : Int) => number / 2;
val addThree = (number: Int) => number + 3;
val composed = addThree compose divideByTwo
```
To keep things close to the ground and avoiding infix notation, in
Erlang it is could be represented like:
```erlang
compose(F1, F2) ->
  fun() -> F2(F1()) end.
```
You may find library funcitonal composition example in a few locations
[here](https://github.com/bearmug/oleg-migrations/blob/make-engine-free-of-side-effects/src/oleg_engine.erl#L36).

#### Functor applications
There area few places in library with clear need to compose function *A*
and another function *B* inside deferred execution context. Specifics is
that *A* supplies list of objects, and *B* should be applied to each of
them. Sounds like some functor *B* to be applied to *A* output, when
this output is being wrapped into future execution context. Two cases
of this appeared in library:
 * have functor running and produce nested list of contexts:
```erlang
%% Map/1 call here produces new context (defferred function call in Erlang)
map(Generate, Map) ->
  fun() -> [Map(R) || R <- Generate()] end.
```
 * flatten (or fold) contexts to a single one:
```erlang
%% Flatten/1 call compactifies contexts and folds 2 levels to single one
flatten(Generate, Flatten) ->
  fun() -> [Flatten(R) || R <- Generate()] end.
```
#### Partial function applications
This technique is very useful, in case if not all function arguments
known yet. Or maybe there is deliberate decision to pass some of arguments
later on. Again, in Scala it may look like:
```scala
val add = (a: Int, b: Int) => a + b
val partiallyApplied = add(3, _)
```
Library code has very simplistic partial application, done for exact
arguments number (although it is easy to generalize it for arguments,
represented as list):
```erlang
partial(F, A, B) ->
  fun(C) -> F(A, B, C) end.
```
Exactly this feature helps [here](https://github.com/bearmug/oleg-migrations/blob/make-engine-free-of-side-effects/src/oleg_engine.erl#L19)
to pass particular migration to partially applied function. Therefore,
no need to care about already known parameters.