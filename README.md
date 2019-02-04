# Erlang ❤ pure database migrations
> Database migrations engine. Effects-free.

[![Build Status](https://travis-ci.org/bearmug/erlang-pure-migrations.svg?branch=master)](https://travis-ci.org/bearmug/erlang-pure-migrations) [![Coverage Status](https://coveralls.io/repos/github/bearmug/erlang-pure-migrations/badge.svg?branch=master)](https://coveralls.io/github/bearmug/erlang-pure-migrations?branch=master)

Migrate your Erlang application database with no effort.
This amazing toolkit has [one and only](https://en.wikipedia.org/wiki/Unix_philosophy)
purpose - consistently upgrade database schema, using Erlang stack.
As an extra - do this in "no side-effects" mode.

# Table of content
- [Current limitations](#current-limitations)
- [Quick start](#quick-start)
  * [Compatibility table](#compatibility-table)
  * [Live code samples](#live-code-samples)
    + [Postgres and epgsql/epgsql sample](#postgres-and--epgsql-epgsql--https---githubcom-epgsql-epgsql--sample)
- [No-effects approach and tools used to achieve it](#no-effects-approach-and-tools-used-to-achieve-it)
  * [Tool #1: effects externalization](#tool--1--effects-externalization)
  * [Tool #2: make effects explicit](#tool--2--make-effects-explicit)
- [Functional programming abstractions used](#functional-programming-abstractions-used)
  * [Functions composition](#functions-composition)
  * [Functor applications](#functor-applications)
  * [Partial function applications](#partial-function-applications)

# Current limitations
 * **up** transactional migration available only. No **downgrade**
 or **rollback** possible. Either whole **up** migration completes OK
 or failed and rolled back to the state before migration.
 * migrations engine **deliberately isolated from any specific
 database library**. This way engine user is free to choose from variety
 of frameworks (see tested combinations [here](#compatibility-table)) and so on.

# Quick start
Just call `engine:migrate/3` (see specification [here](src/engine.erl#L9)), providing:
 * `Path` to migration scripts folder (strictly and incrementally enumerated).
 * `FTx` transaction handler
 * `FQuery` database queries execution handler

## Compatibility table
| Database dialect | Library | Example |
| -------------- | ------ | ------- |
| postgres  | [epgsql/epgsql:4.2.1](https://github.com/epgsql/epgsql/releases/tag/4.2.1) | [epgsql test]((test/epgsql_migrations_SUITE.erl))

## Live code samples
### Postgres and [epgsql/epgsql](https://github.com/epgsql/epgsql) sample
<details>
  <summary>Click to expand</summary>

  ```erlang
  Conn = ?config(conn, Opts),
  MigrationCall =
    engine:migrate(
      "scripts/folder/path",
      fun(F) -> epgsql:with_transaction(Conn, fun(_) -> F() end) end,
      fun(Q) ->
        case epgsql:squery(Conn, Q) of
          {ok, [
            {column, <<"version">>, _, _, _, _, _},
            {column, <<"filename">>, _, _, _, _, _}], Data} ->
              [{list_to_integer(binary_to_list(BinV)), binary_to_list(BinF)} || {BinV, BinF} <- Data];
          {ok, [{column, <<"max">>, _, _, _, _, _}], [{null}]} -> -1;
          {ok, [{column, <<"max">>, _, _, _, _, _}], [{N}]} ->
            list_to_integer(binary_to_list(N));
          [{ok, _, _}, {ok, _}] -> ok;
          {ok, _, _} -> ok;
          {ok, _} -> ok;
          Default -> Default
        end
      end),
  ...
  %% more preparation steps
  ...
  %% migration call
  ok = MigrationCall(),

  ```
Also see examples from live epgsql integration tests
[here](test/epgsql_migrations_SUITE.erl)
</details>

# No-effects approach and tools used to achieve it
Oh, **there is more!** Library implemented in the [way](https://en.wikipedia.org/wiki/Pure_function),
that all side-effects either externalized or deferred explicitly. Reasons
are quite common:
 * bring side-effects as close to program edges as possible. Which may
 mean enhanced code reasoning, better bugs reproduceability, etc...
 * simplify module contracts testing
 * library users empowered to re-run idempotent code safely. Well, if
 tx/query handlers are real ones - execution is still idempotent (at
 application level) and formally pure. But purity maintained inside
 library code only. One call is to be issued anyway - migrations
 table creation, if this one does not exists.

## Tool #1: effects externalization
There are 2 externalized kind of effects:
 * transaction management handler
 * database queries handler
Although, those two can`t be pure in real application, it is fairly
simple to replace them with their pure versions if we would like to
(for debug purposes, or testing, or something else).

## Tool #2: make effects explicit
Other effects (like file operations) are deferred in bulk with outcome
like:
 * pure referentially-transparent program actions composed only. Impact
 or any communication with external world postponed until later stages
 * library users decide when they ready to apply migration changes.
 Maybe for some reason they would like to prepare execution ->
 prepare migrations folder content -> run migrations.

# Functional programming abstractions used
## Functions composition
This trick is quite useful if someone would like to compose two functions
without their actual execution (or without their application,
alternatively speaking). This pretty standard routine may look like below
(Scala or Kotlin+Arrow):
```scala
val divideByTwo = (number : Int) => number / 2;
val addThree = (number: Int) => number + 3;
val composed = addThree compose divideByTwo
```
Simplistic Erlang version:
```erlang
compose(F1, F2) -> fun() -> F2(F1()) end.
```

## Functor applications
There area few places in library with clear need to compose function **A**
and another function **B** inside deferred execution context. Specifics is
that **A** supplies list of objects, and **B** should be applied to each of
them. Sounds like some functor **B** to be applied to **A** output, when
this output is being wrapped into future execution context. Two cases
of this appeared in library:
 * have functor running and produce nested list of contexts:
```erlang
%% Map/1 call here produces new context (defferred function call)
map(Generate, Map) -> fun() -> [Map(R) || R <- Generate()] end.
```
 * flatten (or fold) contexts (or function calls) list to a single one:
```erlang
flatten(Generate) -> fun() -> [ok = R() || R <- Generate()], ok end.
```
## Partial function applications
Partial application is very useful, in case if not all function arguments
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
Partial = fun(V_F) -> do_migration(Path, FQuery, V_F) end,
```