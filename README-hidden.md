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

