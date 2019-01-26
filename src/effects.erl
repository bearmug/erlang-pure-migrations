-module(effects).

-export([addOne/1]).



-spec addOne(F :: fun(() -> integer())) -> R :: fun(() -> integer()).
addOne(F) -> fun() -> F() + 1 end.