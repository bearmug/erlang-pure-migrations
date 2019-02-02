-module(epgsql_migrations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
  dummy_test
].

dummy_test(_Opts) -> ok.