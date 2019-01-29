-module(pure_migrations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([
  wrong_filename_test/1,
  regular_upgrade_test/1,
  rewrite_version_test/1,
  faulty_script_test/1,
  start_not_from_zero_test/1,
  versions_gap_test/1,
  negative_version_test/1
]).

all() -> [
  wrong_filename_test,
  regular_upgrade_test,
  rewrite_version_test,
  faulty_script_test,
  start_not_from_zero_test,
  versions_gap_test,
  negative_version_test
].

wrong_filename_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "01-invalid-script-name"]),
    fun(F) -> F() end,
    query_fun([], ok)
  ),
  ?assertError(badarg, PreparedCall()).

regular_upgrade_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "02-regular-upgrade"]),
    fun(F) -> F() end,
    query_fun([], ok)
  ),
  ?assertEqual([ok], PreparedCall()).

rewrite_version_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "03-rewrite-version"]),
    fun(F) -> F() end,
    query_fun([{0, "00_very_first_script.sql"}], ok)
  ),
  ?assertError({badmatch, 1}, PreparedCall()).

faulty_script_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "00-default"]),
    fun(F) -> F() end,
    query_fun([], {error, system_fault, "Please check database connectivity"})
  ),
  ?assertError({badmatch, {
    error, system_fault, "Please check database connectivity"
  }}, PreparedCall()).

start_not_from_zero_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "04-invalid-start-version"]),
    fun(F) -> F() end,
    query_fun([], ok)
  ),
  ?assertError({badmatch, 0}, PreparedCall()).

versions_gap_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "04-invalid-start-version"]),
    fun(F) -> F() end,
    query_fun([{0, "00_very_first_script.sql"}], ok)
  ),
  ?assertError({badmatch, 1}, PreparedCall()).

negative_version_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "05-negative-version"]),
    fun(F) -> F() end,
    query_fun([], ok)
  ),
  ?assertError({badmatch, 0}, PreparedCall()).

query_fun(ExistingVersions, MigrationResponse) ->
  fun(Q) ->
    case Q of
      "CREATE" ++ _Tail -> ok;
      "SELECT version" ++ _Tail -> ExistingVersions;
      "SELECT max" ++ _Tail -> length(ExistingVersions) - 1;
      "INSERT" ++ _Tail -> ok;
      <<"CREATE TABLE fruit (\n name TEXT\n);">> -> MigrationResponse;
      true -> throw("Unknown calls not expected")
    end
  end.
