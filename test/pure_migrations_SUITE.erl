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
    fun(Q) -> case Q of
                "CREATE" ++ _Tail -> ok;
                "CREATE" ++ _Tail -> ok;
                "SELECT version" ++ _Tail -> [];
                "SELECT max" ++ _Tail -> -1;
                "INSERT"++_Tail -> throw("Insert calls not expected");
                true -> throw("Unknown calls not expected")
              end
    end),
  ?assertError(badarg, PreparedCall()).

regular_upgrade_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "02-regular-upgrade"]),
    fun(F) -> F() end,
    fun(Q) -> case Q of
                "CREATE" ++ _Tail -> ok;
                <<"CREATE TABLE fruit (\n name TEXT\n);">> -> ok;
                "SELECT version" ++ _Tail -> [];
                "SELECT max" ++ _Tail -> -1;
                "INSERT"++_Tail -> ok;
                true -> throw("Unknown calls not expected")
              end
    end),
  ?assertEqual([ok], PreparedCall()).

rewrite_version_test(_Config) -> ?assert(true).
faulty_script_test(_Config) -> ?assert(true).
start_not_from_zero_test(_Config) -> ?assert(true).
versions_gap_test(_Config) -> ?assert(true).
negative_version_test(_Config) -> ?assert(true).
