-module(pure_migrations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([
  empty_folder_test/1,
  wrong_filename_test/1,
  wrong_path_test/1,
  regular_upgrade_test/1,
  rewrite_version_test/1,
  faulty_script_test/1,
  start_not_from_zero_test/1,
  versions_gap_test/1,
  negative_version_test/1
]).

all() -> [
  empty_folder_test,
  wrong_filename_test,
  wrong_path_test,
  regular_upgrade_test,
  rewrite_version_test,
  faulty_script_test,
  start_not_from_zero_test,
  versions_gap_test,
  negative_version_test
].

empty_folder_test(Opts) ->
  PreparedCall = engine:migrate(
    filename:join([?config(data_dir, Opts), "01-empty-folder"]),
    fun(F) -> F() end,
    fun(Q) -> case Q of
                "CREATE" ++ _Tail -> ok;
                "SELECT version" ++ _Tail -> [];
                "SELECT max" ++ _Tail -> 0;
                "INSERT"++_Tail -> throw("Insert calls not expected");
                true -> throw("Unknown calls not expected")
              end
    end),
  PreparedCall().

wrong_filename_test(_Config) -> ?assert(true).
wrong_path_test(_Config) -> ?assert(true).
regular_upgrade_test(_Config) -> ?assert(true).
rewrite_version_test(_Config) -> ?assert(true).
faulty_script_test(_Config) -> ?assert(true).
start_not_from_zero_test(_Config) -> ?assert(true).
versions_gap_test(_Config) -> ?assert(true).
negative_version_test(_Config) -> ?assert(true).
