-module(oleg_engine_epgsql_test).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([empty_folder_test/1]).

all() -> [empty_folder_test].

empty_folder_test(_Config) -> ?assert(true).
%%wrong_filename_test() -> ?assert(true).
%%wrong_path_test() -> ?assert(true).
%%regular_upgrade_test() -> ?assert(true).
%%rewrite_version_test() -> ?assert(true).
%%faulty_script_test() -> ?assert(true).
%%start_not_from_zero_test() -> ?assert(true).
%%versions_gap_test() -> ?assert(true).
%%negative_version_test() -> ?assert(true).
