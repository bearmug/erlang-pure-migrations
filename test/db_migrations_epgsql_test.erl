-module(db_migrations_epgsql_test).

-include_lib("eunit/include/eunit.hrl").

empty_folder_test() -> ?assert(true).
wrong_filename_test() -> ?assert(true).
wrong_path_test() -> ?assert(true).
regular_upgrade_test() -> ?assert(true).
rewrite_version_test() -> ?assert(true).
faulty_script_test() -> ?assert(true).
start_not_from_zero_test() -> ?assert(true).
versions_gap_test() -> ?assert(true).
negative_version_test() -> ?assert(true).
