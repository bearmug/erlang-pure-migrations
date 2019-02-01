-module(pure_migrations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          invalid_location_test,
          no_number_filename_test,
          invalid_format_filename_test,
          regular_upgrade_test,
          rewrite_version_test,
          faulty_script_test,
          start_not_from_zero_test,
          versions_gap_test,
          negative_version_test
         ].

invalid_location_test(_Opts) ->
    PreparedCall = engine:migrate(
                     "\\invalid\location",
                     fun(F) -> F() end,
                     query_fun([], ok)
                    ),
    ?assertError(
       {badfun, {error,invalid_folder, {
                                        folder_supplied, "\\invalidlocation",
                                        error, {error, enoent}}}},
       PreparedCall()).

no_number_filename_test(Opts) ->
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "01-no-number-script-name"]),
                     fun(F) -> F() end,
                     query_fun([], ok)
                    ),
    ?assertError(
       {badfun, {error, invalid_filename, {
                                           expected, "<number>_<description>.sql",
                                           supplied, "wrong_migration_script_name.sql"}}},
       PreparedCall()).

invalid_format_filename_test(Opts) ->
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "01-invalid-format-script-name"]),
                     fun(F) -> F() end,
                     query_fun([], ok)
                    ),
    ?assertError(
       {badfun, {error, invalid_filename, {
                                           expected, "<number>_<description>.sql",
                                           supplied, "1V_wrong_migration_script_name.sql"}}},
       PreparedCall()).

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
    ?assertError(
       {badfun, {error,unexpected_version, {expected, 1, supplied, 0}}},
       PreparedCall()).

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
    ?assertError(
       {badfun, {error,unexpected_version, {expected, 0, supplied, 2}}},
       PreparedCall()).

versions_gap_test(Opts) ->
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "04-invalid-start-version"]),
                     fun(F) -> F() end,
                     query_fun([{0, "00_very_first_script.sql"}], ok)
                    ),
    ?assertError(
       {badfun, {error,unexpected_version, {expected, 1, supplied, 2}}},
       PreparedCall()).

negative_version_test(Opts) ->
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "05-negative-version"]),
                     fun(F) -> F() end,
                     query_fun([], ok)
                    ),
    ?assertError(
       {badfun, {error,unexpected_version, {expected, 0, supplied, -1}}},
       PreparedCall()).

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
