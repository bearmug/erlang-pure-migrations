-module(epgsql_migrations_SUITE).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [ migrate_one_script_test
         , migrate_few_scripts_test
         , incremental_migration_test
         , wrong_initial_version_test
         , migration_gap_test
         , transactional_migration_test
         ].

migrate_one_script_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                     fun(F) ->
                             epgsql:with_transaction(Conn, fun(_) -> F() end)
                     end,
                     epgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {ok, _,[{<<"0">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")).

migrate_few_scripts_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                     fun(F) ->
                             epgsql:with_transaction(Conn, fun(_) -> F() end)
                     end,
                     epgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {ok, _,[{<<"1">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, _,[{<<"1">>}]},
       epgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

incremental_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    TxFun =
        fun(F) ->
                epgsql:with_transaction(Conn, fun(_) -> F() end)
        end,
    MigrationStep1 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       TxFun, epgsql_query_fun(Conn)
                      ),
    MigrationStep2 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                       TxFun, epgsql_query_fun(Conn)
                      ),

    %% assert migrations table created and nothing done
    ?assertMatch(
       {ok, _,[{null}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {error,{error, _, _, undefined_table, <<"relation \"fruit\" does not exist">>, _}},
       epgsql:squery(Conn, "select count(*) from fruit")),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {ok, _, [{<<"0">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),

    %% assert step 2 migration
    ok =MigrationStep2(),
    ?assertMatch(
       {ok, _, [{<<"1">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, _, [{<<"1">>}]},
       epgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

wrong_initial_version_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "02-wrong-initial-version"]),
                     fun(F) ->
                             epgsql:with_transaction(Conn, fun(_) -> F() end)
                     end,
                     epgsql_query_fun(Conn)
                    ),
    ?assertEqual(
       {rollback, {badmatch, {error, unexpected_version, {expected, 0, supplied, 20}}}},
       PreparedCall()).

migration_gap_test(Opts) ->
    Conn = ?config(conn, Opts),
    TxFun =
        fun(F) ->
                epgsql:with_transaction(Conn, fun(_) -> F() end)
        end,
    MigrationStep1 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       TxFun, epgsql_query_fun(Conn)
                      ),
    MigrationStep2 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "03-migration-gap"]),
                       TxFun, epgsql_query_fun(Conn)
                      ),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {ok, _, [{<<"0">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),

    %% assert step 2 failed migration
    ?assertEqual(
       {rollback, {badmatch, {error, unexpected_version, {expected, 1, supplied, 2}}}},
       MigrationStep2()),
    ?assertMatch(
       {ok, _, [{<<"0">>}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {error, {error, error, _, undefined_column, <<"column \"color\" does not exist">>, _}},
       epgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

transactional_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "04-last-migration-fail"]),
                     fun(F) ->
                             epgsql:with_transaction(Conn, fun(_) -> F() end)
                     end,
                     epgsql_query_fun(Conn)
                    ),
    ?assertMatch(
       {rollback, {badmatch, {error, {error, error, _, syntax_error, _, _}}}},
       PreparedCall()),
    ?assertMatch(
       {ok, _, [{null}]},
       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {error, {error, _, _, undefined_table, <<"relation \"fruit\" does not exist">>, _}},
       epgsql:squery(Conn, "select count(*) from fruit")).

epgsql_query_fun(Conn) ->
    fun(Q) ->
            case epgsql:squery(Conn, Q) of
                ok -> ok;
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
    end.

init_per_testcase(_TestCase, Opts) ->
    {ok, [{host, Host},
          {port, _Port},
          {database, Database},
          {username, Username},
          {secret, Secret},
          {timeout, Timeout}]} = application:get_env(db, config),
    {ok, C} = epgsql:connect(Host, Username, Secret,
                             #{
                               database => Database,
                               timeout => Timeout
                              }),
    {ok, _, _} = epgsql:squery(C, "DROP TABLE IF EXISTS database_migrations_history, fruit"),
    [{conn, C}|Opts].

end_per_testcase(_TestCase, Opts) ->
    ok = epgsql:close(?config(conn, Opts)).
