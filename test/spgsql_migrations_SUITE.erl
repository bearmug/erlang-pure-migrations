-module(spgsql_migrations_SUITE).

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
                     spgsql_tx_fun(Conn),
                     spgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {{select,1},[{0}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)).

migrate_few_scripts_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                     spgsql_tx_fun(Conn),
                     spgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {{select, 1}, [{1}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),
    ?assertMatch(
       {{select, 1}, [{1}]},
       pgsql_connection:simple_query("select count(*) from fruit where color = 'yellow'", Conn)).

incremental_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    TxFun = spgsql_tx_fun(Conn),
    QueryFun = spgsql_query_fun(Conn),
    MigrationStep1 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       TxFun, QueryFun),
    MigrationStep2 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                       TxFun, QueryFun),

    %% assert migrations table created and nothing done
    ?assertMatch(
       {{select, 1}, [{null}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),
    ?assertMatch(
       {error, {pgsql_error, [_, _, _, {message, <<"relation \"fruit\" does not exist">>}|_]}},
       pgsql_connection:simple_query("select count(*) from fruit", Conn)),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {{select, 1}, [{0}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),

    %% assert step 2 migration
    ok =MigrationStep2(),
    ?assertMatch(
       {{select, 1}, [{1}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),
    ?assertMatch(
       {{select, 1}, [{1}]},
       pgsql_connection:simple_query("select count(*) from fruit where color = 'yellow'", Conn)).

wrong_initial_version_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "02-wrong-initial-version"]),
                     spgsql_tx_fun(Conn),
                     spgsql_query_fun(Conn)
                    ),
    ?assertMatch(
       {rollback, {badmatch, {error, unexpected_version, {expected, 0, supplied, 20}}}},
       PreparedCall()).

migration_gap_test(Opts) ->
    Conn = ?config(conn, Opts),
    MigrationStep1 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       spgsql_tx_fun(Conn),
                       spgsql_query_fun(Conn)
                      ),
    MigrationStep2 = engine:migrate(
                       filename:join([?config(data_dir, Opts), "03-migration-gap"]),
                       spgsql_tx_fun(Conn),
                       spgsql_query_fun(Conn)
                      ),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {{select, 1}, [{0}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),

    %% assert step 2 failed migration
    ?assertMatch(
       {rollback, {badmatch, {error, unexpected_version, {expected, 1, supplied, 2}}}},
       MigrationStep2()),
    ?assertMatch(
       {{select, 1}, [{0}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),
    ?assertMatch(
       {error, {pgsql_error, [_, _, _, {message, <<"column \"color\" does not exist">>}|_]}},
       pgsql_connection:simple_query("select count(*) from fruit where color = 'yellow'", Conn)).

transactional_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "04-last-migration-fail"]),
                     spgsql_tx_fun(Conn),
                     spgsql_query_fun(Conn)
                    ),
    ?assertMatch(
       {rollback, {badmatch, {error, {pgsql_error, _}}}},
       PreparedCall()),
    ?assertMatch(
       {{select, 1}, [{null}]},
       pgsql_connection:simple_query("select max(version) from database_migrations_history", Conn)),
    ?assertMatch(
       {error, {pgsql_error, [_, _, _, {message, <<"relation \"fruit\" does not exist">>}|_]}},
       pgsql_connection:simple_query("select count(*) from fruit", Conn)).

spgsql_query_fun(Conn) ->
    fun(Q) ->
            case pgsql_connection:simple_query(Q, Conn) of
                {{select, 0}, []} -> [];
                {{select, 1}, Data = [{_V, _F}|_]}  ->
                    [{V, binary_to_list(BinF)} || {V, BinF} <- Data];
                {{select, 1}, [{null}]} -> -1;
                {{select, 1}, [{N}]} -> N;
                {{insert, 0, 1}, []} -> ok;
                {{create, table},[]} -> ok;
                {error, Details} -> {error, Details};
                _ -> ok
            end
    end.

spgsql_tx_fun(Conn) ->
    fun(F) ->
            pgsql_connection:simple_query("BEGIN", Conn),
            try F() of
                Res ->
                    pgsql_connection:simple_query("COMMIT", Conn),
                    Res
            catch
                _:Problem ->
                    pgsql_connection:simple_query("ROLLBACK", Conn),
                    {rollback, Problem}
            end
    end.

init_per_testcase(_TestCase, Opts) ->
    {ok, [{host, Host},
          {port, _Port},
          {database, Database},
          {username, Username},
          {secret, Secret},
          {timeout, _Timeout}]} = application:get_env(postgres, config),
    {ok, _Pid} = pgsql_connection_sup:start_link(),
    C = pgsql_connection:open(Host, Database, Username, Secret),
    {{drop, table}, _} = pgsql_connection:simple_query("DROP TABLE IF EXISTS database_migrations_history, fruit", C),
    [{conn, C}|Opts].

end_per_testcase(_TestCase, Opts) ->
    ok = pgsql_connection:close(?config(conn, Opts)).

