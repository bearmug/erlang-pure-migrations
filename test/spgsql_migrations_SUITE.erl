-module(spgsql_migrations_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [ migrate_one_script_test
         , migrate_few_scripts_test
         , incremental_migration_test
           %%         , wrong_initial_version_test
           %%         , migration_gap_test
           %%         , transactional_migration_test
         ].

migrate_one_script_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = engine:migrate(
                     filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                     spgsql_tx_fun(),
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
                     spgsql_tx_fun(),
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
    TxFun = spgsql_tx_fun(),
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

%%wrong_initial_version_test(Opts) ->
%%    Conn = ?config(conn, Opts),
%%    PreparedCall = engine:migrate(
%%                     filename:join([?config(data_dir, Opts), "02-wrong-initial-version"]),
%%                     fun(F) ->
%%                             epgsql:with_transaction(Conn, fun(_) -> F() end)
%%                     end,
%%                     epgsql_query_fun(Conn)
%%                    ),
%%    ?assertEqual(
%%       {rollback, {badmatch, {error, unexpected_version, {expected, 0, supplied, 20}}}},
%%       PreparedCall()).
%%
%%migration_gap_test(Opts) ->
%%    Conn = ?config(conn, Opts),
%%    TxFun =
%%        fun(F) ->
%%                epgsql:with_transaction(Conn, fun(_) -> F() end)
%%        end,
%%    MigrationStep1 = engine:migrate(
%%                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
%%                       TxFun, epgsql_query_fun(Conn)
%%                      ),
%%    MigrationStep2 = engine:migrate(
%%                       filename:join([?config(data_dir, Opts), "03-migration-gap"]),
%%                       TxFun, epgsql_query_fun(Conn)
%%                      ),
%%
%%    %% assert step 1 migration
%%    ok = MigrationStep1(),
%%    ?assertMatch(
%%       {ok, _, [{<<"0">>}]},
%%       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
%%
%%    %% assert step 2 failed migration
%%    ?assertEqual(
%%       {rollback, {badmatch, {error, unexpected_version, {expected, 1, supplied, 2}}}},
%%       MigrationStep2()),
%%    ?assertMatch(
%%       {ok, _, [{<<"0">>}]},
%%       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
%%    ?assertMatch(
%%       {error, {error, error, _, undefined_column, <<"column \"color\" does not exist">>, _}},
%%       epgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).
%%
%%transactional_migration_test(Opts) ->
%%    Conn = ?config(conn, Opts),
%%    PreparedCall = engine:migrate(
%%                     filename:join([?config(data_dir, Opts), "04-last-migration-fail"]),
%%                     fun(F) ->
%%                             epgsql:with_transaction(Conn, fun(_) -> F() end)
%%                     end,
%%                     epgsql_query_fun(Conn)
%%                    ),
%%    ?assertMatch(
%%       {rollback, {badmatch, {error, {error, error, _, syntax_error, _, _}}}},
%%       PreparedCall()),
%%    ?assertMatch(
%%       {ok, _, [{null}]},
%%       epgsql:squery(Conn, "select max(version) from database_migrations_history")),
%%    ?assertMatch(
%%       {error, {error, _, _, undefined_table, <<"relation \"fruit\" does not exist">>, _}},
%%       epgsql:squery(Conn, "select count(*) from fruit")).

spgsql_query_fun(Conn) ->
    fun(Q) ->
            io:format("going to query=~p~n", [Q]),
            Res = pgsql_connection:simple_query(Q, Conn),
            io:format("query=~p, res=~p~n", [Q, Res]),
            case Res of
                {{select, 0}, []} -> [];
                {{select, 1}, Data = [{_V, _F}|_]}  ->
                    [{V, binary_to_list(BinF)} || {V, BinF} <- Data];
                {{select, 1}, [{null}]} -> -1;
                {{select, 1}, [{N}]} -> N;
                {{insert, 0, 1}, []} -> ok;
                {{create, table},[]} -> ok;
                _ -> ok
            end
    end.

spgsql_tx_fun() ->
    fun(F) -> F() end.

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

