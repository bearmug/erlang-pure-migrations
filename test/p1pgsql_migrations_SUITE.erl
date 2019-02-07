-module(p1pgsql_migrations_SUITE).

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
    PreparedCall = pure_migrations:migrate(
                     filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                     p1pgsql_tx_fun(Conn),
                     p1pgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["0"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")).

migrate_few_scripts_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = pure_migrations:migrate(
                     filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                     p1pgsql_tx_fun(Conn),
                     p1pgsql_query_fun(Conn)
                    ),
    ?assertEqual(ok, PreparedCall()),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["1"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"count", text, _, _, _, _, _}], [["1"]]}]},
       pgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

incremental_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    MigrationStep1 = pure_migrations:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       p1pgsql_tx_fun(Conn), p1pgsql_query_fun(Conn)
                      ),
    MigrationStep2 = pure_migrations:migrate(
                       filename:join([?config(data_dir, Opts), "01-two-scripts-test"]),
                       p1pgsql_tx_fun(Conn), p1pgsql_query_fun(Conn)
                      ),

    %% assert migrations table created and nothing done
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [[null]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, [{error, [{severity, 'ERROR'}, _, _, {message, "relation \"fruit\" does not exist"}|_]}]},
       pgsql:squery(Conn, "select count(*) from fruit")),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["0"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),

    %% assert step 2 migration
    ok =MigrationStep2(),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["1"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"count", text, _, _, _, _, _}], [["1"]]}]},
       pgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

wrong_initial_version_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = pure_migrations:migrate(
                     filename:join([?config(data_dir, Opts), "02-wrong-initial-version"]),
                     p1pgsql_tx_fun(Conn),
                     p1pgsql_query_fun(Conn)
                    ),
    ?assertEqual(
       {rollback, {badmatch, {error, unexpected_version, {expected, 0, supplied, 20}}}},
       PreparedCall()).

migration_gap_test(Opts) ->
    Conn = ?config(conn, Opts),
    MigrationStep1 = pure_migrations:migrate(
                       filename:join([?config(data_dir, Opts), "00-single-script-test"]),
                       p1pgsql_tx_fun(Conn), p1pgsql_query_fun(Conn)
                      ),
    MigrationStep2 = pure_migrations:migrate(
                       filename:join([?config(data_dir, Opts), "03-migration-gap"]),
                       p1pgsql_tx_fun(Conn), p1pgsql_query_fun(Conn)
                      ),

    %% assert step 1 migration
    ok = MigrationStep1(),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["0"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),

    %% assert step 2 failed migration
    ?assertEqual(
       {rollback, {badmatch, {error, unexpected_version, {expected, 1, supplied, 2}}}},
       MigrationStep2()),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [["0"]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, [{error, [{severity, 'ERROR'}, _, _, {message,"column \"color\" does not exist"}|_]}]},
       pgsql:squery(Conn, "select count(*) from fruit where color = 'yellow'")).

transactional_migration_test(Opts) ->
    Conn = ?config(conn, Opts),
    PreparedCall = pure_migrations:migrate(
                     filename:join([?config(data_dir, Opts), "04-last-migration-fail"]),
                     p1pgsql_tx_fun(Conn),
                     p1pgsql_query_fun(Conn)
                    ),
    ?assertMatch(
       {rollback, {badmatch, {error, [_, _, _, {message, "syntax error at or near \"GARBAGE\""}|_]}}},
       PreparedCall()),
    ?assertMatch(
       {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [[null]]}]},
       pgsql:squery(Conn, "select max(version) from database_migrations_history")),
    ?assertMatch(
       {ok, [{error, [_, _, _, {message, "relation \"fruit\" does not exist"}|_]}]},
       pgsql:squery(Conn, "select count(*) from fruit")).

p1pgsql_query_fun(Conn) ->
    fun(Q) ->
            case pgsql:squery(Conn, Q) of
                {ok, [{error, Details}]} -> {error, Details};
                {ok, [{_, [
                           {"version", text, _, _, _, _, _},
                           {"filename", text, _, _, _, _, _}], Data}]} ->
                    [{list_to_integer(V), F} || [V, F] <- Data];
                {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [[null]]}]} -> -1;
                {ok, [{"SELECT 1", [{"max", text, _, _, _, _, _}], [[N]]}]} ->
                    list_to_integer(N);
                {ok, _} -> ok
            end
    end.

p1pgsql_tx_fun(Conn) ->
    fun(F) ->
            pgsql:squery(Conn, "BEGIN"),
            try F() of
                Res ->
                    pgsql:squery(Conn, "COMMIT"),
                    Res
            catch
                _:Problem ->
                    pgsql:squery(Conn, "ROLLBACK"),
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
    {ok, C} = pgsql:connect(Host, Database, Username, Secret),
    {ok,["DROP TABLE"]} = pgsql:squery(C, "DROP TABLE IF EXISTS database_migrations_history, fruit"),
    [{conn, C}|Opts].

end_per_testcase(_TestCase, Opts) ->
    ok = pgsql:terminate(?config(conn, Opts)).
