-module(epgsql_migrations_SUITE).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          migrate_one_script_test
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
    ?assertEqual(ok, PreparedCall()).

epgsql_query_fun(Conn) ->
    fun(Q) ->
            Res = epgsql:squery(Conn, Q),
            io:format("epgsql_query_fun ~p -> ~p~n",[Q, Res]),
            case Res of
                ok -> ok;
                {ok, [
                      {column, <<"version">>, _, _, _, _, _},
                      {column, <<"filename">>, _, _, _, _, _}], Data} -> Data;
                {ok, [{column, <<"max">>, _, _, _, _, _}], [{null}]} -> -1;
                {ok, [{column, <<"max">>, _, _, _, _, _}], [{N}]} -> N;
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
    {ok, _, _} = epgsql:squery(C, "DROP TABLE IF EXISTS database_migrations_history"),
    [{conn, C}|Opts].

end_per_testcase(_TestCase, Opts) ->
    ok = epgsql:close(?config(conn, Opts)).
