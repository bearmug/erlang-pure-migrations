-module(epgsql_migrations_SUITE).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          dummy_test
         ].

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

dummy_test(Opts) ->
    C = ?config(conn, Opts),
    io:format("tests connection: ~p~n", [C]),
    ok.
