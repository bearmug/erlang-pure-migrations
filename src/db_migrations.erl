-module(db_migrations).

-export([migrate/1]).

-spec migrate(string()) -> ok.
migrate(ScriptsLocation) ->
  case find_migrations(ScriptsLocation) of
    [] -> ok;
    Migrations -> db_tx(fun() ->
                          ok = db_init(),
                          [do_migration(M) || M <- Migrations]
                        end)a
  end.

find_migrations() ->
  erlang:error(not_implemented).

db_tx(Fun) ->
  erlang:error(not_implemented).

db_init() ->
  erlang:error(not_implemented).

do_migration(M) ->
  erlang:error(not_implemented).

find_migrations(ScriptsLocation) ->
  erlang:error(not_implemented).