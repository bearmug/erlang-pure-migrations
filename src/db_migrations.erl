-module(db_migrations).

-export([migrate/3]).

-define(DIALECT, dialect_postgres).

-spec migrate(
    Path   :: nonempty_string(),
    FTx    :: fun((F :: fun()) -> any()),
    FQuery :: fun((Query :: string()) -> any()) -> any().
migrate(Path, FTx, FQuery) ->
  ok = is_ok(FQuery(?DIALECT:init())),
  ok = is_ok(FTx(
    fun() ->
      [ok = is_ok(do_migration(M, Path, FQuery)) || M <- find_migrations(Path, FQuery)]
    end)).

do_migration({Version, FileName}, Path, FQuery) ->
  Version = FQuery(?DIALECT:latest_existing_version()) + 1,
  ScriptPath = Path ++ "/" ++ FileName,
  case file:read_file(ScriptPath) of
    {ok, ScriptBody} ->
      ok = is_ok(FQuery(ScriptBody)),
      ok = is_ok(FQuery(?DIALECT:save_migration(Version, FileName)));
    E -> {error, file_read, ScriptPath, E}
  end.

find_migrations(ScriptsLocation, RunQuery) ->
  MigrationsDone = lists:as_set(RunQuery(?DIALECT:migrations_names())),
  case lists:filter(
    fun(N) -> not sets:contains(MigrationsDone, N) end,
    file:list_dir_all(ScriptsLocation)) of
      Files ->
        lists:keysort(1, [string:to_integer(string:split(F, "_")) || F <- Files]);
      _ -> []
  end.

is_ok(ok) -> ok.
is_ok({ok, _}) -> ok.
is_ok({ok, _, _}) -> ok.
is_ok({ok, _, _, _}) -> ok.
is_ok(E) -> {error, E}.