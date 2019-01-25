-module(oleg_engine).

-export([migrate/3]).

-type error() :: {error, Type :: atom(), Details :: any()}.

-spec migrate(
    Path   :: nonempty_string(),
    FTx    :: fun((F) -> ok | error()),
    FQuery :: fun((nonempty_string()) -> ok | list(string()) | error())) -> ok | error()
      when F :: fun(() -> ok | error()).
migrate(Path, FTx, FQuery) ->
  ok = FQuery(dialect_postgres:init()),
  ok = FTx(
    fun() ->
      [ok = do_migration(M, Path, FQuery) || M <- find_migrations(Path, FQuery)]
    end).

do_migration({Version, FileName}, Path, FQuery) ->
  Version = FQuery(dialect_postgres:latest_existing_version()) + 1,
  ScriptPath = Path ++ "/" ++ FileName,
  case file:read_file(ScriptPath) of
    {ok, ScriptBody} ->
      ok = FQuery(ScriptBody),
      ok = FQuery(dialect_postgres:save_migration(Version, FileName));
    E -> {error, file_read, E}
  end.

find_migrations(ScriptsLocation, FQuery) ->
  MigrationsDone = sets:from_list(FQuery(dialect_postgres:migrations_names())),
  lists:filter(
    fun(N) -> not sets:is_element(N, MigrationsDone) end,
    case file:list_dir_all(ScriptsLocation) of
      {ok, Files} ->
        lists:keysort(1, [string:to_integer(string:split(F, "_")) || F <- Files]);
      _ -> []
    end).