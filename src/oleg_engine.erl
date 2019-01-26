-module(oleg_engine).

-export([migrate/3]).

-type error() :: {error, Type :: atom(), Details :: any()}.

-spec migrate(
    Path :: nonempty_string(),
    FTx :: fun((F) -> ok | error()),
    FQuery :: fun((nonempty_string()) -> ok | list(string()) | error())) -> R
  when
  F :: fun(() -> ok | error()),
  R :: fun(() -> ok | error()).
migrate(Path, FTx, FQuery) ->
  ok = FQuery(dialect_postgres:init()),
  fun() -> FTx(fold(
    find_migrations(Path, FQuery),
    fun({V, F}) -> do_migration({V, F}, Path, FQuery) end))
  end.

do_migration({Version, FileName}, Path, FQuery) ->
  Version = FQuery(dialect_postgres:latest_existing_version()) + 1,
  ScriptPath = Path ++ "/" ++ FileName,
  compose(
    fun() -> file:read_file(ScriptPath) end,
    fun({ok, ScriptBody}) ->
      ok = FQuery(ScriptBody),
      ok = FQuery(dialect_postgres:save_migration(Version, FileName))
    end
  ).

find_migrations(ScriptsLocation, FQuery) ->
  MigrationsDone = sets:from_list(FQuery(dialect_postgres:migrations_names())),
  compose(
    fun() -> file:list_dir_all(ScriptsLocation) end,
    fun({ok, Files}) -> lists:filter(
        fun(N) -> not sets:is_element(N, MigrationsDone) end,
        lists:keysort(1, [string:to_integer(string:split(F, "_")) || F <- Files]))
    end
  ).

compose(F1, F2) -> fun() -> F2(F1()) end.

fold(Generate, Fold) -> fun() -> [ok = Fold(R) || R <- Generate()] end.