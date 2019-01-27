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
  fun() -> FTx(flatten(
    map(
      find_migrations(Path, FQuery),
      partial(fun do_migration/3, Path, FQuery)),
    fun(M)-> ok = M() end))
  end.

do_migration(Path, FQuery, {Version, FileName}) ->
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

map(Generate, Fold) -> fun() -> [Fold(R) || R <- Generate()] end.

flatten(Generate, Flat) -> fun() -> [ok = Flat(R) || R <- Generate()] end.

partial(F, A, B) -> fun(C) -> F(A, B, C) end.