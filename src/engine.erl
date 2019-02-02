-module(engine).

-export([migrate/3]).

-type error() :: {error, Type :: atom(), Details :: any()}.

-spec migrate(
        Path :: nonempty_string(),
        FTx :: fun((F) -> ok | error()),
        FQuery :: fun((nonempty_string()) -> ok | list(string()) | integer() | error())) -> R
                                                                                                when
      F :: fun(() -> ok | error()),
      R :: fun(() -> ok | error()).
migrate(Path, FTx, FQuery) ->
    ok = FQuery(dialect_postgres:init()),
    fun() ->
            FTx(flatten(map(
                          find_migrations(Path, FQuery),
                          fun(V_F) -> do_migration(Path, FQuery, V_F) end)))
    end.

do_migration(Path, FQuery, {V, F}) ->
    ExpectedVersion = FQuery(dialect_postgres:latest_existing_version()) + 1,
    case V of
        ExpectedVersion ->
            ScriptPath = filename:join(Path, F),
            compose(
              fun() -> file:read_file(ScriptPath) end,
              fun({ok, ScriptBody}) ->
                      ok = FQuery(ScriptBody),
                      ok = FQuery(dialect_postgres:save_migration(V, F))
              end);
        _ -> {
              error,
              unexpected_version,
              {expected, ExpectedVersion, supplied, V}}
    end;
do_migration(_Path, _FQuery, Error) -> Error.

find_migrations(ScriptsLocation, FQuery) ->
    compose(
      fun() -> file:list_dir_all(ScriptsLocation) end,
      fun(Files) -> filter_script_files(Files, ScriptsLocation, FQuery) end).

filter_script_files({ok, Files}, _Folder, FQuery) ->
    MigrationsDone = sets:from_list(FQuery(dialect_postgres:migrations_names())),
    lists:filter(
      fun(N) -> not sets:is_element(N, MigrationsDone) end,
      lists:keysort(1, [version_and_filename(F) || F <- Files]));
filter_script_files(Error, Folder, _FQuery)->
    [{error, invalid_folder, {folder_supplied, Folder, error, Error}}].

version_and_filename(F) ->
    case string:to_integer(F) of
        {Value, "_" ++ _} -> {Value, F};
        _ -> {error, invalid_filename, {expected, "<number>_<description>.sql", supplied, F}}
    end.

compose(F1, F2) -> fun() -> F2(F1()) end.
map(Generate, Map) -> fun() -> [Map(R) || R <- Generate()] end.
flatten(Generate) -> fun() -> [ok = R() || R <- Generate()], ok end.
