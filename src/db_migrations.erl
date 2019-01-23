-module(db_migrations).

-export([migrate/1]).

-type version()   :: 0..1000.
-type filename()  :: nonempty_string().
-type folder()    :: nonempty_string().
-type migration() :: {version(), filename()}.

-spec migrate(string()) -> ok.
migrate(ScriptsLocation) ->
  case find_migrations(ScriptsLocation) of
    [] -> ok;
    Migrations -> db_tx(fun() ->
                          ok = db_init(),
                          [do_migration(M) || M <- Migrations]
                        end)
  end.

db_tx(Fun) ->
  query("open db-level locking transaction"),
  try Fun() of
    _ ->
      query("commit transaction")
  catch
    E ->
      query("rollback transaction"),
      throw(E)
  end.

db_init() ->
  query("create migrations table if not exists").

do_migration({Version, FileName}) ->
  Version = next_migration_version(),
  query("run migration body"),
  query("insert migration version with Version and FileName").

-spec find_migrations(folder()) -> [migration()].
find_migrations(ScriptsLocation) ->
  AppliedMigrations = lists:as_set(db_applied_migrations()),
  case lists:filter(
    fun(N) -> not sets:contains(AppliedMigrations, N) end,
    file:list_dir_all(ScriptsLocation)) of
      [] -> [];
      Files ->
        lists:keysort(1, [string:to_integer(string:split(F, "_")) || F <- Files])
  end.

query(Str) ->
  erlang:error(not_implemented).

next_migration_version() ->
  query("find latest migration version") + 1.