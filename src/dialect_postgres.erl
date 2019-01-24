-module(dialect_postgres).

-export([init/0, migrations_names/0, save_migration/2, latest_existing_version/0]).


init() ->
  %% check compatibility with the engine
  %% create migrationstable if not exists
  erlang:error(not_implemented).

migrations_names() ->
  erlang:error(not_implemented).

save_migration(_Version, _Filename) ->
  erlang:error(not_implemented).

latest_existing_version() ->
  erlang:error(not_implemented).