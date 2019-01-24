-module(dialect_postgres).

-export([init/0, migrations_names/0, save_migration/2]).


init() ->
  %% check compatibility with the engine
  %% create migrationstable if not exists
  erlang:error(not_implemented).

migrations_names() ->
  erlang:error(not_implemented).