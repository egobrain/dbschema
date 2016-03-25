-module(dbschema).
-compile({parse_transform, do}).

-export([
         up/1,
         list/0,
         down/2
        ]).

-include("migrations.hrl").

-type migration() :: #migration{}.

up(MigrationsFolder) ->
    do([error_m ||
           dbschema_pg_driver:init(),
           FileMigrations = get_migrations(MigrationsFolder),
           Ids = [Id || #migration{id=Id} <- FileMigrations],
           dbschema_utils:check_monotonic(Ids),
           LastId <- dbschema_pg_driver:get_last_id(),
           ActualMigrations =
               [
                M || #migration{id=Id}=M <- FileMigrations,
                     Id > LastId
               ],
           dbschema_utils:success_foreach(
               fun(Migration) ->
                   perform_(up, MigrationsFolder, Migration)
               end, ActualMigrations)
       ]).

list() ->
    dbschema_pg_driver:list(0).

down(MigrationsFolder, MigrationId) ->
    do([error_m ||
           ActualMigrations <- dbschema_pg_driver:list(MigrationId),
           dbschema_utils:success_foreach(
               fun(Migration) ->
                   perform_(down, MigrationsFolder, Migration)
               end, lists:reverse(ActualMigrations))
       ]).

%% =============================================================================
%%% Internal functions
%% =============================================================================

perform_(Type, MigrationsFolder, #migration{filename=Filename}=Migration) ->
    MigrationFile = filename:join([MigrationsFolder, Filename]),
    do([error_m ||
           FileContent <- read_file(MigrationFile),
           {Up, Down} = split_sql(FileContent),
           case Type of
               up ->
                   lager:info("Performing migration: [UP] \"~s\"", [Filename]),
                   dbschema_pg_driver:up(Migration, Up);
               down ->
                   lager:info("Performing migration: [DOWN] \"~s\"", [Filename]),
                   dbschema_pg_driver:down(Migration, Down)
           end
       ]).

-spec get_migrations(Folder :: file:name()) -> [migration()].
get_migrations(Folder) ->
    Migrations =
        filelib:fold_files(
            Folder,
            ".*", true,
            fun(F, Acc) ->
                F2 = filename:basename(F),
                case migration_file(F2) of
                    {ok, Migration} -> [Migration|Acc];
                    {error, _Reason} -> Acc
                end
            end,
            []),
    lists:keysort(#migration.id, Migrations).

-spec migration_file(file:name()) -> {ok, migration()} | {error, Reason :: _}.
migration_file(Filename) ->
    [$.|Ext] = filename:extension(Filename),
    case get_id(Filename) of
        {ok, Id} ->
            migration_file(
                Id,
                list_to_binary(Ext),
                iolist_to_binary(Filename));
        {error, _Reason} = Err -> Err
    end.

-spec migration_file(Id, Ext, Filename) -> {ok, migration()} | {error, Reason} when
      Id :: non_neg_integer(),
      Ext :: binary(),
      Filename :: binary(),
      Reason :: unknown_extension.
migration_file(Id, Type, Filename) when Type =:= <<"sql">>  ->
    {ok, #migration{id = Id, filename = Filename, type = Type}};
migration_file(_, _, _) ->
    {error, unknown_extension}.

-spec get_id(file:name()) -> {ok, Id :: non_neg_integer()} | {error, no_id}.
get_id(Filename) ->
    case re:run(Filename, "(\\d+)-.*", [{capture, [1], list}]) of
        {match, [Id]} -> {ok, list_to_integer(Id)};
        _ -> {error, no_id}
    end.

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, _Data} = Ok -> Ok;
        {error, Reason} ->
            {error, {file_error, Filename, Reason}}
    end.

split_sql(FileContent) ->
    Rows = binary:split(FileContent, <<"\n">>, [global]),
    {ok, UpP} = re:compile(<<"^--\\s*up\\s*">>, [caseless]),
    {ok, DownP} = re:compile(<<"^--\\s*down\\s*">>, [caseless]),
    {_, UpR, DownR} = lists:foldl(fun(Row, {St, UpRows, DownRows}) ->
        case re:run(Row, UpP) of
            {match, _} -> {up, UpRows, DownRows};
            nomatch ->
                case re:run(Row, DownP) of
                    {match, _} -> {down, UpRows, DownRows};
                    nomatch ->
                        case St of
                            up -> {St, [Row,$\n|UpRows], DownRows};
                            down -> {St, UpRows, [Row,$\n|DownRows]}
                        end
                end
        end
    end, {up, [], []}, Rows),
    {lists:reverse(UpR), lists:reverse(DownR)}.
