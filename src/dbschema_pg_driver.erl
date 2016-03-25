-module(dbschema_pg_driver).

-compile({parse_transform, do}).

-export([
         init/0,
         get_last_id/0,
         up/2,
         list/1,
         down/2
        ]).

-include("migrations.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% =============================================================================
%% API
%% =============================================================================

init() ->
    epgpool:transaction(
        fun(C) ->
            do([error_m ||
                   IsSetup <- is_setup(C),
                   case IsSetup of
                       true -> ok;
                       false ->
                           Sql =
                               "CREATE TABLE migrations("
                               "  \"id\" INTEGER PRIMARY KEY,"
                               "  \"filename\" TEXT, "
                               "  \"type\" TEXT, "
                               "  \"timestamp\" TIMESTAMP);",
                           squery(C, Sql),
                           return(ok)
                   end
               ])
        end).

get_last_id() ->
    epgpool:with(
        fun(C) ->
            do([error_m ||
                   Sql = "select max(id) from migrations;",
                   [{Id}] <- equery(C, Sql, []),
                   case Id of
                       null -> return(0);
                       _ -> return(Id)
                   end
               ])
        end).

up(#migration{id=Id, filename=Filename, type=Type}, Sql) ->
    epgpool:transaction(
        fun(C) ->
            do([error_m||
                   squery(C, Sql),
                   InsertMigrationSql =
                       "insert into "
                       "migrations(\"id\", \"filename\", \"type\", \"timestamp\") "
                       "values ($1, $2, $3, $4);",
                   equery(C, InsertMigrationSql, [Id, Filename, Type, os:timestamp()]),
                   return(ok)
               ])
        end).

list(MigrationsId) ->
    Sql =
        "select \"id\", \"filename\", \"type\" from migrations "
        "where \"id\" > $1;",
    epgpool:with(
        fun(C) ->
            do([error_m ||
                   Rows <- equery(C, Sql, [MigrationsId]),
                   Migrations = lists:map(fun tuple_to_migration/1, Rows),
                   SortedMigrations = lists:keysort(#migration.id, Migrations),
                   return(SortedMigrations)
               ])
        end).

down(#migration{id=Id}, Sql) ->
    epgpool:transaction(
        fun(C) ->
            do([error_m ||
                   squery(C, Sql),
                   DeleteMigrationSql =
                       "delete from migrations where \"id\" = $1;",
                   equery(C, DeleteMigrationSql, [Id]),
                   return(ok)
               ])
        end).

is_setup(C) ->
    Sql =
        "SELECT * FROM pg_tables "
        "WHERE tablename='migrations' and schemaname = current_schema()",
    case squery(C, Sql) of
        {ok, [_Row]} -> {ok, true};
        {ok, []} -> {ok, false};
        {error, _Reason} = Err -> Err
    end.

%% =============================================================================
%%% Internal functions
%% =============================================================================

squery(C, Sql) ->
    result(epgpool:squery(C, Sql)).

equery(C, Sql, Params) ->
    result(epgpool:equery(C, Sql, Params)).

result(L) when is_list(L) ->
    dbschema_utils:success_map(fun result/1, L);
result(R) ->
    case R of
        {ok, _Count} -> ok;
        {ok, _Columns, Rows} -> {ok, Rows};
        {ok, _Columns, _Count, Rows} -> {ok, Rows};
        {error, #error{
             severity = Severity,
             code = Code,
             message = Message,
             extra = Extra
          }} ->
            Reason =
                #{severity => Severity,
                  code => Code,
                  message => Message,
                  extra => Extra},
            {error, Reason};
        {error, _Reason} = Err -> Err
    end.

tuple_to_migration({Id, Filename, Type}) ->
    #migration{id=Id, filename=Filename, type=Type}.
