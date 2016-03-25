-module('3-update').
-behaviour(dbschema_migration).

-export([
         up/1,
         down/1
        ]).

up(C) ->
    {ok, _} = epgpool:squery(C, "insert into test(id, name) values (3, 'erl');"),
    ok.

down(C) ->
    {ok, _} = epgpool:squery(C, "delete from test where id = 3;"),
    ok.
