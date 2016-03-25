-module(dbschema_SUITE).

-export([
         init_per_suite/1,
         all/0,
         end_per_suite/1,

         up_test/1,
         down_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("epgsql/include/epgsql.hrl").

init_per_suite(State) ->
    Start = os:timestamp(),
    State2 = pipe([
        fun start_sasl/1,
        fun start_lager/1,
        fun start_epgpool/1
    ], State),
    lager:info("epgpool started in ~p ms\n",
        [timer:now_diff(os:timestamp(), Start) / 1000]),
    State2.

start_sasl(State) ->
    application:load(sasl),
    application:set_env(kernel, error_logger, silent),
    application:set_env(sasl, sasl_error_logger, {file, "log/sasl.log"}),
    error_logger:tty(false),
    State.

start_lager(State) ->
    application:load(lager),
    application:set_env(lager, log_root, "log"),
    application:set_env(lager, handlers, [
        {lager_console_backend, info}
    ]),
    application:set_env(lager, error_logger_hwm, 256),
    State.

start_epgpool(State) ->
    application:load(epgpool),
    Config = epgpool_cth:start_postgres(),
    epgpool_cth:set_env(Config),
    {ok, _} = application:ensure_all_started(epgpool),
    [{pg_config, Config}|State].

end_per_suite(State) ->
    ok = epgpool_cth:stop_postgres(?config(pg_config, State)).

pipe(Funs, Config) ->
    lists:foldl(fun(F, S) -> F(S) end, Config, Funs).

all() ->
    [
     up_test,
     down_test,
     up_test
    ].

-define(MIGRATION_DIR, "../../../../test/migrations").

up_test(_Config) ->
    ok = dbschema:up(?MIGRATION_DIR),
    {ok, _, [
        {1, <<"test1">>, _},
        {2, <<"test2">>, _},
        {3, <<"erl">>, _}
    ]} = epgpool:equery("select id, name, ts from test order by id", []),
    {ok, 3} = dbschema_pg_driver:get_last_id().

down_test(_Config) ->
    ok = dbschema:down(?MIGRATION_DIR, 1),
    {ok, Columns, _} = epgpool:equery("select * from test order by id", []),
    false = lists:keymember(<<"ts">>, #column.name, Columns),
    {ok, 1} = dbschema_pg_driver:get_last_id(),
    {ok, _, [
        {1, <<"test1">>},
        {2, <<"test2">>}
    ]} = epgpool:equery("select id, name from test order by id", []).
