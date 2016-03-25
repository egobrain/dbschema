-module(dbschema_utils).

-export([
         check_monotonic/1,
         check_monotonic/2,

         success_foreach/2,
         success_map/2
        ]).

check_monotonic([]) -> ok;
check_monotonic([H|T]) ->
    check_monotonic(H, T).

check_monotonic(V, L) ->
    check_monotonic_(V, L, []).

check_monotonic_(_, [], []) -> ok;
check_monotonic_(_, [], Acc) ->
    {error, lists:reverse(Acc)};
check_monotonic_(V, [H|T]=L, Acc) ->
    if V =:= H -> check_monotonic_(V, T, [{dupplicated, H}|Acc]);
       V+1 =:= H -> check_monotonic_(V+1, T, Acc);
       V < H -> NewV = V+1, check_monotonic_(NewV, L, [{missing, NewV}|Acc]);
       V > H -> error(badarg)
    end.

-spec success_foreach(Fun, Args) -> ok | {error, Reason} when
      Fun :: fun((A) -> ok | {error, Reason}),
      Args :: [A].
success_foreach(_Fun, []) -> ok;
success_foreach(Fun, [Arg|Rest]) ->
    case Fun(Arg) of
        ok -> success_foreach(Fun, Rest);
        {error, _Reason} = Err -> Err
    end.

-spec success_map(fun((A) -> ok | {ok,B} | {error,R}), [A]) -> {ok,[B]} | {error,R}.
success_map(F, List) -> success_map_(F, List, []).
success_map_(_F, [], Acc) -> {ok, lists:reverse(Acc)};
success_map_(F, [H|T], Acc) ->
    case F(H) of
        {error, _Reason} = Err -> Err;
        {ok, V} -> success_map_(F, T, [V|Acc]);
        ok -> success_map_(F, T, Acc)
    end.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

check_monotonic_test() ->
    Tests =
        [
         {[], ok},
         {[1], ok},
         {[1,2,3], ok},
         {[5,6,7], ok},
         {[1,3], {error, [{missing, 2}]}},
         {[1,4], {error, [{missing, 2}, {missing, 3}]}},
         {[1,3,5], {error, [{missing, 2}, {missing, 4}]}},
         {[1,1], {error, [{dupplicated, 1}]}},
         {[1,2,2], {error, [{dupplicated, 2}]}},
         {[1,1,2,2], {error, [{dupplicated, 1}, {dupplicated, 2}]}},
         {[1,1,3,3], {error, [{dupplicated, 1}, {missing, 2}, {dupplicated, 3}]}}
        ],
    [
     ?assertEqual(check_monotonic(D), R) || {D, R} <- Tests
    ].

-endif.
