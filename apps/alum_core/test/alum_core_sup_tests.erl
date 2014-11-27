-module(alum_core_sup_tests).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init_all_sup_test() ->
    ?assertMatch(
        {ok, {{one_for_one, 5, 10}, L }} when length(L) =:= 3,
        alum_core_sup:init([])
    ).

init_fsm_sup_test() ->
    ?assertMatch(
        {ok, {{simple_one_for_one, 0, 1}, L }} when length(L) =:= 1,
        alum_core_sup:init([alum_core_fsm_sup])
    ).