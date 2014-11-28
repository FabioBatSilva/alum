-module(alum_core_host_tests).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(METADATA_PREFIX, {alum_core, hosts}).

set_host_merge_test() ->
    Merge = [{cache,999}, {n_val,3}, {r,quorum}, {w,quorum}],
    ok    = meck:new(riak_core_metadata),
    Host  = <<"static.com">>,

    meck:expect(riak_core_metadata, put, fun(Arg1, Arg2, Arg3) ->
        ?assertEqual(?METADATA_PREFIX, Arg1),
        ?assertEqual(Host, Arg2),
        ?assertEqual(Merge, Arg3),
        ok
    end),

    Actual = alum_core_host:set_host(Host, [{"cache", 999}]),

    meck:validate(riak_core_metadata),
    meck:unload(riak_core_metadata),

    ?assertEqual(ok, Actual).

get_host_test() ->
    Props    = [{cache,3600}, {n_val,3}, {r,quorum}, {w,quorum}],
    ok       = meck:new(riak_core_metadata),
    Host     = <<"static.somehostname.com">>,
    Expected = {ok, [{name, Host} | Props]},

    meck:expect(riak_core_metadata, get, fun(Arg1, Arg2) ->
        ?assertEqual(?METADATA_PREFIX, Arg1),
        ?assertEqual(Host, Arg2),
        Props
    end),

    Actual = alum_core_host:get_host(Host),

    meck:validate(riak_core_metadata),
    meck:unload(riak_core_metadata),

    ?assertEqual(Expected, Actual).

get_host_not_found_test() ->
    ok       = meck:new(riak_core_metadata),
    Host     = <<"static.alum.com">>,
    Expected = {error, not_found},

    meck:expect(riak_core_metadata, get, fun(Arg1, Arg2) ->
        ?assertEqual(?METADATA_PREFIX, Arg1),
        ?assertEqual(Host, Arg2),
        undefined
    end),

    Actual = alum_core_host:get_host(Host),

    meck:validate(riak_core_metadata),
    meck:unload(riak_core_metadata),

    ?assertEqual(Expected, Actual).

get_host_error_test() ->
    ok       = meck:new(riak_core_metadata),
    Host     = <<"static.alum.com">>,
    Expected = {error, some_error},

    meck:expect(riak_core_metadata, get, fun(Arg1, Arg2) ->
        ?assertEqual(?METADATA_PREFIX, Arg1),
        ?assertEqual(Host, Arg2),
        {error, some_error}
    end),

    Actual = alum_core_host:get_host(Host),

    meck:validate(riak_core_metadata),
    meck:unload(riak_core_metadata),

    ?assertEqual(Expected, Actual).

get_host_props_test() ->
    HostName = <<"static.alum.com">>,
    Props    = [
        {name, HostName}, 
        {cache, 3600},
        {n_val, 3},  
        {w, 2}, 
        {r, 1}
    ],

    ?assertEqual(HostName, alum_core_host:name(Props)),
    ?assertEqual(3600, alum_core_host:cache(Props)),
    ?assertEqual(3, alum_core_host:n_val(Props)),
    ?assertEqual(2, alum_core_host:w_val(Props)),
    ?assertEqual(1, alum_core_host:r_val(Props)).
