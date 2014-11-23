-module(alum_core_config_tests).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse_config_raw_test() ->
    Config   = <<"some raw text">>,
    Expected = <<"some raw text">>,
    Actual   = alum_core_config:parse_config(Config),

    ?assertEqual(Expected, Actual).

parse_config_raw_list_test() ->
    Config   = [<<"some">>, <<"raw">>, <<"text">>],
    Expected = [<<"some">>, <<"raw">>, <<"text">>],
    Actual   = alum_core_config:parse_config(Config),

    ?assertEqual(Expected, Actual).

parse_config_ip_inet_test() ->
    Config   = {ip, "127.0.0.1"},
    Expected = {ip, {127, 0, 0, 1}},
    Actual   = alum_core_config:parse_config(Config),

    ?assertEqual(Expected, Actual).

parse_config_mix_list_test() ->
    Config   = [ {ip, "127.0.0.1"}, {port, 8080} ],
    Expected = [ {ip, {127, 0, 0, 1}}, {port, 8080} ],
    Actual   = alum_core_config:parse_config(Config),

    ?assertEqual(Expected, Actual).

get_content_path_test() ->
    Expected = <<"/tmp/path/to/content">>,
    ok       = application:set_env(alum, content_dir, Expected),
    Actual   = alum_core_config:get_content_path(),

    ?assertEqual(Expected, Actual).

get_web_config_test() ->
    Config = [
        {ip,  "127.0.0.1" },
        {port, 8080 }
    ],
    Expected = [
        {ip,  {127,0,0,1} },
        {port, 8080 }
    ],

    ok     = application:set_env(alum, http_web, Config),
    Actual = alum_core_config:get_web_config(),

    ?assertEqual(Expected, Actual).