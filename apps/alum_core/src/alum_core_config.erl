-module(alum_core_config).

-export([
    get_content_path/0,
    get_web_config/0,
    parse_config/1
]).

%% Public API

%% @doc config for content file directory
get_content_path() ->
    Config    = app_helper:get_env(alum, content_dir),
    Directory = parse_config(Config),
    Directory.

%% @doc config for web content endpoint
get_web_config() ->
    Config    = app_helper:get_env(alum, http_web),
    WebConfig = parse_config(Config),
    WebConfig.

parse_config(Config) when erlang:is_list(Config) ->
    lists:map(fun parse_config/1, Config);

parse_config({ip, Value}) ->
    {ok, Address} = inet:parse_address(Value),
    {ip, Address};

parse_config(Config) ->
    Config.
