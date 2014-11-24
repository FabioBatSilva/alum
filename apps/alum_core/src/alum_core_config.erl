-module(alum_core_config).

-export([
    get_content_path/0,
    get_web_config/0,
    get_api_config/0,
    parse_config/1
]).

%% Public API

%% @doc config for content file directory
get_content_path() ->
    get_config(content_dir).

%% @doc config for web content endpoint
get_web_config() ->
    get_config(http_web).

%% @doc config for api content endpoint
get_api_config() ->
    get_config(http_api).

get_config(Key) ->
    RawConfig  = app_helper:get_env(alum, Key),
    AlumConfig = parse_config(RawConfig),
    AlumConfig.

parse_config(Config) when erlang:is_list(Config) ->
    lists:map(fun parse_config/1, Config);

parse_config({ip, Value}) ->
    {ok, Address} = inet:parse_address(Value),
    {ip, Address};

parse_config(Config) ->
    Config.
