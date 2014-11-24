-module(alum_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    ApiConfig = alum_core_config:get_api_config(),
    Dispatch  = cowboy_router:compile([
        {'_', [
            {"/hosts/[:host]", alum_api_host_handler, []},
            {"/content/[:host]/[:path]", alum_api_content_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(alum_api, 10, ApiConfig, [
        {env, [{dispatch, Dispatch}]}
    ]),
    alum_api_sup:start_link().

stop(_State) ->
    ok.
