-module(alum_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    WebConfig = alum_core_config:get_web_config(),
    Dispatch  = cowboy_router:compile([
        {'_', [
            {"/[...]", alum_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(alum_web, 500, WebConfig, [
        {env, [{dispatch, Dispatch}]}
    ]),
    alum_sup:start_link().

stop(_State) ->
    ok.
