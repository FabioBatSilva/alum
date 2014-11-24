-module(alum_api_host_handler).

-export([
    init/3,
    handle_json/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_json}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

handle_json(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Host, _}   = cowboy_req:binding(host, Req),

    handle_json(Method, Host, Req, State).

handle_json(<<"GET">>, Host, Req, State) ->
    Config = get_host_config(Host),
    Body   = jiffy:encode({Config}),

    {Body, Req, State};

handle_json(<<"PUT">>, Host, Req, State) ->
    Config = get_host_config(Host),
    Body   = jiffy:encode({Config}),

    {Body, Req, State}.

get_host_config(Host) ->
    [
        {host, Host},
        {cache, 3600}
    ].