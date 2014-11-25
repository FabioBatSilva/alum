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
    case get_host_config(Host) of
        {error, _} ->
            {<<"error">>, Req, State};
        not_found ->
	    {<<"not_found">>, Req, State};
 	Props ->
            {jiffy:encode({Props}), Req, State}
    end;

handle_json(<<"PUT">>, Host, Req, State) ->
    {ok, Body, _} = cowboy_req:body(Req),
    JsonResult    = jiffy:decode(Body),
    Config        = case JsonResult of
        {error, _} -> [];
        {List}     -> List
    end,

    case set_host_config(Host, Config) of
        ok    -> {true, Req, State};
        Error ->
            lager:warning("Error: ~p", [Error]), 
            {false, Req, State}
    end.

get_host_config(Host) ->
    alum_core_host:get_host(Host).

set_host_config(Host, Config) ->
    alum_core_host:set_host(Host, Config).
