-module(alum_api_host_handler).

-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    handle_get_config/2,
    handle_put_config/2,
    content_types_accepted/2,
    content_types_provided/2
]).

-record(context, {
    host,
    method,
    config
}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, RespMethod} = cowboy_req:method(Req),
    {Host, RespHost}     = cowboy_req:binding(host, RespMethod),

    {ok, RespHost, #context{host=Host, method=Method}}.

resource_exists(Req, #context{host=Host, method=Method}=State) when Method =:= <<"GET">> ->
    case get_host_config(Host) of
        {ok, Config} ->
            {true, Req, State#context{config=Config}};
        _ ->
            response(false, [{status, not_found}], Req, State)
    end;

resource_exists(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get_config}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        { '*' , handle_put_config}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

handle_get_config(Req,#context{config=Config}=State) ->
    response(Config, Req, State).

handle_put_config(Req, #context{host=Host}=State) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Config        = decode_request(Body),

    case set_host_config(Host, Config) of
        ok ->
            response(true, [{status, ok}], Req, State);
        {error, Details} ->
            lager:error("Error : ~p~n", [Details]),
            response(halt, [{status, error}], Req, State)
    end.

response(Status, Body, Req, State) ->
    Json  = encode_response(Body),
    Resp  = cowboy_req:set_resp_body(Json, Req),

    {Status, Resp, State}.

response(Body, Req, State) ->
    {encode_response(Body), Req, State}.

decode_request(Body) ->
    case jiffy:decode(Body) of
        {error, _} -> [];
        {List}     -> List
    end.

encode_response(Body) ->
    case jiffy:encode({Body}) of
        {error, Details} ->
            lager:error("Error : ~p~n", [Details]),
            {error, Details};
        Binary ->
            Binary
    end.

get_host_config(Host) ->
    alum_core_host:get_host(Host).

set_host_config(Host, Config) ->
    alum_core_host:set_host(Host, Config).
