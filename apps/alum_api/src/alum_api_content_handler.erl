-module(alum_api_content_handler).

-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    handle_get_content/2,
    handle_put_content/2,
    content_types_accepted/2,
    content_types_provided/2
]).

-record(context, {
    file,
    method,
    filepath
}).

init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, RespMethod} = cowboy_req:method(Req),
    {Host, RespHost}     = cowboy_req:binding(host, RespMethod),
    {Path, RespPath}     = cowboy_req:binding(path, RespHost),
    FilePath             = {Host, filename:join(["/", Path])},

    {ok, RespPath, #context{
        filepath=FilePath,
        method=Method
    }}.

resource_exists(Req, #context{filepath=FilePath, method=Method}=State) when Method =:= <<"GET">> ->
    case alum_core:fetch(FilePath) of
        {ok, File} ->
            {true, Req, State#context{file=File}};
        _ ->
            response(false, [{status, not_found}], Req, State)
    end;

resource_exists(Req, State) ->
    {true, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"html">>, '*'}, handle_get_content}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        { '*' , handle_put_content}
    ], Req, State}.

handle_get_content(Req, #context{file={Body, Type}}=State) ->
    Response = cowboy_req:set_resp_header(<<"content-type">>, Type, Req),
    Reply    = {Body, Response, State},

    Reply.

handle_put_content(Req, #context{filepath={Host, Path}}=State) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Result        = alum_core:store({Host, Path, Body}),

    case Result of
        true ->
            response(true, [{status, ok}], Req, State);
        Error ->
            lager:error("Error : ~p~n", [Error]),
            response(halt, [{status, error}], Req, State)
    end.

response(Status, Body, Req, State) ->
    Json  = encode_response(Body),
    Resp  = cowboy_req:set_resp_body(Json, Req),

    {Status, Resp, State}.

encode_response(Body) ->
    case jiffy:encode({Body}) of
        {error, Details} ->
            lager:error("Error : ~p~n", [Details]),
            {error, Details};
        Binary ->
            Binary
    end.
