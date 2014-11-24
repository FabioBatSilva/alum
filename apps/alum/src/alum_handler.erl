-module(alum_handler).

-export([
    init/3,
    handle/2,
    terminate/3,
    allowed_methods/2
]).

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Resp}  = execute(Method, Req),
    {ok, Resp, State}.

terminate(_Reason, _Req, _State) ->
    ok.

execute(<<"GET">>, Req) ->
    % get file content
    {Host, _} = cowboy_req:host(Req),
    {Path, _} = cowboy_req:path(Req),
    FileGet   = alum_core:fetch({Host, Path}),
    Response  = create_get_reply(FileGet, Req),

    Response;

execute(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

create_get_reply({ok, {Body, Type}}, Req) ->
    % generete response
    Headers  = [{<<"content-type">>, Type}],
    Response = cowboy_req:reply(200, Headers, Body, Req),

    Response;

create_get_reply(_, Req) ->
    cowboy_req:reply(404, Req).
