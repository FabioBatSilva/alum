-module(alum_handler).

-export([
    init/3,
    handle/2,
    terminate/3,
    allowed_methods/2
]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    FilePath    = file_path(Req),
    {ok, Resp}  = execute(Method, FilePath, Req),
    {ok, Resp, State}.

terminate(_Reason, _Req, _State) ->
    ok.

execute(<<"GET">>, FilePath, Req) ->
    % get file content
    FileGet  = alum_core:fetch(FilePath),
    Response = create_get_reply(FileGet, Req),

    Response;

execute(<<"PUT">>, {Host, Path}, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    % put file content
    File = {Host, Path, Body},
    true = alum_core:store(File),
    % generete response
    Headers  = [{<<"content-type">>, <<"application/json">>}],
    Response = cowboy_req:reply(200, Headers, <<"true">>, Req),

    Response;

execute(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

create_get_reply({ok, File}, Req) ->
    {Body, Type} = File,
    % generete response
    Headers  = [{<<"content-type">>, Type}],
    Response = cowboy_req:reply(200, Headers, Body, Req),

    Response;

create_get_reply(_, Req) ->
    cowboy_req:reply(404, Req).

file_path(Req) ->
    {Host, _} = cowboy_req:host(Req),
    {Path, _} = cowboy_req:path(Req),
    {Host, Path}.