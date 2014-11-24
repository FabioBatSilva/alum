-module(alum_api_content_handler).

-export([
    init/3,
    handle_content/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2
]).

init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"html">>, '*'}, handle_content}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_content}
    ], Req, State}.

handle_content(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Host, _}   = cowboy_req:binding(host, Req),
    {Path, _}   = cowboy_req:binding(path, Req),

    handle_content(Method, Host, Path, Req, State).

handle_content(<<"GET">>, Host, Path, Req, State) ->
    Response = jiffy:encode({[
        {status, ok},
        {host, Host},
        {path, Path}
    ]}),

    {Response, Req, State};

handle_content(<<"PUT">>, Host, Path, Req, State) ->
    % {ok, Content, _} = cowboy_req:body(Req),
    % Response = jiffy:encode({[
    %    {host, Host},
    %    {path, Path},
    %    {status, ok}
    %]}),

    {true, Req, State}.