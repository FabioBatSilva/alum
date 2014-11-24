-module(alum_core).
-include("alum_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
    store/1,
    fetch/1,
    list/1
]).

-define(DEFAULT_TIMEOUT, 3000).

%% Public API

%% @doc Store a file
%% alum_core:store({ <<"static.alum.com">>, <<"/file.json">> , <<"[1,2,3]">> }).
store({Host, Path, Content}) ->
    Response = alum_core_fsm:do(?PROCESS_VMASTER, {store, {Host, Path, Content}}, ?N, ?W),
    Status   = lists:all(fun(X) when X =:= ok -> true; (_) -> false end, Response),

    Status.

%% @doc Fetch a file
%% alum_core:fetch({ <<"static.alum.com">>, <<"/file.json">> }).
fetch({Host, Path}) ->
    Response  = alum_core_fsm:do(?PROCESS_VMASTER, {fetch, {Host, Path} }, ?N, ?R),
    Element   = lists:last(Response),
    Element.


%% @doc Lists all the existing files
%% alum_core:list({ <<"static.alum.com">>, <<"/">> }).
list({Host, Path}) ->
    % Start coverate FSM to send out the command
    {ok, ReqId} = alum_core_coverage_fsm:start_op(list, {Host, Path}),

    % Wait default interval for the request to complete
    case wait_for_req(ReqId) of
        {error, timeout} ->
            lager:warning("Timed out waiting for the nodes to list !"),
            {error, timeout};
        {ok, Results} ->
            {ok, Results}
    end.

% Waits for a specified request ID to return. Either
% waits for the default timeout, or a user specified one.
wait_for_req(ReqId) ->
    wait_for_req(ReqId, ?DEFAULT_TIMEOUT).
wait_for_req(ReqId, Timeout) ->
    receive
        {ReqId, Resp} -> Resp
    after Timeout ->
        {error, timeout}
    end.
