-module(alum_core_vnode).
-behaviour(riak_core_vnode).

-include("alum_core.hrl").

-export([
    init/1,
    delete/1,
    is_empty/1,
    terminate/2,
    handle_exit/3,
    start_vnode/1,
    handle_command/3,
    handle_coverage/4,
    handoff_starting/2,
    handoff_finished/2,
    handoff_cancelled/1,
    handle_handoff_data/2,
    encode_handoff_item/2,
    handle_handoff_command/3
]).

-ignore_xref([
    start_vnode/1
]).

-record(state, {
    partition :: binary()
}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition}}.

handle_command({store, File}, _Sender, State) ->
    Res = alum_core_data:store(File),
    {reply, Res, State};

handle_command({list, FilePath}, _Sender, State) ->
    Res = alum_core_data:list(FilePath),
    {reply, Res, State};

handle_command({fetch, FilePath}, _Sender, State) ->
    Res = alum_core_data:fetch(FilePath),
    {reply, Res, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(Req, _KeySpaces, Sender, State) ->
    handle_command(Req, Sender, State).

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
