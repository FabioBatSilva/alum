-module(alum_core_fsm).
-behaviour(gen_fsm).

-include("alum_core.hrl").

%% API
-export([
    start_link/1,
    do/4
]).

%% gen_fsm callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    handle_info/3,
    handle_event/3,
    handle_sync_event/4
]).

%% states
-export([
    prepare/3,
    execute/2,
    waiting/2
]).

-record(state, {
    vnode_master,
    cmd,
    from,
    preflist,
    num,
    replies
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(VnodeMaster) ->
    gen_fsm:start_link(?MODULE, [VnodeMaster], []).

do(VnodeMaster, Cmd, N, WR) ->
    {ok, Pid} = alum_core_sup:start_coordinator(VnodeMaster),
    gen_fsm:sync_send_event(Pid, {Cmd, N, WR}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([VnodeMaster]) ->
    {ok, prepare, #state{vnode_master=VnodeMaster, replies=[]}}.

prepare({Cmd, N, WR}, From, State) ->
    DocIdx   = chash:key_of(element(1, Cmd)),
    Preflist = riak_core_apl:get_apl(DocIdx, N, alum_core),

    {next_state, execute, State#state{
        preflist=Preflist,
        from=From,
        cmd=Cmd,
        num=WR
    }, 0}.

execute(timeout, State) when State#state.num == 0 -> % async
    riak_core_vnode_master:command(
        State#state.preflist,
        State#state.cmd,
        State#state.vnode_master
    ),
    gen_server:reply(State#state.from, ok),
    {stop, normal, State};

execute(timeout, State) -> % sync
    riak_core_vnode_master:command(
        State#state.preflist,
        State#state.cmd,
        {fsm, undefined, self()},
        State#state.vnode_master
    ),
    {next_state, waiting, State}.

waiting(Res, State) ->
    Replies = [Res| State#state.replies],
    if length(Replies) /= State#state.num
            -> {next_state, waiting, State#state{replies=Replies}};
       true ->
            gen_fsm:reply(State#state.from, Replies),
            {stop, normal, State#state{replies=Replies}}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
