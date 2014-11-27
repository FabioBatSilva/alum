-module(alum_core_sup).
-behaviour(supervisor).

-include("alum_core.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    start_coordinator/1
]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Sup) ->
    supervisor:start_link({local, Sup}, ?MODULE, [Sup]).

start_coordinator(VnodeMaster) ->
    supervisor:start_child(alum_core_fsm_sup, [VnodeMaster]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Coverage = {alum_core_coverage_fsm_sup,
        {alum_core_coverage_fsm_sup, start_link, []},
        permanent, 60000, supervisor, dynamic
    },
    Coord =  {alum_core_fsm_sup,
        {?MODULE, start_link, [alum_core_fsm_sup]},
        permanent, 5000, supervisor, [?MODULE]
    },
    Master = {?PROCESS_VMASTER,
        {riak_core_vnode_master, start_link, [alum_core_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]
    },

    {ok, {{one_for_one, 5, 10}, [Coord, Coverage, Master]}};

init([alum_core_fsm_sup]) ->
    Fsm = {alum_core_fsm,
        {alum_core_fsm, start_link, []},
        temporary, 5000, worker, [alum_core_fsm]
    },

    {ok, {{simple_one_for_one, 0, 1}, [Fsm]}}.
