-module(alum_core_host).

-define(METADATA_PREFIX, {alum_core, hosts}).

-export([
    set_host/2,
    get_host/1,
    name/1,
    n_val/1
]).

defaults() ->
    [
        {cache, 3600},
        {r, quorum},
        {w, quorum},
        {n_val, 3}
    ].

merge(Overriding, Other) ->
    lists:ukeymerge(1,
        lists:ukeysort(1, Overriding),
        lists:ukeysort(1, Other)
    ).

merge_defaults(Overriding) ->
    merge(Overriding, defaults()).

get_host(Host) ->
    case get_metadata(Host) of
        {error, _} = Error -> 
            {error, Error};
        undefined ->
            {error, not_found};
        Props -> 
            {ok, [{name, Host} | Props]}
    end.

set_host(Host, Value) ->
    set_metadata(Host, merge_defaults(Value)).

set_metadata(Host, HostMeta) ->
    riak_core_metadata:put(?METADATA_PREFIX, host_key(Host), HostMeta).

get_metadata(Host) ->
    riak_core_metadata:get(?METADATA_PREFIX, host_key(Host)).

host_key(Name) ->
    Name.

name(Props) ->
    proplists:get_value(name, Props).

n_val(Props) ->
    proplists:get_value(n_val, Props).
