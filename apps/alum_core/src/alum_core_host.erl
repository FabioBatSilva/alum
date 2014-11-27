-module(alum_core_host).

-define(METADATA_PREFIX, {alum_core, hosts}).

-export([
    set_host/2,
    get_host/1,
    cache/1,
    n_val/1,
    r_val/1,
    w_val/1,
    name/1
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
        {error, Details} ->
            {error, Details};
        undefined ->
            {error, not_found};
        Props -> 
            {ok, [{name, Host} | Props]}
    end.

set_host(Host, Value) ->
    set_metadata(Host, merge_defaults(Value)).

name(Props) ->
    get_prop_value(name, Props).

cache(Props) ->
    get_prop_value(cache, Props).

n_val(Props) ->
    get_prop_value(n_val, Props).

r_val(Props) ->
    get_prop_value(r, Props).

w_val(Props) ->
    get_prop_value(w, Props).

set_metadata(Host, HostMeta) ->
    riak_core_metadata:put(?METADATA_PREFIX, host_key(Host), HostMeta).

get_metadata(Host) ->
    riak_core_metadata:get(?METADATA_PREFIX, host_key(Host)).

get_prop_value(Key, Props) ->
    proplists:get_value(Key, Props).

host_key(Name) ->
    Name.