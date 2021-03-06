-module(alum_core_host).

-define(METADATA_PREFIX, {alum_core, hosts}).

-export([
    set_host/2,
    get_host/1,
    cache/1,
    n_val/1,
    name/1
]).

defaults() ->
    [
        {cache, 3600},
        {n_val, 3}
    ].

merge(Overriding, Other) ->
    lists:ukeymerge(1,
        lists:ukeysort(1, Overriding),
        lists:ukeysort(1, Other)
    ).

merge_defaults(Overriding) ->
    merge(parse_values(Overriding), defaults()).

parse_values(Config) when erlang:is_list(Config) ->
    lists:map(fun parse_values/1, Config);

parse_values({Key, Value}) when erlang:is_binary(Key) ->
    {erlang:binary_to_atom(Key), Value};

parse_values({Key, Value}) when erlang:is_list(Key) ->
    {erlang:list_to_atom(Key), Value};

parse_values({Key, Value}) ->
    {Key, Value}.

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

set_metadata(Host, HostMeta) ->
    riak_core_metadata:put(?METADATA_PREFIX, host_key(Host), HostMeta).

get_metadata(Host) ->
    riak_core_metadata:get(?METADATA_PREFIX, host_key(Host)).

get_prop_value(Key, Props) ->
    proplists:get_value(Key, Props).

host_key(Name) ->
    Name.
