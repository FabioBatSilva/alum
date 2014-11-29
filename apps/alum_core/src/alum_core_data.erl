-module(alum_core_data).

-include_lib("kernel/include/file.hrl").

%% API.
-export([
    fetch/1,
    store/1,
    list/1
]).

%% API.

fetch(FilePath) ->
    RealPath = file_path(FilePath),
    FileRead = file:read_file(RealPath),

    case FileRead of
        {ok, Body}      -> {ok, {Body, file_type(RealPath)}};
        {error, enoent} -> {not_found, enoent};
        Error           -> Error
    end.

store({Host, Path, Content}) ->
    FilePath = {Host, Path},
    RealPath = file_path(FilePath),
    ok       = filelib:ensure_dir(RealPath),

    file:write_file(RealPath, Content).

list({Host, Path}) ->
    FilePath = {Host, Path},
    RealPath = file_path(FilePath),

    case file:list_dir(RealPath) of
        {ok, List}      -> {ok, List};
        {error, enoent} -> {not_found, enoent};
        Error           -> Error
    end.

file_path({Host, Path}) ->
    BasePath = get_root_content_path(),
    HostPath = filename:join([BasePath, Host]),
    RealPath = <<HostPath/binary, Path/binary>>,

    RealPath.

file_type(RealPath) ->
    mimetypes:filename(RealPath).

get_root_content_path() ->
    filename:absname(alum_core_config:get_content_path()).
