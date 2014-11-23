-module(alum).

%% API.
-export([
    get/1,
    put/1,
    list/1
]).

%% Public API


%% @doc Get a file content
get(FilePath) ->
    alum_core:fetch(FilePath).

%% @doc Lists all file names for a specific path
list(FilePath) ->
    alum_core:list(FilePath).

%% @doc Put content to a file
put(File) ->
    alum_core:store(File).