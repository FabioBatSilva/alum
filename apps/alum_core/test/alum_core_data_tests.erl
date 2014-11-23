-module(alum_core_data_tests).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

list_test() ->

    ok = meck:new(file, [passthrough, unstick]),
    ok = meck:new(alum_core_config, [passthrough, unstick]),

    BasePath    = <<"/tmp">>,
    RealPath    = <<"/tmp/static.com/files">>,
    FilePath    = {<<"static.com">>, <<"/files">>},
    Expected    = [<<"image.png">>, <<"doc.txt">>],

    meck:expect(alum_core_config, get_content_path, [], BasePath),
    meck:expect(file, list_dir, [RealPath], {ok, Expected}),

    Actual = alum_core_data:list(FilePath),

    meck:validate(alum_core_config),
    meck:unload(alum_core_config),
    meck:validate(file),
    meck:unload(file),

    ?assertEqual(Expected, Actual).

fetch_test() ->

    ok = meck:new(file, [passthrough, unstick]),
    ok = meck:new(mimetypes, [passthrough, unstick]),
    ok = meck:new(alum_core_config, [passthrough, unstick]),

    BasePath    = <<"/tmp">>,
    FileContent = <<"CONTENT">>,
    FileType    = <<"image/png">>,
    RealPath    = <<"/tmp/static.com/image.png">>,
    FilePath    = {<<"static.com">>, <<"/image.png">>},
    Expected    = {ok, {FileContent, FileType}},

    meck:expect(alum_core_config, get_content_path, [], BasePath),
    meck:expect(file, read_file, [RealPath], {ok, FileContent}),
    meck:expect(mimetypes, filename, [RealPath], FileType),

    Actual = alum_core_data:fetch(FilePath),

    meck:validate(alum_core_config),
    meck:unload(alum_core_config),
    meck:validate(mimetypes),
    meck:unload(mimetypes),
    meck:validate(file),
    meck:unload(file),

    ?assertEqual(Expected, Actual).

fetch_error_test() ->

    ok = meck:new(file, [passthrough, unstick]),
    ok = meck:new(mimetypes, [passthrough, unstick]),
    ok = meck:new(alum_core_config, [passthrough, unstick]),

    BasePath    = <<"/tmp">>,
    RealPath    = <<"/tmp/static.com/enoent-file.png">>,
    FilePath    = {<<"static.com">>, <<"/enoent-file.png">>},
    Expected    = {not_found},

    meck:expect(alum_core_config, get_content_path, [], BasePath),
    meck:expect(file, read_file, [RealPath], {error, enoent}),

    Actual = alum_core_data:fetch(FilePath),

    ?assertEqual(false, meck:called(mimetypes, filename, [RealPath])),
    meck:validate(alum_core_config),
    meck:unload(alum_core_config),
    meck:validate(mimetypes),
    meck:unload(mimetypes),
    meck:validate(file),
    meck:unload(file),

    ?assertEqual(Expected, Actual).

store_test() ->

    ok = meck:new(file, [passthrough, unstick]),
    ok = meck:new(filelib, [passthrough, unstick]),
    ok = meck:new(alum_core_config, [passthrough, unstick]),

    BasePath    = <<"/tmp">>,
    FileContent = <<"CONTENT">>,
    FilePath    = <<"/tmp/static.com/image.png">>,
    File        = {<<"static.com">>, <<"/image.png">>, FileContent},

    meck:expect(alum_core_config, get_content_path, [], BasePath),
    meck:expect(file, write_file, [FilePath, FileContent], ok),
    meck:expect(filelib, ensure_dir, [FilePath], ok),

    Actual = alum_core_data:store(File),

    meck:validate(alum_core_config),
    meck:unload(alum_core_config),
    meck:validate(filelib),
    meck:unload(filelib),
    meck:validate(file),
    meck:unload(file),

    ?assertEqual(ok, Actual).

store_error_test() ->

    ok = meck:new(file, [passthrough, unstick]),
    ok = meck:new(alum_core_config, [passthrough, unstick]),

    BasePath    = <<"/tmp">>,
    FileContent = <<"CONTENT">>,
    FilePath    = <<"/tmp/static.com/enospc-img.png">>,
    File        = {<<"static.com">>, <<"/enospc-img.png">>, FileContent},

    meck:expect(alum_core_config, get_content_path, [], BasePath),
    meck:expect(file, write_file, [FilePath, FileContent], {error, enospc}),

    Actual = alum_core_data:store(File),

    meck:validate(alum_core_config),
    meck:unload(alum_core_config),
    meck:validate(file),
    meck:unload(file),

    ?assertEqual({error, enospc}, Actual).

