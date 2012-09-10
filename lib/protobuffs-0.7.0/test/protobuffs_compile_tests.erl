%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by David AAberg <davabe@hotmail.com>

-module(protobuffs_compile_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Modules = [protobuffs_file],
    meck:new(Modules),
    meck:expect(protobuffs_file, open,
		fun (_, _) -> {ok, in_file} end),
    meck:expect(protobuffs_file, close, fun (_) -> ok end),
    meck:expect(protobuffs_file, request,
		fun (_) -> {eof, dummy} end),
    meck:expect(protobuffs_file, compile_forms,
		fun (_, _) -> {ok, dummy, <<"Bytest">>, dummy} end),
    meck:expect(protobuffs_file, write_file,
		fun (_, _) -> ok end),
    meck:expect(protobuffs_file, format,
		fun (_, _, _) -> ok end),
    meck:expect(protobuffs_file, path_open,
		fun (Path, FileName, _) ->
			{ok, io_device, filename:join([Path, FileName])}
		end),
    Modules.

cleanup(Modules) -> meck:unload(Modules).

scan_file_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:scan_file(dummy_file))),
      ?_assertMatch(ok,
		    (protobuffs_compile:scan_file("dummy_file.proto")))]}.

scan_string_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:scan_string("", "dummy")))]}.

generate_source_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch(ok,
		    (protobuffs_compile:generate_source(dummy_file))),
      ?_assertMatch(ok,
		    (protobuffs_compile:generate_source("dummy_file.proto")))]}.

parse_imports_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [?_assertMatch([],
		    (protobuffs_compile:parse_imports([], dummy_path))),
      ?_assertMatch([{import, dummy_import_file}, file_boundary],
		    (protobuffs_compile:parse_imports([{import,
							dummy_import_file}],
						      "dummy_path"))),
      ?_assertMatch([what_ever],
		    (protobuffs_compile:parse_imports([what_ever],
						      dummy_path)))]}.
