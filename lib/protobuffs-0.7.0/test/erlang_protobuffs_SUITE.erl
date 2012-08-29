%%%-------------------------------------------------------------------
%%% @author David Åberg <>
%%% @copyright (C) 2010, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2010 by David Åberg <>
%%%-------------------------------------------------------------------
-module(erlang_protobuffs_SUITE).

-compile(export_all).
-include("quickcheck_setup.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 60}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) -> [{num_tests, 1000} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) -> ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) -> Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) -> ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) -> Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) -> ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [protobuffs_test_case, protobuffs_packed_test_case,
     test_proto_files, test_extendable_messages].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
protobuffs_test_case() -> [].

protobuffs_packed_test_case() -> [].

test_proto_files() -> [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
protobuffs_test_case(Config) ->
    NumTests = (?config(num_tests, Config)),
    true = quickcheck(numtests(NumTests,
                               protobuffs_proper:prop_protobuffs())).

protobuffs_packed_test_case(Config) ->
    NumTests = (?config(num_tests, Config)),
    true = quickcheck(numtests(NumTests,
                               protobuffs_proper:prop_protobuffs_packed())).

test_proto_files(Config) ->
    DataDir = (?config(data_dir, Config)),
    NumTests = (?config(num_tests, Config)),
    ProtoFiles = filelib:wildcard(filename:join([DataDir,
						 "proto", "*.proto"])),
    TestProtoFile = fun (ProtoFile, Acc) ->
			    Path = filename:absname(ProtoFile),
			    Message = filename:basename(ProtoFile, ".proto"),
			    test_server:format("~n===Testcase ~p, testing message ~p===~n",
					       [self(), Message]),
			    test_server:format("Testcase ~p, parse file ~p",
					       [self(), Path]),
			    protobuffs_compile:scan_file(Path,
							 [{imports_dir,
							   [filename:join([DataDir,
									   "proto"]),
							    filename:join([DataDir,
									   "proto",
									   "import"])]}]),
			    Test = list_to_atom("proper_protobuffs_" ++
						  Message),
			    Result = quickcheck(numtests(NumTests,
                                             protobuffs_proper:Test()),
						       [long_result,
							{on_output,
							 fun (".", _) -> ok;
							     (S, F) ->
								 test_server:format(S,
										    F)
							 end}]),
			    case Result of
			      true ->
				  test_server:format("Test ~p: ok~n~n~n",
						     [Message]),
				  Acc andalso true;
			      F ->
				  test_server:format("Test ~p: Failed with ~p~n~n~n",
						     [Message, Result]),
				  false
			    end
		    end,
    case lists:foldl(TestProtoFile, true, ProtoFiles) of
      true -> ok;
      _ -> ct:fail("One or more property test cases failed")
    end.

test_extendable_messages(Config) ->
    DataDir = (?config(data_dir, Config)),
    NumTests = (?config(num_tests, Config)),
    ProtoFiles = [begin
		    Filename = filename:join([DataDir, "proto", X]),
		    Path = filename:absname(Filename),
		    Options = [{imports_dir,
				[filename:join([DataDir, "proto"]),
				 filename:join([DataDir, "proto", "import"])]}],
		    protobuffs_compile:scan_file(Path, Options)
		  end
		  || X <- ["extend.proto", "extensions.proto"]],
    Tests = [proper_protobuffs_extend_degraded,
	     proper_protobuffs_extend_assign,
	     proper_protobuffs_extend_get,
	     proper_protobuffs_extend_has_enum,
	     proper_protobuffs_extend_has_message,
	     proper_protobuffs_extend_has_string],
    Folder = fun (Testname, Acc) ->
		     test_server:format("~n===Extensions Testcase ~p===~n",
					[Testname]),
		     Result = quickcheck(numtests(NumTests,
								protobuffs_proper:Testname()),
						[long_result,
						 {on_output,
						  fun (".", _) -> ok;
						      (S, F) ->
							  test_server:format(S,
									     F)
						  end}]),
		     case Result of
		       true ->
			   test_server:format("Test ~p:  ok~n~n~n", [Testname]),
			   Acc andalso true;
		       _ ->
			   test_server:format("Test ~p:  Failed with ~p~n~n~n",
					      [Testname, Result]),
			   false
		     end
	     end,
    BigRes = lists:foldl(Folder, true, Tests),
    case BigRes of
      true -> ok;
      _ -> ct:fail("One or more extension tests failed")
    end.

%%---------------------------------------------------------------------
%% Help flies
%%---------------------------------------------------------------------
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    file:close(InFile),
    {ok, Parsed} = protobuffs_parser:parse(Acc),
    Parsed.

loop(InFile, Acc) ->
    case io:request(InFile,
		    {get_until, prompt, protobuffs_scanner, token, [1]})
	of
      {ok, Token, _EndLine} -> loop(InFile, Acc ++ [Token]);
      {error, token} -> exit(scanning_error);
      {eof, _} -> Acc
    end.
