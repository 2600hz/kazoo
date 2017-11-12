%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_util_test).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% For format_account_* tests
-export([format_account_id_raw/1
        ,format_account_id_encoded/1
        ,format_account_id_unencoded/1
        ,format_account_mod_id_from_year_month/1
        ,format_account_mod_id_from_now/1
        ,format_account_modb_raw/1
        ,format_account_modb_encoded/1
        ,format_account_modb_unencoded/1
        ]).

-define(AN_ACCOUNT_ID, <<"4fe69c5b61015084f1fe5684abc6e502">>).

%% PROPER TESTING
-ifdef(PROPER).

prop_pretty_print_bytes() ->
    ?FORALL({T, G, M, K, B}
           ,{range(0,3), range(0,1023), range(0,1023), range(0,1023), range(0,1023)}
           ,begin
                Bytes = (T * ?BYTES_T) + (G * ?BYTES_G) + (M * ?BYTES_M) + (K * ?BYTES_K) + B,
                Expected = iolist_to_binary(
                             lists:reverse(
                               lists:foldl(fun({0, "B"}, "") ->
                                                   ["B", <<"0">>];
                                              ({0, _}, Acc) -> Acc;
                                              ({N, Unit}, Acc) -> [Unit, kz_term:to_binary(N) | Acc]
                                           end
                                          ,[]
                                          ,[{T, "T"}
                                           ,{G, "G"}
                                           ,{M, "M"}
                                           ,{K, "K"}
                                           ,{B, "B"}
                                           ])
                              )
                            ),
                Result = kz_util:pretty_print_bytes(Bytes),
                ?WHENFAIL(io:format("~pT ~pG ~pM ~pK ~pB (~pb): ~p =:= ~p~n", [T, G, M, K, B, Bytes, Result, Expected])
                         ,Result =:= Expected
                         )
            end).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 20000,
      [?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.


%% Just to please coverage :)
log_test_() ->
    [?_assertEqual(ok, kz_util:log_stacktrace())
    ,?_assertEqual(ok, kz_util:log_stacktrace(erlang:get_stacktrace()))
    ].

calling_app_test_() ->
    [?_assertEqual(eunit_test, maps:get(app, kz_util:calling_process()))
    ,?_assertMatch(undefined, kz_util:get_app("kazoo"))
    ].

usages_test_() ->
    [?_assertEqual(true, is_integer(kz_util:bin_usage()))
    ,?_assertEqual(true, is_integer(kz_util:mem_usage()))
    ,?_assertEqual(true, kz_term:is_ne_binary(kz_util:node_name()))
    ,?_assertEqual(true, kz_term:is_ne_binary(kz_util:node_hostname()))
    ].

iolist_join_test_() ->
    [?_assertEqual([], kz_util:iolist_join($,, []))
    ,?_assertEqual([$a,<<" || ">>,$b,<<" || ">>,$c], kz_util:iolist_join(<<" || ">>, [$a,$b,$c]))
    ].

get_event_type_test_() ->
    EventCategory = {<<"Event-Category">>, <<"call">>},
    EventName = {<<"Event-Name">>, <<"CHANNEL_CONNECTED">>},
    [?_assertEqual({undefined,undefined}, kz_util:get_event_type([]))
    ,?_assertEqual({undefined,undefined}, kz_util:get_event_type(kz_json:from_list([])))
    ,?_assertEqual({<<"call">>,undefined}, kz_util:get_event_type([EventCategory]))
    ,?_assertEqual({<<"call">>,undefined}, kz_util:get_event_type(kz_json:from_list([EventCategory])))
    ,?_assertEqual({undefined,<<"CHANNEL_CONNECTED">>}, kz_util:get_event_type([EventName]))
    ,?_assertEqual({undefined,<<"CHANNEL_CONNECTED">>}, kz_util:get_event_type(kz_json:from_list([EventName])))
    ,?_assertEqual({<<"call">>,<<"CHANNEL_CONNECTED">>}, kz_util:get_event_type([EventCategory,EventName]))
    ,?_assertEqual({<<"call">>,<<"CHANNEL_CONNECTED">>}, kz_util:get_event_type(kz_json:from_list([EventCategory,EventName])))
    ].

put_callid_test_() ->
    ApiCallId = [{<<"Call-ID">>, <<"bla">>}],
    [?_assertEqual(<<"bla">>, begin kz_util:put_callid(<<"bla">>), kz_util:get_callid() end)
    ,?_assertEqual(bla, begin kz_util:put_callid(bla), kz_util:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_util:put_callid(ApiCallId), kz_util:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_util:put_callid(kz_json:from_list(ApiCallId)), kz_util:get_callid() end)
    ,?_assert(is_integer(begin kz_util:set_startup(), kz_util:startup() end))
    ].


uri_test_() ->
    [?_assertEqual(<<"http://test.com/path1/path2">>, kz_util:uri(<<"http://test.com">>, [<<"path1">>, <<"path2">>]))
    ,?_assertEqual(<<"http://192.168.0.1:8888/path1/path2">>, kz_util:uri(<<"http://192.168.0.1:8888/">>, [<<"path1">>, <<"path2">>]))
    ,?_assertEqual(<<"http://test.com/path1/path2">>, kz_util:uri(<<"http://test.com/">>, [<<"path1/">>, <<"path2/">>]))
    ].

normalize_account_name_test_() ->
    [?_assertEqual(undefined, kz_util:normalize_account_name(undefined))
    ,?_assertEqual(<<"blip2blop">>, kz_util:normalize_account_name(<<"Blip#2!Blop">>))
    ].

is_in_account_hierarchy_test_() ->
    [?_assertEqual(false, kz_util:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kz_util:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kz_util:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, true))
    ,?_assertEqual(false, kz_util:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, false))
    ,?_assertEqual(false, kz_util:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, false))
    ,?_assertEqual(false, kz_util:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, true))
    ,?_assertEqual(true, kz_util:is_in_account_hierarchy(?AN_ACCOUNT_ID, ?AN_ACCOUNT_ID, true))
    ].

is_system_admin_test_() ->
    [?_assertEqual(false, kz_util:is_system_admin(undefined))
    ].

is_account_enabled_test_() ->
    [?_assertEqual(false, kz_util:is_account_enabled(undefined))
    ].

is_account_expired_test_() ->
    [?_assertEqual(false, kz_util:is_account_expired(undefined))
    ].

try_load_module_test_() ->
    [?_assertEqual(false, kz_util:try_load_module(undefined))
    ,?_assertEqual(false, kz_util:try_load_module("undefined"))
    ,?_assertEqual(false, kz_util:try_load_module(<<"undefined">>))
    ,?_assertEqual(kz_util, kz_util:try_load_module("kz_util"))
    ,?_assertEqual(kz_util, kz_util:try_load_module(<<"kz_util">>))
    ,?_assertEqual(kz_util, kz_util:try_load_module(kz_util))
    ,?_assertEqual(false, kz_util:try_load_module(kz_term:to_list(?AN_ACCOUNT_ID)))
    ,?_assertEqual(false, kz_util:try_load_module(?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kz_util:try_load_module(kz_term:to_atom(?AN_ACCOUNT_ID,true)))
    ].

spawns_test_() ->
    [?_assert(is_pid(kz_util:spawn(fun () -> io:format("x") end)))
    ,?_assert(is_pid(kz_util:spawn(fun (X) -> io:format("~p",[X]) end, [x])))
    ,?_assert(is_pid(kz_util:spawn_link(fun () -> io:format("x") end)))
    ,?_assert(is_pid(kz_util:spawn_link(fun (X) -> io:format("~p",[X]) end, [x])))
    ,?_assertMatch({_,_}, kz_util:spawn_monitor(fun (X) -> io:format("~p",[X]) end, [x]))
    ].

resolve_uri_test_() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    [?_assertEqual(<<"http://pivot/script2.php">>, kz_util:resolve_uri(RawPath, Relative))
    ,?_assertEqual(<<"http://pivot/script2.php">>, kz_util:resolve_uri(RawPath, <<"/", Relative/binary>>))
    ,?_assertEqual(Relative, kz_util:resolve_uri(Relative, undefined))
    ,?_assertEqual(RawPath, kz_util:resolve_uri(Relative, RawPath))
    ,?_assertEqual(Relative, kz_util:resolve_uri(kz_term:to_list(Relative), undefined))
    ,?_assertEqual(RawPath, kz_util:resolve_uri(kz_term:to_list(Relative), RawPath))
    ,?_assertEqual(RawPath, kz_util:resolve_uri(Relative, kz_term:to_list(RawPath)))
    ,?_assertEqual(<<"http://host/d1/d2/a">>, kz_util:resolve_uri(<<"http://host/d1/d2/d3/file.ext">>, <<"../.././a">>))
    ].

resolve_uri_path_test_() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPathList = [<<"http:">>, <<>>, <<"pivot">>, <<"script2.php">>],
    [?_assertEqual(RawPathList, kz_util:resolve_uri_path(RawPath, Relative))
    ,?_assertEqual(RawPathList, kz_util:resolve_uri_path(RawPath, <<"/", Relative/binary>>))
    ].

account_formats_test_() ->
    AccountId = <<A:2/binary, B:2/binary, Rest:28/binary>> = kz_binary:rand_hex(16),
    AccountDbUn = list_to_binary(["account/", A, "/", B, "/", Rest]),
    AccountDbEn = list_to_binary(["account%2F", A, "%2F", B, "%2F", Rest]),

    {Y, M, _} = erlang:date(),
    TS = kz_time:now_s(),
    Now = os:timestamp(),
    Year = kz_term:to_binary(Y),
    Month = kz_date:pad_month(M),

    MODbId = list_to_binary([AccountId, "-", Year, Month]),
    MODbEn = list_to_binary([AccountDbEn, "-", Year, Month]),
    MODbUn = list_to_binary([AccountDbUn, "-", Year, Month]),

    Formats = [AccountId, AccountDbUn, AccountDbEn
              ,MODbId, MODbEn, MODbUn
              ],
    %% Note: the whole point of exporting some of these is so that function_clause can be caught
    Funs = [{fun kz_util:format_account_id/1, AccountId}
           ,{fun ?MODULE:format_account_id_raw/1, AccountId}
           ,{fun ?MODULE:format_account_id_encoded/1, AccountDbEn}
           ,{fun ?MODULE:format_account_id_unencoded/1, AccountDbUn}
           ,{fun kz_util:format_account_db/1, AccountDbEn}
           ,{fun kz_util:format_account_mod_id/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_year_month/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_now/1, MODbEn}
           ,{fun kz_util:format_account_modb/1, MODbId}
           ,{fun ?MODULE:format_account_modb_raw/1, MODbId}
           ,{fun ?MODULE:format_account_modb_encoded/1, MODbEn}
           ,{fun ?MODULE:format_account_modb_unencoded/1, MODbUn}
           ],
    [{format_title(Fun, Format, Expected)
     ,format_assert(Fun, Format, Expected)
     }
     || {Fun, Expected} <- Funs,
        Format <- Formats
    ] ++
        [?_assertEqual(undefined, kz_util:format_account_id(undefined, raw))
        ,?_assertEqual(<<"accounts">>, kz_util:format_account_id(<<"accounts">>, raw))
        ,?_assertEqual(MODbEn, kz_util:format_account_id(AccountDbEn, TS))
        ,?_assertEqual(MODbEn, kz_util:format_account_mod_id(AccountDbEn, TS))
        ,?_assertEqual(undefined, kz_util:format_account_id(undefined, Year, Month))
        ,?_assertEqual(MODbEn, kz_util:format_account_id(AccountDbEn, Year, Month))
        ,?_assertEqual(MODbEn, kz_util:format_account_id(AccountDbEn, Year, M))
        ,?_assertEqual(?KZ_TASKS_DB, kz_util:format_account_id(?KZ_TASKS_DB, raw))
        ,?_assertEqual(<<"bla">>, kz_util:format_account_id(<<"bla">>, raw))
        ].

format_assert(Fun, Format, Expected) ->
    Matchable = format_title(Fun, Format, Expected),
    case {is_simple_modb_converter(Matchable), Format} of
        {'true', ?MATCH_ACCOUNT_RAW(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_ENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_UNENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {_Else, Format} -> ?_assertEqual(Expected, Fun(Format))
    end.

format_title(Fun, Format, Expected) ->
    lists:flatten(
      io_lib:format("~p converting ~s to ~s", [Fun, Format, Expected])
     ).

is_simple_modb_converter("#Fun<kz_util.format_account_modb.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_util_test.format_account_modb_raw.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_util_test.format_account_modb_encoded.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_util_test.format_account_modb_unencoded.1>"++_) -> 'true';
is_simple_modb_converter(_Else) -> 'false'.

format_account_id_raw(F) -> kz_util:format_account_id(F, 'raw').
format_account_id_encoded(F) -> kz_util:format_account_id(F, 'encoded').
format_account_id_unencoded(F) -> kz_util:format_account_id(F, 'unencoded').
format_account_mod_id_from_year_month(F) ->
    {Year, Month, _} = erlang:date(),
    kz_util:format_account_mod_id(F, Year, Month).
format_account_mod_id_from_now(F) ->
    kz_util:format_account_mod_id(F, os:timestamp()).
format_account_modb_raw(F) -> kz_util:format_account_modb(F, 'raw').
format_account_modb_encoded(F) -> kz_util:format_account_modb(F, 'encoded').
format_account_modb_unencoded(F) -> kz_util:format_account_modb(F, 'unencoded').

-define(PP_TESTS, [{0, <<"0B">>, <<"0B">>}
                  ,{1, <<"1B">>, <<"1B">>}
                  ,{2, <<"2B">>, <<"2B">>}

                  ,{?BYTES_K-1, <<"1023B">>, <<"1023B">>}
                  ,{?BYTES_K, <<"1K">>, <<"1K">>}
                  ,{?BYTES_K+1, <<"1K1B">>, <<"1K">>}

                  ,{?BYTES_M-1, <<"1023K1023B">>, <<"1023K">>}
                  ,{?BYTES_M, <<"1M">>, <<"1M">>}
                  ,{?BYTES_M+1, <<"1M1B">>, <<"1M">>}

                  ,{?BYTES_G-1, <<"1023M1023K1023B">>, <<"1023M">>}
                  ,{?BYTES_G, <<"1G">>, <<"1G">>}
                  ,{?BYTES_G+1, <<"1G1B">>, <<"1G">>}

                  ,{?BYTES_T-1, <<"1023G1023M1023K1023B">>, <<"1023G">>}
                  ,{?BYTES_T, <<"1T">>, <<"1T">>}
                  ,{?BYTES_T+1, <<"1T1B">>, <<"1T">>}
                  ]).

pretty_print_bytes_test_() ->
    Tests = ?PP_TESTS,
    [?_assertEqual({Bytes, FullFormatted, TruncFormatted}
                  ,{Bytes
                   ,kz_util:pretty_print_bytes(Bytes, 'full')
                   ,kz_util:pretty_print_bytes(Bytes, 'truncated')
                   }
                  )
     || {Bytes, FullFormatted, TruncFormatted} <- Tests
    ].


runs_in_test_() ->
    [?_assertEqual(timeout, kz_util:runs_in(1, fun timer:sleep/1, [10]))
    ,?_assertEqual({ok,ok}, kz_util:runs_in(10, fun timer:sleep/1, [1]))
    ,?_assertEqual(timeout, kz_util:runs_in(1.0, fun timer:sleep/1, [10]))
    ,?_assertEqual({ok,ok}, kz_util:runs_in(10.0, fun timer:sleep/1, [1]))
    ].


uniq_test_() ->
    [?_assertEqual([], kz_util:uniq([]))
    ,?_assertEqual([{module_name, <<"my_module">>}]
                  ,kz_util:uniq([{module_name, <<"my_module">>}
                                ,{module_name, <<"blaaa">>}
                                ,{module_name, false}
                                ])
                  )
    ].
