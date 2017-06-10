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

-include_lib("kazoo/include/kz_types.hrl").
-include("kz_databases.hrl").

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

-include_lib("kazoo/include/kz_types.hrl").

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

prop_to_integer() ->
    ?FORALL({F, I}
           ,{float(), integer()}
           ,begin
                Is = [[Fun(N), N]
                      || Fun <- [fun kz_term:to_list/1
                                ,fun kz_term:to_binary/1
                                ],
                         N <- [F, I]
                     ],
                lists:all(fun([FN, N]) ->
                                  erlang:is_integer(kz_term:to_integer(N))
                                      andalso erlang:is_integer(kz_term:to_integer(FN))
                          end
                         ,Is
                         )
            end).

prop_to_number() ->
    ?FORALL({F, I}
           ,{float(), integer()}
           ,begin
                Is = [[Fun(N), N]
                      || Fun <- [fun kz_term:to_list/1
                                ,fun kz_term:to_binary/1
                                ],
                         N <- [F, I]
                     ],
                lists:all(fun([FN, N]) ->
                                  erlang:is_number(kz_term:to_number(N))
                                      andalso erlang:is_number(kz_term:to_number(FN))
                          end
                         ,Is
                         )
            end).

prop_to_float() ->
    ?FORALL({F, I}
           ,{float(), integer()}
           ,begin
                Fs = [[Fun(N), N]
                      || Fun <- [fun kz_term:to_list/1
                                ,fun kz_term:to_binary/1
                                ],
                         N <- [F, I]
                     ],
                lists:all(fun([FN, N]) ->
                                  erlang:is_float(kz_term:to_float(N))
                                      andalso erlang:is_float(kz_term:to_float(FN))
                          end
                         ,Fs
                         )
            end).

prop_to_list() ->
    ?FORALL({A, L, B, I, F}
           ,{atom(), list(), binary(), integer(), float()}
           ,lists:all(fun(X) -> is_list(kz_term:to_list(X)) end, [A, L, B, I, F])
           ).

%%-type iolist() :: maybe_improper_list(char() | binary() | iolist(), binary() | []).
prop_to_binary() ->
    ?FORALL({A, L, B, I, F, IO}
           ,{atom(), list(range(0,255)), binary(), integer(), float(), iolist()}
           ,lists:all(fun(X) -> is_binary(kz_term:to_binary(X)) end, [A, L, B, I, F, IO])
           ).

prop_iolist_t() ->
    ?FORALL(IO, iolist(), is_binary(kz_term:to_binary(IO))).

prop_to_from_hex() ->
    ?FORALL({F}, {binary()},
            begin
                F =:= kz_binary:from_hex(kz_term:to_hex_binary(F))
            end).

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
      [
       ?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.

to_x_test_() ->
    TS = kz_time:current_tstamp(),
    [?_assertError(badarg, kz_term:to_integer(1.0, strict))
    ,?_assertEqual(1, kz_term:to_integer(1.0, notstrict))
    ,?_assertError(badarg, kz_term:to_float(1, strict))
    ,?_assertEqual(1.0, kz_term:to_float(1, notstrict))
    ,?_assertEqual(list_to_binary(pid_to_list(self())), kz_term:to_binary(self()))
    ,?_assertEqual(to_atom, kz_term:to_atom(to_atom))
    ,?_assertEqual(to_atom, kz_term:to_atom("to_atom"))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>, false))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>, ["to_atom"]))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(TS))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(kz_term:to_list(TS)))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(kz_term:to_binary(TS)))
    ].


%% Just to please coverage :)
log_test_() ->
    [?_assertEqual(ok, kz_util:log_stacktrace())
    ,?_assertEqual(ok, kz_util:log_stacktrace(erlang:get_stacktrace()))
    ].

calling_app_test_() ->
    [?_assertEqual(eunit_test, maps:get(app, kz_util:calling_process()))
    ,?_assertMatch(undefined, kz_util:get_app("kazoo"))
    ].

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, kz_binary:pad(<<"12345">>, 10, <<"0">>)).

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

shuffle_list_test_() ->
    [?_assertEqual([], kz_term:shuffle_list([]))
    ,?_assertEqual([42], kz_term:shuffle_list([42]))
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

pos_test_() ->
    [?_assertEqual(-1, kz_binary:pos($A, <<>>))
    ,?_assertEqual(0, kz_binary:pos($A, <<$A>>))
    ,?_assertEqual(0, kz_binary:pos($,, <<",,,">>))
    ,?_assertEqual(1, kz_binary:pos($,, <<"A,,">>))
    ,?_assertEqual(2, kz_binary:pos($', <<"A,'">>))
    ,?_assertEqual(-1, kz_binary:pos($B, <<"A,'">>))
    ].

closests_test_() ->
    [?_assertEqual([], kz_binary:closests([$A], <<>>))
    ,?_assertEqual([{$B,1}], kz_binary:closests([$B,$i], <<"ABAAABA">>))
    ,?_assertEqual([{$B,1}, {$i,6}], kz_binary:closests([$B,$i], <<"ABAAABiA">>))
    ].

to_hex_test_() ->
    [?_assertEqual("626c61", kz_term:to_hex(bla))
    ,?_assertEqual("626c61", kz_term:to_hex("bla"))
    ,?_assertEqual("626c61", kz_term:to_hex(<<"bla">>))
    ,?_assertEqual(<<"626c61">>, kz_binary:hexencode(bla))
    ,?_assertEqual(<<"626c61">>, kz_binary:hexencode("bla"))
    ,?_assertEqual(<<"626c61">>, kz_binary:hexencode(<<"bla">>))
    ,?_assertEqual(30, byte_size(kz_binary:rand_hex("15")))
    ,?_assertEqual(32, byte_size(kz_binary:rand_hex(<<"16">>)))
    ].

put_callid_test_() ->
    ApiCallId = [{<<"Call-ID">>, <<"bla">>}],
    [?_assertEqual(<<"bla">>, begin kz_util:put_callid(<<"bla">>), kz_util:get_callid() end)
    ,?_assertEqual(bla, begin kz_util:put_callid(bla), kz_util:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_util:put_callid(ApiCallId), kz_util:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_util:put_callid(kz_json:from_list(ApiCallId)), kz_util:get_callid() end)
    ,?_assert(is_integer(begin kz_util:set_startup(), kz_util:startup() end))
    ].

join_binary_test_() ->
    [?_assertEqual(<<>>, kz_binary:join([]))
    ,?_assertEqual(<<"f, o, o">>, kz_binary:join([<<"f">>, <<"o">>, <<"o">>]))
    ,?_assertEqual(<<"foo">>, kz_binary:join([<<"foo">>], <<", ">>))
    ,?_assertEqual(<<"foo, bar">>, kz_binary:join([<<"foo">>, <<"bar">>], <<", ">>))
    ,?_assertEqual(<<"foo, bar, baz">>, kz_binary:join([<<"foo">>, <<"bar">>, <<"baz">>], <<", ">>))
    ].

ucfirst_binary_test_() ->
    [?_assertEqual(<<"Foo">>, kz_binary:ucfirst(<<"foo">>))
    ,?_assertEqual(<<"Foo">>, kz_binary:ucfirst(<<"Foo">>))
    ,?_assertEqual(<<"FOO">>, kz_binary:ucfirst(<<"FOO">>))
    ,?_assertEqual(<<"1oo">>, kz_binary:ucfirst(<<"1oo">>))
    ,?_assertEqual(<<"100">>, kz_binary:ucfirst(<<"100">>))
    ,?_assertEqual(<<"1FF">>, kz_binary:ucfirst(<<"1FF">>))
    ].

lcfirst_binary_test_() ->
    [?_assertEqual(<<"foo">>, kz_binary:lcfirst(<<"foo">>))
    ,?_assertEqual(<<"foo">>, kz_binary:lcfirst(<<"Foo">>))
    ,?_assertEqual(<<"fOO">>, kz_binary:lcfirst(<<"FOO">>))
    ,?_assertEqual(<<"1oo">>, kz_binary:lcfirst(<<"1oo">>))
    ,?_assertEqual(<<"100">>, kz_binary:lcfirst(<<"100">>))
    ,?_assertEqual(<<"1FF">>, kz_binary:lcfirst(<<"1FF">>))
    ].

to_lower_binary_test_() ->
    [?_assertEqual(undefined, kz_term:to_lower_binary(undefined))
    ,?_assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"foo">>))
    ,?_assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"Foo">>))
    ,?_assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"FoO">>))
    ,?_assertEqual(<<"f00">>, kz_term:to_lower_binary(<<"f00">>))
    ,?_assertEqual(<<"f00">>, kz_term:to_lower_binary(<<"F00">>))
    ,?_assertEqual(<<"f00">>, kz_term:to_lower_binary("F00"))
    ].

to_upper_binary_test_() ->
    [?_assertEqual(undefined, kz_term:to_upper_binary(undefined))
    ,?_assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"foo">>))
    ,?_assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"Foo">>))
    ,?_assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"FoO">>))
    ,?_assertEqual(<<"F00">>, kz_term:to_upper_binary(<<"f00">>))
    ,?_assertEqual(<<"F00">>, kz_term:to_upper_binary(<<"F00">>))
    ,?_assertEqual(<<"F00">>, kz_term:to_upper_binary("F00"))
    ].

to_lower_string_test_() ->
    [?_assertEqual(undefined, kz_term:to_lower_string(undefined))
    ,?_assertEqual("foo", kz_term:to_lower_string("foo"))
    ,?_assertEqual("foo", kz_term:to_lower_string("Foo"))
    ,?_assertEqual("foo", kz_term:to_lower_string("FoO"))
    ,?_assertEqual("f00", kz_term:to_lower_string("f00"))
    ,?_assertEqual("f00", kz_term:to_lower_string("F00"))
    ,?_assertEqual("f00", kz_term:to_lower_string(<<"F00">>))
    ].

to_upper_string_test_() ->
    [?_assertEqual(undefined, kz_term:to_upper_string(undefined))
    ,?_assertEqual("FOO", kz_term:to_upper_string("foo"))
    ,?_assertEqual("FOO", kz_term:to_upper_string("Foo"))
    ,?_assertEqual("FOO", kz_term:to_upper_string("FoO"))
    ,?_assertEqual("F00", kz_term:to_upper_string("f00"))
    ,?_assertEqual("F00", kz_term:to_upper_string("F00"))
    ,?_assertEqual("F00", kz_term:to_upper_string(<<"F00">>))
    ].

to_case_char_test_() ->
    [?_assertEqual(16#F8, kz_term:to_lower_char(16#D8))
    ,?_assertEqual(16#E0, kz_term:to_lower_char(16#C0))
    ,?_assertEqual(16#D8, kz_term:to_upper_char(16#F8))
    ,?_assertEqual(16#C0, kz_term:to_upper_char(16#E0))
    ].

strip_binary_test_() ->
    [?_assertEqual(<<"foo">>, kz_binary:strip(<<"foo">>))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"foo ">>))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<" foo ">>))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"  foo  ">>))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"     foo">>))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"     foo   ">>, both))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"     foo   ">>, [left,right]))

    ,?_assertEqual(<<"foo">>, kz_binary:strip_left(<<"foo">>, $\s))
    ,?_assertEqual(<<"foo">>, kz_binary:strip_left(<<" foo">>, $\s))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<" foo">>, left))
    ,?_assertEqual(<<"foo ">>, kz_binary:strip_left(<<" foo ">>, $\s))
    ,?_assertEqual(<<"foo ">>, kz_binary:strip_left(<<"foo ">>, $\s))

    ,?_assertEqual(<<"foo">>, kz_binary:strip_right(<<"foo">>, $\s))
    ,?_assertEqual(<<" foo">>, kz_binary:strip_right(<<" foo">>, $\s))
    ,?_assertEqual(<<" foo">>, kz_binary:strip_right(<<" foo ">>, $\s))
    ,?_assertEqual(<<"foo">>, kz_binary:strip_right(<<"foo ">>, $\s))
    ,?_assertEqual(<<"foo">>, kz_binary:strip(<<"foo ">>, right))
    ].

to_boolean_test_() ->
    All = [<<"true">>, "true", 'true', <<"false">>, "false", 'false'],
    NotAll = [0, 123, 1.23, "123", "abc", 'abc', <<"abc">>, <<"123">>, {'what', 'is', 'this', 'doing', 'here'}],
    [?_assertEqual('true', lists:all(fun(X) ->
                                             try kz_term:to_boolean(X) of
                                                 _ -> 'true'
                                             catch _:_ -> 'false'
                                             end
                                     end
                                    ,All
                                    )
                  )
    ,?_assertEqual('true', lists:all(fun(X) ->
                                             try kz_term:to_boolean(X) of
                                                 _ -> 'false'
                                             catch _:_ -> 'true'
                                             end
                                     end
                                    ,NotAll
                                    )
                  )
    ].

strip_test() ->
    ?assertEqual(kz_binary:strip(<<"...Hello.....">>, $.), <<"Hello">>).

uri_test_() ->
    [?_assertEqual(<<"http://test.com/path1/path2">>, kz_util:uri(<<"http://test.com">>, [<<"path1">>, <<"path2">>]))
    ,?_assertEqual(<<"http://192.168.0.1:8888/path1/path2">>, kz_util:uri(<<"http://192.168.0.1:8888/">>, [<<"path1">>, <<"path2">>]))
    ,?_assertEqual(<<"http://test.com/path1/path2">>, kz_util:uri(<<"http://test.com/">>, [<<"path1/">>, <<"path2/">>]))
    ].

suffix_binary_test_() ->
    [?_assertEqual(false, kz_binary:suffix(<<>>, <<"1">>))
    ,?_assertEqual(false, kz_binary:suffix(<<"1">>, <<>>))
    ,?_assertEqual('true', kz_binary:suffix(<<"34">>, <<"1234">>))
    ,?_assertEqual('false', kz_binary:suffix(<<"34">>, <<"12345">>))
    ,?_assertEqual('false', kz_binary:suffix(<<"1234">>, <<"1">>))
    ].

clean_binary_test_() ->
    [?_assertEqual(<<>>, kz_binary:clean(<<>>))
    ,?_assertEqual(<<"bla">>, kz_binary:clean(<<"bla">>))
    ,?_assertEqual(<<"bla">>, kz_binary:clean(<<"bla  ">>))
    ,?_assertEqual(<<"bla">>, kz_binary:clean(<<"  bla">>))
    ,?_assertEqual(<<"bla">>, kz_binary:clean(<<"  bla  ">>))
    ,?_assertEqual(<<"bla">>, kz_binary:clean(<<" b l a ">>))
    ,?_assertEqual(<<"bla\n">>, kz_binary:clean(<<" b l a \n">>))
    ].

binary_hashes_test_() ->
    [?_assertEqual(<<"d41d8cd98f00b204e9800998ecf8427e">>, kz_binary:md5(<<>>))
    ,?_assertEqual("44add22b6f3179b751eafd68ee370f7d", kz_term:a1hash(<<"u">>, <<"r">>, <<"p">>))
    ].

float_bounds_test_() ->
    [?_assertEqual(1, kz_term:floor(1.0))
    ,?_assertEqual(1, kz_term:floor(1.2))
    ,?_assertEqual(1, kz_term:floor(1.5))
    ,?_assertEqual(1, kz_term:floor(1.7))
    ,?_assertEqual(1, kz_term:ceiling(1.0))
    ,?_assertEqual(2, kz_term:ceiling(1.2))
    ,?_assertEqual(2, kz_term:ceiling(1.5))
    ,?_assertEqual(2, kz_term:ceiling(1.7))
    ].

binary_reverse_test_() ->
    [?_assertEqual(<<>>, kz_binary:reverse(<<>>))
    ,?_assertEqual(<<"B a">>, kz_binary:reverse(<<"a B">>))
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

get_account_realm_test_() ->
    [?_assertEqual(undefined, kz_util:get_account_realm(undefined, ?AN_ACCOUNT_ID))
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

error_to_binary_test_() ->
    [?_assertEqual(<<"oops">>, kz_term:error_to_binary({error, oops}))
    ,?_assertEqual(<<"oops">>, kz_term:error_to_binary(oops))
    ,?_assertEqual(<<"Unknown Error">>, kz_term:error_to_binary(fun io:format/1))
    ].

is_true_false_test_() ->
    [?_assertEqual(true, kz_term:is_true(<<"true">>))
    ,?_assertEqual(true, kz_term:is_true("true"))
    ,?_assertEqual(true, kz_term:is_true(true))
    ,?_assertEqual(false, kz_term:is_true("tru"))
    ,?_assertEqual(false, kz_term:is_true(<<"undefined">>))
    ,?_assertEqual(false, kz_term:is_true(undefined))
    ,?_assertEqual(false, kz_term:is_true(<<"null">>))
    ,?_assertEqual(false, kz_term:is_true(null))
    ,?_assertEqual(false, kz_term:is_true(<<"false">>))
    ,?_assertEqual(true, kz_term:always_true(bla))
    ,?_assertEqual(true, kz_term:is_false(<<"false">>))
    ,?_assertEqual(true, kz_term:is_false("false"))
    ,?_assertEqual(true, kz_term:is_false(false))
    ,?_assertEqual(false, kz_term:is_false("flse"))
    ,?_assertEqual(false, kz_term:is_false(<<"undefined">>))
    ,?_assertEqual(false, kz_term:is_false(undefined))
    ,?_assertEqual(false, kz_term:is_false(<<"null">>))
    ,?_assertEqual(false, kz_term:is_false(null))
    ,?_assertEqual(false, kz_term:is_false(<<"true">>))
    ,?_assertEqual(false, kz_term:always_false(bla))
    ,?_assertEqual(false, kz_term:is_ne_binary(bla))
    ,?_assertEqual(true, kz_term:is_ne_binary(<<"bla">>))
    ,?_assertEqual(false, kz_term:is_ne_binaries(<<"bla">>))
    ,?_assertEqual(true, kz_term:is_ne_binaries([]))
    ,?_assertEqual(true, kz_term:is_ne_binaries([<<"cnam">>, <<"bla">>]))
    ,?_assertEqual(false, kz_term:is_ne_binaries([undefined, <<"bla">>]))
    ,?_assertEqual(true, kz_term:is_boolean(<<"true">>))
    ,?_assertEqual(true, kz_term:is_boolean(<<"false">>))
    ,?_assertEqual(true, kz_term:is_boolean("true"))
    ,?_assertEqual(true, kz_term:is_boolean("false"))
    ,?_assertEqual(true, kz_term:is_boolean(true))
    ,?_assertEqual(true, kz_term:is_boolean(false))
    ,?_assertEqual(false, kz_term:is_boolean(bla))
    ,?_assertEqual(false, kz_term:is_boolean(<<"undefined">>))
    ].

is_empty_test_() ->
    [?_assertEqual(true, kz_term:is_empty(0))
    ,?_assertEqual(true, kz_term:is_empty(0.0))
    ,?_assertEqual(true, kz_term:is_empty("0"))
    ,?_assertEqual(true, kz_term:is_empty(<<"0">>))
    ,?_assertEqual(true, kz_term:is_empty([]))
    ,?_assertEqual(true, kz_term:is_empty(""))
    ,?_assertEqual(true, kz_term:is_empty(<<>>))
    ,?_assertEqual(true, kz_term:is_empty(undefined))
    ,?_assertEqual(true, kz_term:is_empty("undefined"))
    ,?_assertEqual(true, kz_term:is_empty(<<"undefined">>))
    ,?_assertEqual(true, kz_term:is_empty(kz_json:new()))
    ,?_assertEqual(true, kz_term:is_empty(null))
    ,?_assertEqual(true, kz_term:is_empty("NULL"))
    ,?_assertEqual(true, kz_term:is_empty(<<"NULL">>))
    ,?_assertEqual(true, kz_term:is_empty(false))
    ,?_assertEqual(true, kz_term:is_empty("false"))
    ,?_assertEqual(true, kz_term:is_empty(<<"false">>))
    ,?_assertEqual(false, kz_term:is_empty(1))
    ,?_assertEqual(false, kz_term:is_empty(1.0))
    ,?_assertEqual(false, kz_term:is_empty(true))
    ,?_assertEqual(false, kz_term:is_empty(bla))
    ,?_assertEqual(false, kz_term:is_empty([42]))
    ,?_assertEqual(false, kz_term:is_empty(kz_json:from_list([{<<"a">>, 42}])))
    ].

is_not_empty_test_() ->
    [?_assertEqual(false, kz_term:is_not_empty(0))
    ,?_assertEqual(false, kz_term:is_not_empty(0.0))
    ,?_assertEqual(false, kz_term:is_not_empty("0"))
    ,?_assertEqual(false, kz_term:is_not_empty(<<"0">>))
    ,?_assertEqual(false, kz_term:is_not_empty([]))
    ,?_assertEqual(false, kz_term:is_not_empty(""))
    ,?_assertEqual(false, kz_term:is_not_empty(<<>>))
    ,?_assertEqual(false, kz_term:is_not_empty(undefined))
    ,?_assertEqual(false, kz_term:is_not_empty("undefined"))
    ,?_assertEqual(false, kz_term:is_not_empty(<<"undefined">>))
    ,?_assertEqual(false, kz_term:is_not_empty(kz_json:new()))
    ,?_assertEqual(false, kz_term:is_not_empty(null))
    ,?_assertEqual(false, kz_term:is_not_empty("NULL"))
    ,?_assertEqual(false, kz_term:is_not_empty(<<"NULL">>))
    ,?_assertEqual(false, kz_term:is_not_empty(false))
    ,?_assertEqual(false, kz_term:is_not_empty("false"))
    ,?_assertEqual(false, kz_term:is_not_empty(<<"false">>))
    ,?_assertEqual(true, kz_term:is_not_empty(1))
    ,?_assertEqual(true, kz_term:is_not_empty(1.0))
    ,?_assertEqual(true, kz_term:is_not_empty(true))
    ,?_assertEqual(true, kz_term:is_not_empty(bla))
    ,?_assertEqual(true, kz_term:is_not_empty([42]))
    ,?_assertEqual(true, kz_term:is_not_empty(kz_json:from_list([{<<"a">>, 42}])))
    ].

is_proplist_test_() ->
    [?_assertEqual(true, kz_term:is_proplist([]))
    ,?_assertEqual(true, kz_term:is_proplist([{a,2}]))
    ,?_assertEqual(true, kz_term:is_proplist([{a,2}, b]))
    ,?_assertEqual(false, kz_term:is_proplist([{a,2}, b, <<"c">>]))
    ,?_assertEqual(false, kz_term:is_proplist(<<>>))
    ,?_assertEqual(false, kz_term:is_proplist(#{}))
    ,?_assertEqual(true, kz_term:is_proplist([{<<"a">>,2}]))
    ,?_assertEqual(false, kz_term:is_proplist(kz_json:from_list([{<<"a">>,2}])))
    ].

id_test() ->
    ?assertEqual(bla, kz_term:identity(bla)).

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

truncate_binary_test_() ->
    [?_assertEqual(<<>>, kz_binary:truncate(<<>>, 0))
    ,?_assertEqual(<<>>, kz_binary:truncate(<<>>, 42))
    ,?_assertEqual(<<"b">>, kz_binary:truncate(<<"bla">>, 1))
    ,?_assertEqual(<<"bl">>, kz_binary:truncate_right(<<"bla">>, 2))
    ,?_assertEqual(<<"la">>, kz_binary:truncate_left(<<"bla">>, 2))
    ,?_assertEqual(<<"a">>, kz_binary:truncate_left(<<"bla">>, 1))
    ,?_assertEqual(<<"bla">>, kz_binary:truncate(<<"bla">>, 4))
    ,?_assertEqual(<<"bla">>, kz_binary:truncate_left(<<"bla">>, 4))
    ,?_assertEqual(<<"bla">>, kz_binary:truncate_right(<<"bla">>, 4))
    ].

account_formats_test_() ->
    AccountId = <<A:2/binary, B:2/binary, Rest:28/binary>> = kz_binary:rand_hex(16),
    AccountDbUn = list_to_binary(["account/", A, "/", B, "/", Rest]),
    AccountDbEn = list_to_binary(["account%2F", A, "%2F", B, "%2F", Rest]),

    {Y, M, _} = erlang:date(),
    TS = kz_time:current_tstamp(),
    Now = os:timestamp(),
    Year = kz_term:to_binary(Y),
    Month = kz_time:pad_month(M),

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
