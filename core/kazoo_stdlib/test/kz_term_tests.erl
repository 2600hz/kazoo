%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_term_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

-ifdef(PROPER).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 20000,
      [?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

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

-endif.


to_x_test_() ->
    TS = kz_time:now_s(),
    [?_assertError(badarg, kz_term:to_integer(1.0, strict))
    ,?_assertEqual(1, kz_term:to_integer(1.0, notstrict))
    ,?_assertEqual(42, kz_term:to_integer(<<"42">>, strict))
    ,?_assertEqual(42, kz_term:to_integer("42.0", notstrict))
    ,?_assertError(badarg, kz_term:to_float(1, strict))
    ,?_assertEqual(1.0, kz_term:to_float(1, notstrict))
    ,?_assertEqual(42.0, kz_term:to_float(<<"42.0">>, strict))
    ,?_assertEqual(42.0, kz_term:to_float(<<"42">>, notstrict))
    ,?_assertError(badarg, kz_term:to_float("42", strict))
    ,?_assertEqual(42.0, kz_term:to_float("42", notstrict))
    ,?_assertEqual(list_to_binary(pid_to_list(self())), kz_term:to_binary(self()))
    ,?_assertEqual(to_atom, kz_term:to_atom(to_atom))
    ,?_assertEqual(to_atom, kz_term:to_atom("to_atom"))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>, false))
    ,?_assertEqual(to_atom, kz_term:to_atom(<<"to_atom">>, ["to_atom"]))
    ,?_assertEqual('42', kz_term:to_atom(42))
    ,?_assertEqual(a, kz_term:to_atom(a, true))
    ,?_assertEqual(a, kz_term:to_atom("a", true))
    ,?_assertEqual(a, kz_term:to_atom(<<"a">>, true))
    ,?_assertEqual('1', kz_term:to_atom(1, true))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(TS))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(kz_term:to_list(TS)))
    ,?_assertEqual(element(1,calendar:gregorian_seconds_to_datetime(TS)), kz_term:to_date(kz_term:to_binary(TS)))
    ,?_assertEqual("63611209435.0", kz_term:to_list(63611209435.0))
    ,?_assertEqual(<<"63611209435.0">>, kz_term:to_binary(63611209435.0))
    ,?_assertEqual(<<"63657597518.515564">>, kz_term:to_binary(63657597518.515564))
    ,?_assertEqual(1, kz_term:to_number(1))
    ,?_assertEqual(42, kz_term:to_number(<<"42">>))
    ,?_assertEqual(4.2, kz_term:to_number(<<"4.2">>))
    ,?_assertEqual(42, kz_term:to_number("42"))
    ,?_assertEqual(4.2, kz_term:to_number("4.2"))
    ,?_assertEqual(undefined, kz_term:to_api_binary(undefined))
    ,?_assertEqual(<<"bla">>, kz_term:to_api_binary(bla))
    ,?_assertEqual(true, kz_term:is_api_ne_binary(undefined))
    ,?_assertEqual(false, kz_term:is_api_ne_binary("undefined"))
    ,?_assertEqual(true, kz_term:is_api_ne_binary(<<"oh boy">>))
    ].

shuffle_list_test_() ->
    [?_assertEqual([], kz_term:shuffle_list([]))
    ,?_assertEqual([42], kz_term:shuffle_list([42]))
    ,?_assertMatch([_,_,_], kz_term:shuffle_list(lists:seq(1, 3)))
    ].

to_hex_test_() ->
    [?_assertEqual("626c61", kz_term:to_hex(bla))
    ,?_assertEqual("626c61", kz_term:to_hex("bla"))
    ,?_assertEqual("626c61", kz_term:to_hex(<<"bla">>))
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

float_bounds_test_() ->
    [?_assertEqual(1, kz_term:floor(1.0))
    ,?_assertEqual(1, kz_term:floor(1.2))
    ,?_assertEqual(1, kz_term:floor(1.5))
    ,?_assertEqual(1, kz_term:floor(1.7))
    ,?_assertEqual(-1, kz_term:floor(-1))
    ,?_assertEqual(-2, kz_term:floor(-1.5))
    ,?_assertEqual(1, kz_term:ceiling(1.0))
    ,?_assertEqual(2, kz_term:ceiling(1.2))
    ,?_assertEqual(2, kz_term:ceiling(1.5))
    ,?_assertEqual(2, kz_term:ceiling(1.7))
    ,?_assertEqual(-1, kz_term:ceiling(-1))
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
    ,?_assertEqual(false, kz_term:is_empty(false))
    ,?_assertEqual(false, kz_term:is_empty("false"))
    ,?_assertEqual(false, kz_term:is_empty(<<"false">>))
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
    ,?_assertEqual(true, kz_term:is_not_empty(false))
    ,?_assertEqual(true, kz_term:is_not_empty("false"))
    ,?_assertEqual(true, kz_term:is_not_empty(<<"false">>))
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

to_pid_test_() ->
    {'setup'
    ,fun() ->
             Pid = spawn(fun() -> receive X -> X end end),
             'true' = register('foobarbaz', Pid),
             Pid
     end
    ,fun(Pid) -> Pid ! 'stop' end
    ,fun(Pid) ->
             [?_assertEqual('undefined', kz_term:to_pid('undefined'))
             ,?_assertEqual(Pid, kz_term:to_pid(Pid))
             ,?_assertEqual(Pid, kz_term:to_pid('foobarbaz'))
             ,?_assertEqual('undefined', kz_term:to_pid('someothername'))
             ,?_assertEqual(Pid, kz_term:to_pid(pid_to_list(Pid)))
             ,?_assertEqual(Pid, kz_term:to_pid(list_to_binary(pid_to_list(Pid))))
             ]
     end
    }.

iolist_join_test_() ->
    [?_assertEqual([], kz_term:iolist_join($,, []))
    ,?_assertEqual([$a,<<" || ">>,$b,<<" || ">>,$c], kz_term:iolist_join(<<" || ">>, [$a,$b,$c]))
    ].

-ifdef(PERF).
-define(REPEAT, 100000).

-define(BIN_INT, <<"1234567890">>).

horse_to_integer() ->
    horse:repeat(?REPEAT, kz_term:to_integer(?BIN_INT)).

to_integer_convoluted(Bin) ->
    list_to_integer(binary_to_list(Bin)).

horse_to_integer_convoluted() ->
    horse:repeat(?REPEAT, to_integer_convoluted(?BIN_INT)).

to_integer_straight(Bin) ->
    binary_to_integer(Bin).

horse_to_integer_straight() ->
    horse:repeat(?REPEAT, to_integer_straight(?BIN_INT)).

-endif.
