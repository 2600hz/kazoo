%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_term_test).

-include_lib("kazoo/include/kz_types.hrl").

-ifdef(PROPER).
- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

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
                F =:= kz_term:from_hex_binary(kz_term:to_hex_binary(F))
            end).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, kz_term:pad_binary(<<"12345">>, 10, <<"0">>)).

join_binary_test() ->
    ?assertEqual(<<"foo">>, kz_term:join_binary([<<"foo">>], <<", ">>)),
    ?assertEqual(<<"foo, bar">>, kz_term:join_binary([<<"foo">>, <<"bar">>], <<", ">>)),
    ?assertEqual(<<"foo, bar, baz">>, kz_term:join_binary([<<"foo">>, <<"bar">>, <<"baz">>], <<", ">>)).

ucfirst_binary_test() ->
    ?assertEqual(<<"Foo">>, kz_term:ucfirst_binary(<<"foo">>)),
    ?assertEqual(<<"Foo">>, kz_term:ucfirst_binary(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, kz_term:ucfirst_binary(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, kz_term:ucfirst_binary(<<"1oo">>)),
    ?assertEqual(<<"100">>, kz_term:ucfirst_binary(<<"100">>)),
    ?assertEqual(<<"1FF">>, kz_term:ucfirst_binary(<<"1FF">>)).

lcfirst_binary_test() ->
    ?assertEqual(<<"foo">>, kz_term:lcfirst_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, kz_term:lcfirst_binary(<<"Foo">>)),
    ?assertEqual(<<"fOO">>, kz_term:lcfirst_binary(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, kz_term:lcfirst_binary(<<"1oo">>)),
    ?assertEqual(<<"100">>, kz_term:lcfirst_binary(<<"100">>)),
    ?assertEqual(<<"1FF">>, kz_term:lcfirst_binary(<<"1FF">>)).

to_lower_binary_test() ->
    ?assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"Foo">>)),
    ?assertEqual(<<"foo">>, kz_term:to_lower_binary(<<"FoO">>)),
    ?assertEqual(<<"f00">>, kz_term:to_lower_binary(<<"f00">>)),
    ?assertEqual(<<"f00">>, kz_term:to_lower_binary(<<"F00">>)).

to_upper_binary_test() ->
    ?assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"foo">>)),
    ?assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, kz_term:to_upper_binary(<<"FoO">>)),
    ?assertEqual(<<"F00">>, kz_term:to_upper_binary(<<"f00">>)),
    ?assertEqual(<<"F00">>, kz_term:to_upper_binary(<<"F00">>)).

to_lower_string_test() ->
    ?assertEqual("foo", kz_term:to_lower_string("foo")),
    ?assertEqual("foo", kz_term:to_lower_string("Foo")),
    ?assertEqual("foo", kz_term:to_lower_string("FoO")),
    ?assertEqual("f00", kz_term:to_lower_string("f00")),
    ?assertEqual("f00", kz_term:to_lower_string("F00")).

to_upper_string_test() ->
    ?assertEqual("FOO", kz_term:to_upper_string("foo")),
    ?assertEqual("FOO", kz_term:to_upper_string("Foo")),
    ?assertEqual("FOO", kz_term:to_upper_string("FoO")),
    ?assertEqual("F00", kz_term:to_upper_string("f00")),
    ?assertEqual("F00", kz_term:to_upper_string("F00")).

strip_binary_test() ->
    ?assertEqual(<<"foo">>, kz_term:strip_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, kz_term:strip_binary(<<"foo ">>)),
    ?assertEqual(<<"foo">>, kz_term:strip_binary(<<" foo ">>)),
    ?assertEqual(<<"foo">>, kz_term:strip_binary(<<"  foo  ">>)),
    ?assertEqual(<<"foo">>, kz_term:strip_binary(<<"     foo">>)),

    ?assertEqual(<<"foo">>, kz_term:strip_left_binary(<<"foo">>, $\s)),
    ?assertEqual(<<"foo">>, kz_term:strip_left_binary(<<" foo">>, $\s)),
    ?assertEqual(<<"foo ">>, kz_term:strip_left_binary(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo ">>, kz_term:strip_left_binary(<<"foo ">>, $\s)),

    ?assertEqual(<<"foo">>, kz_term:strip_right_binary(<<"foo">>, $\s)),
    ?assertEqual(<<" foo">>, kz_term:strip_right_binary(<<" foo">>, $\s)),
    ?assertEqual(<<" foo">>, kz_term:strip_right_binary(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo">>, kz_term:strip_right_binary(<<"foo ">>, $\s)).

to_boolean_test() ->
    All = [<<"true">>, "true", 'true', <<"false">>, "false", 'false'],
    NotAll = [0, 123, 1.23, "123", "abc", 'abc', <<"abc">>, <<"123">>, {'what', 'is', 'this', 'doing', 'here'}],
    ?assertEqual('true', lists:all(fun(X) ->
                                           try kz_term:to_boolean(X) of
                                               _ -> 'true'
                                           catch _:_ -> 'false'
                                           end
                                   end
                                   ,All
                                  )
                ),
    ?assertEqual('true', lists:all(fun(X) ->
                                           try kz_term:to_boolean(X) of
                                               _ -> 'false'
                                           catch _:_ -> 'true'
                                           end
                                   end
                                   ,NotAll
                                  )
                ).

strip_test() ->
    ?assertEqual(kz_term:strip_binary(<<"...Hello.....">>, $.), <<"Hello">>).

suffix_binary_test() ->
    ?assertEqual('true', kz_term:suffix_binary(<<"34">>, <<"1234">>)),
    ?assertEqual('false', kz_term:suffix_binary(<<"34">>, <<"12345">>)),
    ?assertEqual('false', kz_term:suffix_binary(<<"1234">>, <<"1">>)).
