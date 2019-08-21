%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_binary_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

-ifdef(PROPER).
proper_test_() ->
    {"Runs kz_json PropEr tests"
    ,{'timeout'
     ,10000
     ,[{atom_to_list(F)
       ,fun () ->
                ?assert(proper:quickcheck(?MODULE:F(), [{'to_file', 'user'}
                                                       ,{'numtests', 500}
                                                       ]))
        end
       }
       || {F, 0} <- ?MODULE:module_info('exports'),
          F > 'prop_',
          F < 'prop`'
      ]
     }
    }.

prop_bin_reverse() ->
    ?FORALL(Bin
           ,binary()
           ,?WHENFAIL(?debugFmt("failed to reverse binary ~p~n", [Bin])
                     ,kz_binary:reverse(kz_binary:reverse(Bin)) =:= Bin
                     )
           ).

prop_to_from_hex() ->
    ?FORALL(Bin
           ,binary(),
            begin
                Bin =:= kz_binary:from_hex(kz_term:to_hex_binary(Bin))
            end).

-endif.

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, kz_binary:pad(<<"12345">>, 10, <<"0">>)).

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
    [?_assertEqual(<<"626c61">>, kz_binary:hexencode(bla))
    ,?_assertEqual(<<"626c61">>, kz_binary:hexencode("bla"))
    ,?_assertEqual(<<"626c61">>, kz_binary:hexencode(<<"bla">>))
    ,?_assertEqual(30, byte_size(kz_binary:rand_hex("15")))
    ,?_assertEqual(32, byte_size(kz_binary:rand_hex(<<"16">>)))
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

strip_test() ->
    ?assertEqual(kz_binary:strip(<<"...Hello.....">>, $.), <<"Hello">>).

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

binary_reverse_test_() ->
    [?_assertEqual(<<>>, kz_binary:reverse(<<>>))
    ,?_assertEqual(<<"B a">>, kz_binary:reverse(<<"a B">>))
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

-ifdef(PERF).
-define(REPEAT, 100000).

%% from https://stackoverflow.com/questions/20830201/better-way-to-reverse-binary#20832625
%% Bin size 4:
%% kz_binary_test:rev in 0.171672s
%% kz_binary_test:rev2 in 0.023417s
%% kz_binary_test:reverse in 0.031123s

%% Bin size 16:
%% kz_binary_test:rev in 0.635037s
%% kz_binary_test:rev2 in 0.029335s
%% kz_binary_test:reverse in 0.047751s

%% Bin size: 128
%% kz_binary_test:rev in 3.322764s
%% kz_binary_test:rev2 in 0.059977s
%% kz_binary_test:reverse in 0.338276s

%% Bin size: 1024
%% kz_binary_test:rev in 31.880859s
%% kz_binary_test:rev2 in 0.363329s
%% kz_binary_test:reverse in 1.530542s

-define(BIN, <<"75d46db34465e620c214bdeaf214eac1c87833851166fbbd59aeb0a35aa1bd901abac59eb53fe4c076261537632aefba8ab16889550f84bb51c6934a97ba813c">>).

-spec rev1(binary()) -> binary().
rev1(Bin) -> rev1(Bin, <<>>).
rev1(<<>>, Acc) -> Acc;
rev1(<<H:1/binary, Rest/binary>>, Acc) ->
    rev1(Rest, <<H/binary, Acc/binary>>).

horse_rev() ->
    horse:repeat(?REPEAT, rev1(?BIN)).

horse_rev2() ->
    horse:repeat(?REPEAT, kz_binary:reverse(?BIN)).


old_reverse(Bin) ->
    kz_term:to_binary(lists:reverse(kz_term:to_list(Bin))).

horse_reverse() ->
    horse:repeat(?REPEAT, old_reverse(?BIN)).

-endif.
