%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_sip_test).

-include_lib("eunit/include/eunit.hrl").

parse_until_test_() ->
    {Before, After} = knm_sip:parse_until(<<":">>, <<"foo:bar">>),
    [?_assertEqual(<<"foo">>, Before)
    ,?_assertEqual(<<"bar">>, After)
    ].

parse_full_test_() ->
    Uri = knm_sip:parse(<<"sip:username@host.com:2600">>),

    [?_assertEqual('sip', knm_sip:scheme(Uri))
    ,?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"host.com">>, knm_sip:host(Uri))
    ,?_assertEqual(2600, knm_sip:port(Uri))
    ].

parse_uh_test_() ->
    Uri = knm_sip:parse(<<"sip:username@host.com">>),

    [?_assertEqual('sip', knm_sip:scheme(Uri))
    ,?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"host.com">>, knm_sip:host(Uri))
    ,?_assertEqual(5060, knm_sip:port(Uri))
    ].

parse_suh_test_() ->
    Uri = knm_sip:parse(<<"sips:username@host.com">>),

    [?_assertEqual('sips', knm_sip:scheme(Uri))
    ,?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"host.com">>, knm_sip:host(Uri))
    ,?_assertEqual(5060, knm_sip:port(Uri))
    ].

parse_noscheme_test_() ->
    Uri = knm_sip:parse(<<"username@host.com">>),

    [?_assertEqual('sip', knm_sip:scheme(Uri))
    ,?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"host.com">>, knm_sip:host(Uri))
    ,?_assertEqual(5060, knm_sip:port(Uri))
    ].

parse_noscheme_port_test_() ->
    Uri = knm_sip:parse(<<"username@host.com:2600">>),

    [?_assertEqual('sip', knm_sip:scheme(Uri))
    ,?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"host.com">>, knm_sip:host(Uri))
    ,?_assertEqual(2600, knm_sip:port(Uri))
    ].

encode_test() ->
    Uri = knm_sip:parse(<<"username@host.com:2600">>),
    ?assertEqual(<<"sip:username@host.com:2600">>, knm_sip:encode(Uri)).

encode_full_test() ->
    U = <<"sip:username@host.com:2600">>,
    Uri = knm_sip:parse(U),
    ?assertEqual(U, knm_sip:encode(Uri)).

encode_5060_test() ->
    U = <<"sips:username@host.com">>,
    Uri = knm_sip:parse(U),
    ?assertEqual(U, knm_sip:encode(Uri)).

encode_full_5060_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = knm_sip:parse(U),
    ?assertEqual(<<"sips:username@host.com">>, knm_sip:encode(Uri)).

parse_angles_test_() ->
    U = <<"<sips:username@host.com:5060>">>,
    Uri = knm_sip:parse(U),

    [?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"sips:username@host.com">>, knm_sip:encode(Uri))
    ].

parse_angles_2_test_() ->
    U = <<"<sip:username@host.com>">>,
    Uri = knm_sip:parse(U),

    [?_assertEqual(<<"username">>, knm_sip:user(Uri))
    ,?_assertEqual(<<"sip:username@host.com">>, knm_sip:encode(Uri))
    ].
