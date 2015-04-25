%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_sip_test).

-include_lib("eunit/include/eunit.hrl").

parse_until_test() ->
    {Before, After} = wnm_sip:parse_until(<<":">>, <<"foo:bar">>),
    ?assertEqual(<<"foo">>, Before),
    ?assertEqual(<<"bar">>, After).

parse_full_test() ->
    Uri = wnm_sip:parse(<<"sip:username@host.com:2600">>),

    ?assertEqual('sip', wnm_sip:scheme(Uri)),
    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"host.com">>, wnm_sip:host(Uri)),
    ?assertEqual(2600, wnm_sip:port(Uri)).

parse_uh_test() ->
    Uri = wnm_sip:parse(<<"sip:username@host.com">>),

    ?assertEqual('sip', wnm_sip:scheme(Uri)),
    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"host.com">>, wnm_sip:host(Uri)),
    ?assertEqual(5060, wnm_sip:port(Uri)).

parse_suh_test() ->
    Uri = wnm_sip:parse(<<"sips:username@host.com">>),

    ?assertEqual('sips', wnm_sip:scheme(Uri)),
    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"host.com">>, wnm_sip:host(Uri)),
    ?assertEqual(5060, wnm_sip:port(Uri)).

parse_noscheme_test() ->
    Uri = wnm_sip:parse(<<"username@host.com">>),

    ?assertEqual('sip', wnm_sip:scheme(Uri)),
    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"host.com">>, wnm_sip:host(Uri)),
    ?assertEqual(5060, wnm_sip:port(Uri)).

parse_noscheme_port_test() ->
    Uri = wnm_sip:parse(<<"username@host.com:2600">>),

    ?assertEqual('sip', wnm_sip:scheme(Uri)),
    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"host.com">>, wnm_sip:host(Uri)),
    ?assertEqual(2600, wnm_sip:port(Uri)).

encode_test() ->
    Uri = wnm_sip:parse(<<"username@host.com:2600">>),
    ?assertEqual(<<"sip:username@host.com:2600">>, wnm_sip:encode(Uri)).

encode_full_test() ->
    U = <<"sip:username@host.com:2600">>,
    Uri = wnm_sip:parse(U),
    ?assertEqual(U, wnm_sip:encode(Uri)).

encode_5060_test() ->
    U = <<"sips:username@host.com">>,
    Uri = wnm_sip:parse(U),
    ?assertEqual(U, wnm_sip:encode(Uri)).

encode_full_5060_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = wnm_sip:parse(U),
    ?assertEqual(<<"sips:username@host.com">>, wnm_sip:encode(Uri)).

parse_angles_test() ->
    U = <<"<sips:username@host.com:5060>">>,
    Uri = wnm_sip:parse(U),

    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"sips:username@host.com">>, wnm_sip:encode(Uri)).

parse_angles_2_test() ->
    U = <<"<sip:username@host.com>">>,
    Uri = wnm_sip:parse(U),

    ?assertEqual(<<"username">>, wnm_sip:user(Uri)),
    ?assertEqual(<<"sip:username@host.com">>, wnm_sip:encode(Uri)).
