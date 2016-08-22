%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kzsip_uris_test).

-include_lib("eunit/include/eunit.hrl").

parse_until_test_() ->
    {Before, After} = kzsip_uris:parse_until(<<":">>, <<"foo:bar">>),
    [?_assertEqual(<<"foo">>, Before)
    ,?_assertEqual(<<"bar">>, After)
    ].

parse_full_test_() ->
    Uri = kzsip_uris:parse(<<"sip:username@host.com:2600">>),
    [?_assertEqual('sip', kzsip_uris:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uris:host(Uri))
    ,?_assertEqual(2600, kzsip_uris:port(Uri))
    ].

parse_uh_test_() ->
    Uri = kzsip_uris:parse(<<"sip:username@host.com">>),
    [?_assertEqual('sip', kzsip_uris:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uris:host(Uri))
    ,?_assertEqual(5060, kzsip_uris:port(Uri))
    ].

parse_suh_test_() ->
    Uri = kzsip_uris:parse(<<"sips:username@host.com">>),
    [?_assertEqual('sips', kzsip_uris:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uris:host(Uri))
    ,?_assertEqual(5060, kzsip_uris:port(Uri))
    ].

parse_noscheme_test_() ->
    Uri = kzsip_uris:parse(<<"username@host.com">>),
    [?_assertEqual('sip', kzsip_uris:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uris:host(Uri))
    ,?_assertEqual(5060, kzsip_uris:port(Uri))
    ].

parse_noscheme_port_test_() ->
    Uri = kzsip_uris:parse(<<"username@host.com:2600">>),
    [?_assertEqual('sip', kzsip_uris:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uris:host(Uri))
    ,?_assertEqual(2600, kzsip_uris:port(Uri))
    ].

encode_test() ->
    Uri = kzsip_uris:parse(<<"username@host.com:2600">>),
    ?assertEqual(<<"sip:username@host.com:2600">>, kzsip_uris:encode(Uri)).

encode_full_test() ->
    U = <<"sip:username@host.com:2600">>,
    Uri = kzsip_uris:parse(U),
    ?assertEqual(U, kzsip_uris:encode(Uri)).

encode_5060_test() ->
    U = <<"sips:username@host.com">>,
    Uri = kzsip_uris:parse(U),
    ?assertEqual(U, kzsip_uris:encode(Uri)).

encode_full_5060_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = kzsip_uris:parse(U),
    ?assertEqual(<<"sips:username@host.com">>, kzsip_uris:encode(Uri)).

parse_angles_test_() ->
    U = <<"<sips:username@host.com:5060>">>,
    Uri = kzsip_uris:parse(U),
    [?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"sips:username@host.com">>, kzsip_uris:encode(Uri))
    ].

parse_angles_2_test_() ->
    U = <<"<sip:username@host.com>">>,
    Uri = kzsip_uris:parse(U),
    [?_assertEqual(<<"username">>, kzsip_uris:user(Uri))
    ,?_assertEqual(<<"sip:username@host.com">>, kzsip_uris:encode(Uri))
    ].
