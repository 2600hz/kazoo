%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzsip_uri_tests).

-include_lib("eunit/include/eunit.hrl").

parse_until_test_() ->
    {Before, After} = kzsip_uri:parse_until(<<":">>, <<"foo:bar">>),
    [?_assertEqual(<<"foo">>, Before)
    ,?_assertEqual(<<"bar">>, After)
    ].

parse_full_test_() ->
    Uri = kzsip_uri:parse(<<"sip:username@host.com:2600">>),
    [?_assertEqual('sip', kzsip_uri:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uri:host(Uri))
    ,?_assertEqual(2600, kzsip_uri:port(Uri))
    ].

parse_uh_test_() ->
    Uri = kzsip_uri:parse(<<"sip:username@host.com">>),
    [?_assertEqual('sip', kzsip_uri:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uri:host(Uri))
    ,?_assertEqual(5060, kzsip_uri:port(Uri))
    ].

parse_suh_test_() ->
    Uri = kzsip_uri:parse(<<"sips:username@host.com">>),
    [?_assertEqual('sips', kzsip_uri:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uri:host(Uri))
    ,?_assertEqual(5060, kzsip_uri:port(Uri))
    ].

parse_noscheme_test_() ->
    Uri = kzsip_uri:parse(<<"username@host.com">>),
    [?_assertEqual('sip', kzsip_uri:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uri:host(Uri))
    ,?_assertEqual(5060, kzsip_uri:port(Uri))
    ].

parse_noscheme_port_test_() ->
    Uri = kzsip_uri:parse(<<"username@host.com:2600">>),
    [?_assertEqual('sip', kzsip_uri:scheme(Uri))
    ,?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"host.com">>, kzsip_uri:host(Uri))
    ,?_assertEqual(2600, kzsip_uri:port(Uri))
    ].

encode_test() ->
    Uri = kzsip_uri:parse(<<"username@host.com:2600">>),
    ?assertEqual(<<"sip:username@host.com:2600">>, kzsip_uri:encode(Uri)).

encode_full_test() ->
    U = <<"sip:username@host.com:2600">>,
    Uri = kzsip_uri:parse(U),
    ?assertEqual(U, kzsip_uri:encode(Uri)).

encode_5060_test() ->
    U = <<"sips:username@host.com">>,
    Uri = kzsip_uri:parse(U),
    ?assertEqual(U, kzsip_uri:encode(Uri)).

encode_full_5060_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = kzsip_uri:parse(U),
    ?assertEqual(<<"sips:username@host.com">>, kzsip_uri:encode(Uri)).

parse_angles_test_() ->
    U = <<"<sips:username@host.com:5060>">>,
    Uri = kzsip_uri:parse(U),
    [?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"sips:username@host.com">>, kzsip_uri:encode(Uri))
    ].

parse_angles_2_test_() ->
    U = <<"<sip:username@host.com>">>,
    Uri = kzsip_uri:parse(U),
    [?_assertEqual(<<"username">>, kzsip_uri:user(Uri))
    ,?_assertEqual(<<"sip:username@host.com">>, kzsip_uri:encode(Uri))
    ].
