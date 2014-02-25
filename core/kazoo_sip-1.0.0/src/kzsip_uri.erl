%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Parse and manipulate SIP URIs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzsip_uri).

-export([parse/1
         ,encode/1

         ,to_json/1
         ,from_json/1
        ]).

%% Accessors
-export([scheme/1
         ,user/1
         ,host/1
         ,port/1
        ]).

-include("kazoo_sip.hrl").

-record(sip_uri, {scheme = 'sip' :: 'sip' | 'sips'
                  ,user :: ne_binary()
                  ,host :: ne_binary()
                  ,port = 5060 :: pos_integer()
                 }).
-type sip_uri() :: #sip_uri{}.
-export_type([sip_uri/0]).

-spec to_json(sip_uri()) -> wh_json:object().
to_json(#sip_uri{scheme=Scheme
                 ,user=User
                 ,host=Host
                 ,port=Port
                }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"scheme">>, wh_util:to_binary(Scheme)}
         ,{<<"user">>, User}
         ,{<<"host">>, Host}
         ,{<<"port">>, Port}
        ])).

-spec from_json(wh_json:object()) -> sip_uri().
from_json(JObj) ->
    #sip_uri{scheme=wh_json:get_atom_value(<<"scheme">>, JObj)
             ,user=wh_json:get_value(<<"user">>, JObj)
             ,host=wh_json:get_value(<<"host">>, JObj)
             ,port=wh_json:get_integer_value(<<"port">>, JObj)
            }.

-spec parse(ne_binary()) -> sip_uri().
parse(Bin) ->
    parse_scheme(Bin, #sip_uri{}).
parse_scheme(<<"sip:", Bin/binary>>, Uri) ->
    parse_user(Bin, Uri#sip_uri{scheme='sip'});
parse_scheme(<<"sips:", Bin/binary>>, Uri) ->
    parse_user(Bin, Uri#sip_uri{scheme='sips'});
parse_scheme(Bin, Uri) ->
    parse_user(Bin, Uri#sip_uri{scheme='sip'}).

-spec encode(sip_uri()) -> ne_binary().
encode(#sip_uri{scheme=S, user=U, host=H, port=5060}) ->
    list_to_binary([atom_to_list(S), ":", U, "@", H]);
encode(#sip_uri{scheme=S, user=U, host=H, port=P}) ->
    list_to_binary([atom_to_list(S), ":", U, "@", H, ":", integer_to_list(P)]).

-spec parse_user(binary(), sip_uri()) -> sip_uri().
parse_user(Bin, Uri) ->
    case parse_until(<<"@">>, Bin) of
        {_, <<>>} -> throw({'invalid_uri_user', Bin});
        {User, Rest} -> parse_host(Rest, Uri#sip_uri{user=User})
    end.

-spec parse_host(binary(), sip_uri()) -> sip_uri().
parse_host(Bin, Uri) ->
    case parse_until(<<":">>, Bin) of
        {<<>>, _} -> throw({'invalid_uri_host', Bin});
        {H, <<>>} -> Uri#sip_uri{host=H, port=5060};
        {H, P} -> Uri#sip_uri{host=H, port=wh_util:to_integer(P)}
    end.

-spec parse_until(ne_binary(), ne_binary()) -> {binary(), binary()}.
parse_until(C, Bin) ->
    case binary:split(Bin, C) of
        [B] -> {B, <<>>};
        [Pre, Post] -> {Pre, Post}
    end.

scheme(#sip_uri{scheme=S}) -> S.
user(#sip_uri{user=U}) -> U.
host(#sip_uri{host=H}) -> H.
port(#sip_uri{port=P}) -> P.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_until_test() ->
    {Before, After} = parse_until(<<":">>, <<"foo:bar">>),
    ?assertEqual(<<"foo">>, Before),
    ?assertEqual(<<"bar">>, After).

parse_full_test() ->
    Uri = parse(<<"sip:username@host.com:2600">>),

    ?assertEqual('sip', scheme(Uri)),
    ?assertEqual(<<"username">>, user(Uri)),
    ?assertEqual(<<"host.com">>, host(Uri)),
    ?assertEqual(2600, port(Uri)).

parse_uh_test() ->
    Uri = parse(<<"sip:username@host.com">>),

    ?assertEqual('sip', scheme(Uri)),
    ?assertEqual(<<"username">>, user(Uri)),
    ?assertEqual(<<"host.com">>, host(Uri)),
    ?assertEqual(5060, port(Uri)).

parse_suh_test() ->
    Uri = parse(<<"sips:username@host.com">>),

    ?assertEqual('sips', scheme(Uri)),
    ?assertEqual(<<"username">>, user(Uri)),
    ?assertEqual(<<"host.com">>, host(Uri)),
    ?assertEqual(5060, port(Uri)).

parse_noscheme_test() ->
    Uri = parse(<<"username@host.com">>),

    ?assertEqual('sip', scheme(Uri)),
    ?assertEqual(<<"username">>, user(Uri)),
    ?assertEqual(<<"host.com">>, host(Uri)),
    ?assertEqual(5060, port(Uri)).

parse_noscheme_port_test() ->
    Uri = parse(<<"username@host.com:2600">>),

    ?assertEqual('sip', scheme(Uri)),
    ?assertEqual(<<"username">>, user(Uri)),
    ?assertEqual(<<"host.com">>, host(Uri)),
    ?assertEqual(2600, port(Uri)).

encode_test() ->
    Uri = parse(<<"username@host.com:2600">>),
    ?assertEqual(<<"sip:username@host.com:2600">>, encode(Uri)).

encode_full_test() ->
    U = <<"sip:username@host.com:2600">>,
    Uri = parse(U),
    ?assertEqual(U, encode(Uri)).

encode_5060_test() ->
    U = <<"sips:username@host.com">>,
    Uri = parse(U),
    ?assertEqual(U, encode(Uri)).

encode_full_5060_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = parse(U),
    ?assertEqual(<<"sips:username@host.com">>, encode(Uri)).

to_json_test() ->
    U = <<"sips:username@host.com:5060">>,
    Uri = parse(U),
    JObj = to_json(Uri),
    ?assertEqual(Uri, from_json(JObj)).

-endif.
