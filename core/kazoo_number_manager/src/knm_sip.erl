%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Parse and manipulate SIP URIs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_sip).

-export([parse/1
        ,encode/1
        ]).

%% Accessors
-export([scheme/1, set_scheme/2
        ,user/1, set_user/2
        ,host/1, set_host/2
        ,port/1, set_port/2
        ]).

-ifdef(TEST).
-export([parse_until/2]).
-endif.

-include("knm.hrl").

-record(sip_uri, {scheme = 'sip' :: 'sip' | 'sips'
                 ,user :: ne_binary()
                 ,host :: ne_binary()
                 ,port = 5060 :: pos_integer()
                 }).
-type sip_uri() :: #sip_uri{}.
-export_type([sip_uri/0]).

-spec parse(ne_binary()) -> sip_uri().
parse(Bin) ->
    parse_scheme(Bin, #sip_uri{}).

parse_scheme(<<"<", Bin/binary>>, Uri) ->
    parse_scheme(Bin, Uri);
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
        {H, <<>>} -> Uri#sip_uri{host=kz_util:strip_right_binary(H, $>), port=5060};
        {H, <<">">>} -> Uri#sip_uri{host=H, port=5060};
        {H, P} -> Uri#sip_uri{host=H, port=parse_port(P)}
    end.

-spec parse_port(binary()) -> pos_integer().
parse_port(P) ->
    case parse_until(<<">">>, P) of
        {<<>>, _} -> throw({'invalid_uri_port', P});
        {Port, _} -> kz_util:to_integer(Port)
    end.

-spec parse_until(ne_binary(), ne_binary()) -> {binary(), binary()}.
parse_until(C, Bin) ->
    case binary:split(Bin, C) of
        [B] -> {B, <<>>};
        [Pre, Post] -> {Pre, Post}
    end.

scheme(#sip_uri{scheme=S}) -> S.
set_scheme(#sip_uri{}=Sip, S) ->
    Sip#sip_uri{scheme=S}.

user(#sip_uri{user=U}) -> U.
set_user(#sip_uri{}=Sip, U) ->
    Sip#sip_uri{user=U}.

host(#sip_uri{host=H}) -> H.
set_host(#sip_uri{}=Sip, H) ->
    Sip#sip_uri{host=H}.

port(#sip_uri{port=P}) -> P.
set_port(#sip_uri{}=Sip, P) ->
    Sip#sip_uri{port=P}.
