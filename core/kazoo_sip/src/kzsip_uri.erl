%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Parse and manipulate SIP URIs
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzsip_uri).

-export([uris/1
        ,ruri/1
        ,uri/1
        ]).

-export([parse/1
        ,encode/1
        ]).

-export([scheme/1, set_scheme/2
        ,user/1, set_user/2
        ,host/1, set_host/2
        ,port/1, set_port/2
        ]).

-ifdef(TEST).
-export([parse_until/2]).
-endif.

-include("kazoo_sip.hrl").

-type uri() :: nklib:uri().
-type uris() :: nklib_parse_uri:uris().
-export_type([uri/0, uris/0]).

-type scheme() :: 'sip' | 'sips'.
-record(sip_uri, {scheme = 'sip' :: scheme()
                 ,user :: kz_term:api_ne_binary()
                 ,host :: kz_term:api_ne_binary()
                 ,port = 5060 :: pos_integer()
                 }).
-type sip_uri() :: #sip_uri{}.
-export_type([sip_uri/0]).

-spec uris(binary() | string() | uri()) -> [uri()] | 'error'.
uris(Uri) -> nklib_parse_uri:uris(Uri).

-spec ruri(uri()) -> binary().
ruri(#uri{scheme='undefined'}=Uri) -> nklib_unparse:uri3(Uri#uri{scheme='sip'});
ruri(Uri) -> nklib_unparse:uri3(Uri).

-spec uri(uri()) -> binary().
uri(#uri{scheme='undefined'}=Uri) -> nklib_unparse:uri(Uri#uri{scheme='sip'});
uri(Uri) -> nklib_unparse:uri(Uri).

-spec parse(kz_term:ne_binary()) -> sip_uri().
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

-spec encode(sip_uri()) -> kz_term:ne_binary().
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
        {H, <<>>} -> Uri#sip_uri{host=kz_binary:strip_right(H, $>), port=5060};
        {H, <<">">>} -> Uri#sip_uri{host=H, port=5060};
        {H, P} -> Uri#sip_uri{host=H, port=parse_port(P)}
    end.

-spec parse_port(binary()) -> pos_integer().
parse_port(P) ->
    case parse_until(<<">">>, P) of
        {<<>>, _} -> throw({'invalid_uri_port', P});
        {Port, _} -> kz_term:to_integer(Port)
    end.

-spec parse_until(kz_term:ne_binary(), kz_term:ne_binary()) -> {binary(), binary()}.
parse_until(C, Bin) ->
    case binary:split(Bin, C) of
        [B] -> {B, <<>>};
        [Pre, Post] -> {Pre, Post}
    end.

-spec scheme(sip_uri()) -> scheme().
scheme(#sip_uri{scheme=S}) -> S.

-spec set_scheme(sip_uri(), scheme()) -> sip_uri().
set_scheme(#sip_uri{}=Sip, S) ->
    Sip#sip_uri{scheme=S}.

-spec user(sip_uri()) -> kz_term:ne_binary().
user(#sip_uri{user=U}) -> U.

-spec set_user(sip_uri(), kz_term:ne_binary()) -> sip_uri().
set_user(#sip_uri{}=Sip, U) ->
    Sip#sip_uri{user=U}.

-spec host(sip_uri()) -> kz_term:ne_binary().
host(#sip_uri{host=H}) -> H.

-spec set_host(sip_uri(), kz_term:ne_binary()) -> sip_uri().
set_host(#sip_uri{}=Sip, H) ->
    Sip#sip_uri{host=H}.

-spec port(sip_uri()) -> pos_integer().
port(#sip_uri{port=P}) -> P.

-spec set_port(sip_uri(), pos_integer()) -> sip_uri().
set_port(#sip_uri{}=Sip, P) ->
    Sip#sip_uri{port=P}.
