%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Utils shared by the various proxy servers
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_proxy_util).

-export([stream_body/5
        ,get_shout_header/2

        ,resp_headers/1, resp_headers/4
        ,fold_resp_headers/2
        ]).

-include("kazoo_media.hrl").

-type shout_header() :: {non_neg_integer(), kz_term:ne_binary()}.
-export_type([shout_header/0]).

-spec stream_body(cowboy_req:req(), pos_integer(), binary(), shout_header() | 'undefined', boolean()) -> 'ok'.
stream_body(Req, ChunkSize, Bin, Header, ToPad) ->
    lager:debug("let's stream the binary"),
    send_chunks(Req, ChunkSize, Bin, Header, ToPad).

-spec send_chunks(cowboy_req:req(), pos_integer(), binary(), shout_header() | 'undefined', boolean()) -> 'ok'.
send_chunks(Req, _ChunkSize, <<>>, _ShoutHeader, _ToPad) ->
    lager:debug("nothing left to send"),
    cowboy_req:stream_body(<<>>, 'fin', Req);
send_chunks(Req, ChunkSize, Bin, Header, ToPad) ->
    try
        <<Send:ChunkSize/binary, Rest/binary>> = Bin,
        'ok' = write_chunk(Req, Send, Header),
        send_chunks(Req, ChunkSize, Rest, bump(Header), ToPad)
    catch
        'error':{'badmatch',_} ->
            lager:debug("sending last chunk"),
            write_data(Req, ChunkSize, Bin, Header, ToPad)
    end.

-spec bump(shout_header() | 'undefined') -> shout_header() | 'undefined'.
bump('undefined') -> 'undefined';
bump({K, H}) -> {K+1, H}.

%% When we know we have the whole chunk, just send it + header
-spec write_chunk(cowboy_req:req(), binary(), shout_header() | 'undefined') -> 'ok'.
write_chunk(Req, Bin, 'undefined') ->
    cowboy_req:stream_body(Bin, 'nofin', Req);
write_chunk(Req, Bin, Header) ->
    cowboy_req:stream_body([Bin, the_header(Header)], 'nofin', Req).

%% when we have less than the chunk size to send, possibly pad it
-spec write_data(cowboy_req:req(), pos_integer(), binary(), shout_header() | 'undefined', boolean()) -> 'ok'.
write_data(Req, ChunkSize, Bin, Header, 'true') ->
    Size = byte_size(Bin),
    H = the_header(Header),

    Padding = binary:copy(<<0>>, ChunkSize-Size-byte_size(H)),

    cowboy_req:stream_body([Bin, H, Padding], 'fin', Req);
write_data(Req, _ChunkSize, Bin, 'undefined', _) ->
    cowboy_req:stream_body(Bin, 'fin', Req);
write_data(Req, _ChunkSide, Bin, Header, 'false') ->
    cowboy_req:stream_body([Bin, the_header(Header)], 'fin', Req).

-spec the_header(shout_header() | 'undefined') -> binary().
the_header('undefined') -> <<>>;
the_header({K, H}) ->
    case K rem 5 of
        0 -> H;
        _ -> <<0>>
    end.

-spec get_shout_header(kz_term:ne_binary(), kz_term:ne_binary()) -> shout_header().
get_shout_header(MediaName, Url) ->
    Bin = list_to_binary(["StreamTitle='",MediaName
                         ,"';StreamUrl='",Url,"';"
                         ]),
    Nblocks = ((byte_size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - byte_size(Bin),
    Extra = lists:duplicate(NPad, 0),
    {0, list_to_binary([Nblocks, Bin, Extra])}.

-spec resp_headers(kz_term:ne_binary()) -> cowboy:http_headers().
resp_headers(ContentType) ->
    #{<<"content-type">> => ContentType
     ,<<"server">> => list_to_binary([?APP_NAME, "/", ?APP_VERSION])
     }.

-spec resp_headers(pos_integer(), kz_term:ne_binary(), binary(), binary()) -> cowboy:http_headers().
resp_headers(ChunkSize, ContentType, MediaName, Url) ->
    #{<<"content-type">> => ContentType
     ,<<"icy-br">> => <<"8">>
     ,<<"icy-genre">> => <<"Kazoo Media">>
     ,<<"icy-metaint">> => kz_term:to_binary(ChunkSize)
     ,<<"icy-name">> => MediaName
     ,<<"icy-notice1">> => <<"MediaMgr">>
     ,<<"icy-pub">> => <<"1">>
     ,<<"icy-url">> => Url
     ,<<"server">> => list_to_binary([?APP_NAME, "/", ?APP_VERSION])
     }.

-spec fold_resp_headers(cowboy_req:req(), cowboy:http_headers()) -> cowboy_req:req().
fold_resp_headers(Req, #{}=Headers) ->
    maps:fold(fun(K, V, ReqAcc) ->
                      cowboy_req:set_resp_header(K, V, ReqAcc)
              end
             ,Req
             ,Headers
             ).
