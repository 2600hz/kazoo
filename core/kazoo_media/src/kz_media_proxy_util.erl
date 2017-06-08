%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Utils shared by the various proxy servers
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_proxy_util).

-export([stream/6
        ,get_shout_header/2

        ,set_resp_headers/2, set_resp_headers/5
        ]).

-include("kazoo_media.hrl").

-type shout_header() :: {0, ne_binary()}.
-export_type([shout_header/0]).

-spec stream(inet:socket(), atom(), pos_integer(), binary(), shout_header() | 'undefined', boolean()) -> any().
stream(Socket, Transport, ChunkSize, Bin, Header, ToPad) ->
    lager:debug("let's stream the binary"),
    send_chunks(Socket, Transport, ChunkSize, Bin, Header, ToPad).

send_chunks(_S, _T, _, <<>>, _, _) ->
    lager:debug("nothing to send");
send_chunks(Socket, Transport, ChunkSize, Bin, Header, ToPad) ->
    try
        <<Send:ChunkSize/binary, Rest/binary>> = Bin,
        write_chunk(Socket, Transport, Send, Header),
        send_chunks(Socket, Transport, ChunkSize, Rest, bump(Header), ToPad)
    catch
        'error':{'badmatch',_} ->
            lager:debug("sending last chunk"),
            write_data(Socket, Transport, ChunkSize, Bin, Header, ToPad)
    end.

bump('undefined') -> 'undefined';
bump({K, H}) -> {K+1, H}.

%% When we know we have the whole chunk, just send it + header
write_chunk(Socket, Transport, Bin, 'undefined') ->
    Transport:send(Socket, Bin);
write_chunk(Socket, Transport, Bin, Header) ->
    Transport:send(Socket, [Bin, the_header(Header)]).

%% when we have less than the chunk size to send, possibly pad it
write_data(Socket, Transport, ChunkSize, Bin, Header, 'true') ->
    Size = byte_size(Bin),
    H = the_header(Header),

    Padding = binary:copy(<<0>>, ChunkSize-Size-byte_size(H)),

    Transport:send(Socket, [Bin, H, Padding]);
write_data(Socket, Transport, _, Bin, 'undefined', _) ->
    Transport:send(Socket, Bin);
write_data(Socket, Transport, _, Bin, Header, 'false') ->
    Transport:send(Socket, [Bin, the_header(Header)]).

the_header('undefined') -> <<>>;
the_header({K, H}) ->
    case K rem 5 of
        0 -> H;
        _ -> <<0>>
    end.

-spec get_shout_header(ne_binary(), ne_binary()) -> shout_header().
get_shout_header(MediaName, Url) ->
    Bin = list_to_binary(["StreamTitle='",MediaName
                         ,"';StreamUrl='",Url,"';"
                         ]),
    Nblocks = ((byte_size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - byte_size(Bin),
    Extra = lists:duplicate(NPad, 0),
    {0, list_to_binary([Nblocks, Bin, Extra])}.



-spec set_resp_headers(cowboy_req:req(), ne_binary()) -> cowboy_req:req().
set_resp_headers(Req, ContentType) ->
    fold_resp_headers(Req
                     ,[{<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
                      ,{<<"Content-Type">>, ContentType}
                      ]
                     ).

-spec set_resp_headers(cowboy_req:req(), pos_integer(), ne_binary(), binary(), binary()) ->
                              cowboy_req:req().
set_resp_headers(Req, ChunkSize, ContentType, MediaName, Url) ->
    fold_resp_headers(Req
                     ,[{<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
                      ,{<<"Content-Type">>, ContentType}
                      ,{<<"icy-notice1">>, <<"MediaMgr">>}
                      ,{<<"icy-name">>, MediaName}
                      ,{<<"icy-genre">>, <<"Kazoo Media">>}
                      ,{<<"icy-url">>, Url}
                      ,{<<"content-type">>, ContentType}
                      ,{<<"icy-pub">>, <<"1">>}
                      ,{<<"icy-metaint">>, kz_term:to_binary(ChunkSize)}
                      ,{<<"icy-br">>, <<"8">>}
                      ]
                     ).

-spec fold_resp_headers(cowboy_req:req(), kz_proplist()) -> cowboy_req:req().
fold_resp_headers(Req, Headers) when is_list(Headers) ->
    lists:foldl(fun({K, V}, ReqAcc) ->
                        cowboy_req:set_resp_header(K, V, ReqAcc)
                end
               ,Req
               ,Headers
               ).
