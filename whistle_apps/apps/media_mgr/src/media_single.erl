%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles single requests for media binaries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_single).

-export([init/3, terminate/2, handle/2, stream/5]).

-include("media.hrl").

init({_Transport, _Proto}, Req0, _Opts) ->
    put(callid, ?LOG_SYSTEM_ID),

    {[Id, Doc, Attachment], Req1} = cowboy_http_req:path_info(Req0),

    lager:debug("fetch ~s/~s/~s", [Id, Doc, Attachment]),

    {ok, Pid} = media_files_sup:find_file_server(Id, Doc, Attachment),
    lager:debug("found media file server: ~p", [Pid]),
    {ok, Req1, media_file:single(Pid)}.

handle(Req0, {Meta, Bin}) ->
    Size = byte_size(Bin),
    ChunkSize = case Size of S when S > ?CHUNKSIZE -> ?CHUNKSIZE; S -> S end,

    lager:debug("size: ~b chunk: ~b", [Size, ChunkSize]),

    ContentType = wh_json:get_value(<<"content_type">>, Meta),
    MediaName = wh_json:get_value(<<"media_name">>, Meta, <<>>),
    Url = wh_json:get_value(<<"url">>, Meta, <<>>),

    Pad = (ContentType =:= <<"audio/mpeg">>) orelse
        (ContentType =:= <<"audio/mp3">>),

    {ok, Req1} = cowboy_http_req:set_resp_body_fun(Size
                                                   ,fun() ->
                                                            stream(Req0, ChunkSize, Bin, get_shout_header(MediaName, Url), Pad)
                                                    end
                                                   ,Req0),

    {ok, Req2} = set_resp_headers(Req1, ChunkSize, ContentType, MediaName, Url),
    {ok, Req3} = cowboy_http_req:reply(200, Req2),
    {ok, Req3, ok}.

terminate(_Req, _State) ->
    lager:debug("terminating").

stream(Req, ChunkSize, Bin, Header, ToPad) ->
    {ok, Transport, Socket} = cowboy_http_req:transport(Req),

    lager:debug("streaming in ~b bytes (use padding: ~p)", [ChunkSize, ToPad]),
    send_chunks(Transport, Socket, ChunkSize, Bin, {0, Header}, ToPad).

send_chunks(_, _, _, <<>>, _, _) ->
    lager:debug("nothing to send");
send_chunks(Transport, Socket, ChunkSize, Bin, Header, ToPad) ->
    try erlang:split_binary(Bin, ChunkSize) of
        {Send, Rest} ->
            lager:debug("sending another chunk (~b left, hdr: ~b)", [byte_size(Rest), element(1, Header)]),
            write_chunk(Transport, Socket, Send, Header),
            send_chunks(Transport, Socket, ChunkSize, Rest, bump(Header), ToPad)
    catch
        _:_ ->
            lager:debug("sending last bit (~b to send, hdr: ~b)", [byte_size(Bin), element(1, Header)]),
            write_data(Transport, Socket, ChunkSize, Bin, Header, ToPad)
    end.

%% When we know we have the whole chunk, just send it + header
write_chunk(Transport, Socket, Bin, Header) ->
    Transport:send(Socket, [Bin, the_header(Header)]).

%% when we have less than the chunk size to send, possibly pad it
write_data(Transport, Socket, ChunkSize, Bin, Header, true) ->
    Size = byte_size(Bin),
    H = the_header(Header),
    PaddingAmount = ChunkSize-Size-byte_size(H),

    lager:debug("padding ~b bytes", [PaddingAmount]),

    Padding = binary:copy(<<0>>, PaddingAmount),
    Transport:send(Socket, [Bin, H, Padding]);
write_data(Transport, Socket, _, Bin, Header, false) ->
    Transport:send(Socket, [Bin, the_header(Header)]).

bump(undefined) -> undefined;
bump({K, H}) -> {K+1, H}.

the_header(undefined) ->
    <<>>;
the_header({K, H}) ->
    case K rem 5 of
        0 -> H;
        _ -> <<0>>
    end.


-spec set_resp_headers/5 :: (#http_req{}, pos_integer(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', #http_req{}}.
set_resp_headers(Req, ChunkSize, ContentType, MediaName, Url) ->
    lists:foldl(fun({K,V}, {ok, Req0Acc}) ->
                        cowboy_http_req:set_resp_header(K, V, Req0Acc)
                end, {ok, Req}, [
                                 {<<"icy-notice1">>, <<"MediaMgr<BR>">>}
                                 ,{<<"icy-name">>, MediaName}
                                 ,{<<"icy-genre">>, <<"Whistle Media">>}
                                 ,{<<"icy-url">>, Url}
                                 ,{<<"content-type">>, ContentType}
                                 ,{<<"icy-pub">>, <<"1">>}
                                 ,{<<"icy-metaint">>, wh_util:to_binary(ChunkSize)}
                                 ,{<<"icy-br">>, <<"8">>}
                                ]
               ).

-spec get_shout_header/2 :: (ne_binary(), ne_binary()) -> ne_binary().
get_shout_header(MediaName, Url) ->
    Bin = list_to_binary(["StreamTitle='",MediaName
                          ,"';StreamUrl='",Url,"';"
                         ]),
    Nblocks = ((byte_size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - byte_size(Bin),
    Extra = lists:duplicate(NPad, 0),
    list_to_binary([Nblocks, Bin, Extra]).
