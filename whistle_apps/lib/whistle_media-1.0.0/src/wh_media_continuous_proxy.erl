%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles single requests for media binaries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_continuous_proxy).

-export([init/3
         ,terminate/2
         ,handle/2
         ,stream/5
        ]).

-include("whistle_media.hrl").

init({_Transport, _Proto}, Req0, _Opts) ->
    put(callid, wh_util:rand_hex_binary(16)),
    case cowboy_http_req:path_info(Req0) of
        {[<<"tts">>, Id], Req1} ->
            init_from_tts(Id, Req1);
        {[?MEDIA_DB = Db, Id, Attachment], Req1} ->
            init_from_doc(Db, Id, Attachment, Req1);
        {[Db, Id, Attachment], Req1} ->
            AccountDb = wh_util:format_account_id(Db, encoded),
            init_from_doc(AccountDb, Id, Attachment, Req1)
    end.

init_from_tts(Id, Req) ->
    lager:debug("fetching tts/~s", [Id]),
    case wh_media_cache_sup:find_tts_server(Id) of
        {ok, Pid} ->
            {ok, Req, wh_media_file_cache:single(Pid)};
        {error, _} ->
            lager:debug("missing tts server"),
            {ok, Req1} = cowboy_http_req:reply(404, Req),
            {shutdown, Req1, ok}
    end.

init_from_doc(Db, Id, Attachment, Req) ->
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    case wh_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {ok, Pid} ->
            {ok, Req, wh_media_file_cache:single(Pid)};
        {error, _} ->
            lager:debug("missing file server"),
            {ok, Req1} = cowboy_http_req:reply(404, Req),
            {shutdown, Req1, ok}
    end.

handle(Req0, {Meta, Bin}) ->
    Size = byte_size(Bin),
    ChunkSize = case Size of S when S > ?CHUNKSIZE -> ?CHUNKSIZE; S -> S end,

    ContentType = wh_json:get_value(<<"content_type">>, Meta),
    MediaName = wh_json:get_value(<<"media_name">>, Meta, <<>>),
    Url = wh_json:get_value(<<"url">>, Meta, <<>>),

    {ok, Req2} = case ContentType of
                     CT when CT =:= <<"audio/mpeg">> orelse CT =:= <<"audio/mp3">> ->
                         {ok, Req1} = set_resp_headers(Req0, ChunkSize, ContentType, MediaName, Url),

                         cowboy_http_req:set_resp_body_fun(Size
                                                           ,fun() ->
                                                                    stream(Req1, ChunkSize, Bin, get_shout_header(MediaName, Url), true)
                                                            end
                                                           ,Req1);
                     CT ->
                         {ok, Req1} = set_resp_headers(Req0, CT),

                         cowboy_http_req:set_resp_body_fun(Size
                                                           ,fun() ->
                                                                    stream(Req1, ChunkSize, Bin, undefined, false)
                                                            end
                                                           ,Req1)
                 end,

    {ok, Req3} = cowboy_http_req:reply(200, Req2),

    {ok, Req3, ok}.

terminate(_Req, _State) ->
    lager:debug("terminating").

stream(Req, ChunkSize, Bin, Header, ToPad) ->
    {ok, Transport, Socket} = cowboy_http_req:transport(Req),

    send_chunks(Transport, Socket, ChunkSize, Bin, Header, ToPad).

send_chunks(_, _, _, <<>>, _, _) ->
    lager:debug("nothing to send");
send_chunks(Transport, Socket, ChunkSize, Bin, Header, ToPad) ->
    try
        <<Send:ChunkSize/binary, Rest/binary>> = Bin,
        write_chunk(Transport, Socket, Send, Header),
        send_chunks(Transport, Socket, ChunkSize, Rest, bump(Header), ToPad)
    catch
        error:{badmatch,_} ->
            write_data(Transport, Socket, ChunkSize, Bin, Header, ToPad)
    end.

%% When we know we have the whole chunk, just send it + header
write_chunk(Transport, Socket, Bin, undefined) ->
    Transport:send(Socket, Bin);
write_chunk(Transport, Socket, Bin, Header) ->
    Transport:send(Socket, [Bin, the_header(Header)]).

%% when we have less than the chunk size to send, possibly pad it
write_data(Transport, Socket, ChunkSize, Bin, Header, true) ->
    Size = byte_size(Bin),
    H = the_header(Header),

    Padding = binary:copy(<<0>>, ChunkSize-Size-byte_size(H)),

    Transport:send(Socket, [Bin, H, Padding]);
write_data(Transport, Socket, _, Bin, undefined, _) ->
    Transport:send(Socket, Bin);
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

set_resp_headers(Req, ContentType) ->
    lists:foldl(fun({K,V}, {ok, Req0Acc}) ->
                        cowboy_http_req:set_resp_header(K, V, Req0Acc)
                end, {ok, Req}, [
                                 {<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
                                 ,{<<"Content-Type">>, ContentType}
                                ]
               ).

-spec set_resp_headers/5 :: (#http_req{}, pos_integer(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', #http_req{}}.
set_resp_headers(Req, ChunkSize, ContentType, MediaName, Url) ->
    lists:foldl(fun({K,V}, {ok, Req0Acc}) ->
                        cowboy_http_req:set_resp_header(K, V, Req0Acc)
                end, {ok, Req}, [{<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
                                 ,{<<"Content-Type">>, ContentType}
                                 ,{<<"icy-notice1">>, <<"MediaMgr">>}
                                 ,{<<"icy-name">>, MediaName}
                                 ,{<<"icy-genre">>, <<"Whistle Media">>}
                                 ,{<<"icy-url">>, Url}
                                 ,{<<"content-type">>, ContentType}
                                 ,{<<"icy-pub">>, <<"1">>}
                                 ,{<<"icy-metaint">>, wh_util:to_binary(ChunkSize)}
                                 ,{<<"icy-br">>, <<"8">>}
                                ]
               ).

-spec get_shout_header/2 :: (ne_binary(), ne_binary()) -> {0, ne_binary()}.
get_shout_header(MediaName, Url) ->
    Bin = list_to_binary(["StreamTitle='",MediaName
                          ,"';StreamUrl='",Url,"';"
                         ]),
    Nblocks = ((byte_size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - byte_size(Bin),
    Extra = lists:duplicate(NPad, 0),
    {0, list_to_binary([Nblocks, Bin, Extra])}.
