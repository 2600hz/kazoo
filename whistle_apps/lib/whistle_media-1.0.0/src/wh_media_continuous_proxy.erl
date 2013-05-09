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
         ,terminate/3
         ,handle/2
        ]).

-include("whistle_media.hrl").

init({_Transport, _Proto}, Req0, _Opts) ->
    put('callid', wh_util:rand_hex_binary(16)),
    case cowboy_req:path_info(Req0) of
        {[<<"tts">>, Id], Req1} ->
            init_from_tts(Id, Req1);
        {[?MEDIA_DB = Db, Id, Attachment], Req1} ->
            init_from_doc(Db, Id, Attachment, Req1);
        {[Db, Id, Attachment], Req1} ->
            AccountDb = wh_util:format_account_id(Db, 'encoded'),
            init_from_doc(AccountDb, Id, Attachment, Req1)
    end.

init_from_tts(Id, Req) ->
    lager:debug("fetching tts/~s", [Id]),
    case wh_media_cache_sup:find_tts_server(Id) of
        {'ok', Pid} ->
            {'ok', Req, wh_media_file_cache:single(Pid)};
        {'error', _} ->
            lager:debug("missing tts server: 404"),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    end.

init_from_doc(Db, Id, Attachment, Req) ->
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    case wh_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {'ok', Pid} ->
            {'ok', Req, wh_media_file_cache:single(Pid)};
        {'error', _} ->
            lager:debug("missing file server: 404"),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    end.

handle(Req0, {Meta, Bin}) ->
    Size = byte_size(Bin),
    ChunkSize = case Size of S when S > ?CHUNKSIZE -> ?CHUNKSIZE; S -> S end,

    ContentType = wh_json:get_value(<<"content_type">>, Meta),
    MediaName = wh_json:get_value(<<"media_name">>, Meta, <<>>),
    Url = wh_json:get_value(<<"url">>, Meta, <<>>),

    Req2 = case ContentType of
               CT when CT =:= <<"audio/mpeg">> orelse CT =:= <<"audio/mp3">> ->
                   Req1 = set_resp_headers(Req0, ChunkSize, ContentType, MediaName, Url),
                   cowboy_req:set_resp_body_fun(Size
                                                ,fun(Socket, Transport) ->
                                                         wh_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin
                                                                                    ,wh_media_proxy_util:get_shout_header(MediaName, Url)
                                                                                    , 'true')
                                                 end
                                                ,Req1);
               CT ->
                   Req1 = set_resp_headers(Req0, CT),

                   cowboy_req:set_resp_body_fun(Size
                                                ,fun(Socket, Transport) ->
                                                         wh_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin, 'undefined', 'false')
                                                 end
                                                ,Req1)
           end,

    {'ok', Req3} = cowboy_req:chunked_reply(200, Req2),

    {'ok', Req3, 'ok'}.

terminate(_Reason, Req, _State) ->
    lager:debug("terminating contiuous proxy req"),
    Req.

set_resp_headers(Req, ContentType) ->
    lists:foldl(fun({K,V}, Req0Acc) ->
                        cowboy_req:set_resp_header(K, V, Req0Acc)
                end, Req, [{<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
                           ,{<<"Content-Type">>, ContentType}
                          ]
               ).

-spec set_resp_headers(cowboy_req:req(), pos_integer(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', cowboy_req:req()}.
set_resp_headers(Req, ChunkSize, ContentType, MediaName, Url) ->
    lists:foldl(fun({K,V}, Req0Acc) ->
                        cowboy_req:set_resp_header(K, V, Req0Acc)
                end, Req, [{<<"Server">>, list_to_binary([?APP_NAME, "/", ?APP_VERSION])}
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
