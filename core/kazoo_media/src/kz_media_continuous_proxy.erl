%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% Handles continuous requests for media binaries
%%% New requests enter the stream with the rest of requestors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_continuous_proxy).

-export([init/3
        ,terminate/3
        ,handle/2
        ]).

-include("kazoo_media.hrl").

-type state() :: {kz_json:object(), binary()}.

-spec init(any(), cowboy_req:req(), any()) -> {'ok', cowboy_req:req(), state()} |
                                              {'shutdown', cowboy_req:req(), 'ok'}.
init({_Transport, _Proto}, Req0, _Opts) ->
    kz_util:put_callid(kz_util:rand_hex_binary(16)),
    case cowboy_req:path_info(Req0) of
        {[<<"tts">>, Id], Req1} ->
            init_from_tts(Id, Req1);
        {[Url, _AName], Req1} ->
            init_from_doc(Url, Req1)
    end.

init_from_tts(Id, Req) ->
    lager:debug("fetching tts/~s", [Id]),
    case kz_media_cache_sup:find_tts_server(Id) of
        {'ok', Pid} ->
            {'ok', Req, kz_media_file_cache:single(Pid)};
        {'error', _} ->
            lager:debug("missing tts server: 404"),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    end.

init_from_doc(Url, Req) ->
    {Db, Id, Attachment, _Options} = binary_to_term(base64:decode(Url)),
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    case kz_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {'ok', Pid} ->
            {'ok', Req, kz_media_file_cache:continuous(Pid)};
        {'error', _} ->
            lager:debug("starting file server ~s/~s/~s", [Db, Id, Attachment]),
            case kz_media_cache_sup:start_file_server(Db, Id, Attachment) of
                {'ok', Pid} ->
                    {'ok', Req, kz_media_file_cache:continuous(Pid)};
                {'error', Error} ->
                    lager:debug("start server failed ~s/~s/~s: ~p", [Db, Id, Attachment, Error]),
                    {'ok', Req1} = cowboy_req:reply(404, Req),
                    {'shutdown', Req1, 'ok'}
            end
    end.

-spec handle(cowboy_req:req(), state()) -> {'ok', cowboy_req:req(), 'ok'}.
handle(Req0, {Meta, Bin}) ->
    Size = byte_size(Bin),
    ChunkSize = min(Size, ?CHUNKSIZE),

    ContentType = kz_json:get_value(<<"content_type">>, Meta),
    MediaName = kz_json:get_value(<<"media_name">>, Meta, <<>>),
    Url = kz_json:get_value(<<"url">>, Meta, <<>>),

    lager:debug("media: ~s content-type: ~s size: ~b", [MediaName, ContentType, Size]),

    Req2 = case ContentType of
               CT when CT =:= <<"audio/mpeg">>
                       orelse CT =:= <<"audio/mp3">> ->
                   Req1 = set_resp_headers(Req0, ChunkSize, ContentType, MediaName, Url),
                   cowboy_req:set_resp_body_fun(Size
                                               ,fun(Socket, Transport) ->
                                                        lager:debug("ready to stream file using transport ~p to socket ~p", [Transport, Socket]),
                                                        kz_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin
                                                                                  ,kz_media_proxy_util:get_shout_header(MediaName, Url)
                                                                                  , 'true')
                                                end
                                               ,Req1);
               CT ->
                   cowboy_req:set_resp_body_fun(Size
                                               ,fun(Socket, Transport) ->
                                                        lager:debug("ready to stream file using transport ~p to socket ~p", [Transport, Socket]),
                                                        kz_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin, 'undefined', 'false')
                                                end
                                               ,set_resp_headers(Req0, CT))
           end,

    {'ok', Req3} = cowboy_req:reply(200, Req2),
    lager:debug("sending reply"),

    {'ok', Req3, 'ok'}.

-spec terminate(any(), cowboy_req:req(), state()) -> cowboy_req:req().
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
                          ,{<<"icy-genre">>, <<"Kazoo Media">>}
                          ,{<<"icy-url">>, Url}
                          ,{<<"content-type">>, ContentType}
                          ,{<<"icy-pub">>, <<"1">>}
                          ,{<<"icy-metaint">>, kz_util:to_binary(ChunkSize)}
                          ,{<<"icy-br">>, <<"8">>}
                          ]
               ).
