%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Handles single requests for media binaries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_single_proxy).

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
            init_from_tts(maybe_strip_extension(Id), Req1);
        {[Url, _Name], Req1} ->
            init_from_doc(Url, Req1)
    end.

init_from_tts(Id, Req) ->
    lager:debug("fetching tts/~s", [Id]),
    try kz_media_cache_sup:find_tts_server(Id) of
        {'ok', Pid} ->
            {'ok', Req, kz_media_file_cache:single(Pid)};
        {'error', _E} ->
            lager:debug("missing tts server for ~s: ~p", [Id, _E]),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    catch
        _E:_R ->
            lager:debug("exception thrown: ~s: ~p", [_E, _R]),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    end.

init_from_doc(Url, Req) ->
    {Db, Id, Attachment, _Options} = binary_to_term(base64:decode(Url)),
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    try kz_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {'ok', Pid} ->
            {'ok', Req, kz_media_file_cache:single(Pid)};
        {'error', _} ->
            lager:debug("starting file server ~s/~s/~s", [Db, Id, Attachment]),
            case kz_media_cache_sup:start_file_server(Db, Id, Attachment) of
                {'ok', Pid} ->
                    {'ok', Req, kz_media_file_cache:single(Pid)};
                {'error', Error} ->
                    lager:debug("start server failed ~s/~s/~s: ~p", [Db, Id, Attachment, Error]),
                    {'ok', Req1} = cowboy_req:reply(404, Req),
                    {'shutdown', Req1, 'ok'}
            end
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            lager:debug("exception thrown: ~s: ~p", [_E, _R]),
            {'ok', Req1} = cowboy_req:reply(404, Req),
            {'shutdown', Req1, 'ok'}
    end.

-spec handle(cowboy_req:req(), state()) -> {'ok', cowboy_req:req(), 'ok'}.
handle(Req0, {Meta, Bin}) ->
    Size = byte_size(Bin),
    lager:debug("found binary to return: ~b bytes", [Size]),
    ChunkSize = min(Size, ?CHUNKSIZE),

    ContentType = kz_json:get_value(<<"content_type">>, Meta),
    MediaName = kz_json:get_value(<<"media_name">>, Meta, <<>>),
    Url = kz_json:get_value(<<"url">>, Meta, <<>>),

    Req2 = case ContentType of
               CT when CT =:= <<"audio/mpeg">>
                       orelse CT =:= <<"audio/mp3">> ->
                   Req1 = set_resp_headers(Req0, ChunkSize, ContentType, MediaName, Url),
                   cowboy_req:set_resp_body_fun(Size
                                               ,fun(Socket, Transport) ->
                                                        kz_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin
                                                                                  ,kz_media_proxy_util:get_shout_header(MediaName, Url)
                                                                                  ,'true')
                                                end
                                               ,Req1);
               CT ->
                   Req1 = set_resp_headers(Req0, CT),
                   cowboy_req:set_resp_body_fun(Size
                                               ,fun(Socket, Transport) ->
                                                        kz_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin, 'undefined', 'false')
                                                end
                                               ,Req1)
           end,

    {'ok', Req3} = cowboy_req:reply(200, Req2),
    {'ok', Req3, 'ok'}.

-spec terminate(any(), cowboy_req:req(), state()) -> cowboy_req:req().
terminate(_Reason, Req, _State) ->
    lager:debug("terminating single proxy req"),
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

maybe_strip_extension(Id) -> filename:rootname(Id).
