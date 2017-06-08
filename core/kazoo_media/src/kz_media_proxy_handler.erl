-module(kz_media_proxy_handler).

-export([init/3
        ,terminate/3
        ,handle/2
        ]).

-include("kazoo_media.hrl").

-define(STATE(Metadata, MediaBinary)
       ,{Metadata, MediaBinary}
       ).
-type state() :: ?STATE(kz_json:object(), binary()).
-type stream_type() :: 'single' | 'continuous'.

-spec init(any(), cowboy_req:req(), any()) -> {'ok', cowboy_req:req(), state()} |
                                              {'shutdown', cowboy_req:req(), 'ok'}.
init({_Transport, _Proto}, Req0, [StreamType]) ->
    kz_util:put_callid(kz_binary:rand_hex(16)),
    lager:info("starting ~s media proxy"),
    case cowboy_req:path_info(Req0) of
        {[<<"tts">>, Id], Req1} ->
            init_from_tts(maybe_strip_extension(Id), Req1, StreamType);
        {[Url, _Name], Req1} ->
            init_from_doc(Url, Req1, StreamType)
    end.

init_from_tts(Id, Req, StreamType) ->
    lager:debug("fetching tts/~s", [Id]),
    try kz_media_cache_sup:find_tts_server(Id) of
        {'ok', Pid} ->
            {Meta, Bin} = media_data(Pid, StreamType),
            {'ok', Req, ?STATE(Meta, Bin)};
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

init_from_doc(Url, Req, StreamType) ->
    {Db, Id, Attachment, _Options} = binary_to_term(base64:decode(Url)),
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    try kz_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {'ok', Pid} ->
            {Meta, Bin} = media_data(Pid, StreamType),
            {'ok', Req, ?STATE(Meta, Bin)};
        {'error', _} ->
            lager:debug("starting file server ~s/~s/~s", [Db, Id, Attachment]),
            case kz_media_cache_sup:start_file_server(Db, Id, Attachment) of
                {'ok', Pid} ->
                    {Meta, Bin} = media_data(Pid, StreamType),
                    {'ok', Req, ?STATE(Meta, Bin)};
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
handle(Req0, ?STATE(Meta, Bin)) ->
    ContentType = kz_json:get_ne_binary_value(<<"content_type">>, Meta),

    {'ok', Req1} = cowboy_req:reply(200, set_resp_body_fun(Req0, Meta, Bin, ContentType)),
    lager:debug("sending reply"),

    {'ok', Req1, 'ok'}.

-spec set_resp_body_fun(cowboy_req:req(), kz_json:object(), binary(), ne_binary()) ->
                               cowboy_req:req().
set_resp_body_fun(Req0, Meta, Bin, ContentType)
  when ContentType =:= <<"audio/mpeg">>
       orelse ContentType =:= <<"audio/mp3">> ->
    Size = byte_size(Bin),
    ChunkSize = min(Size, ?CHUNKSIZE),
    MediaName = kz_json:get_binary_value(<<"media_name">>, Meta, <<>>),
    Url = kz_json:get_binary_value(<<"url">>, Meta, <<>>),

    lager:debug("media: ~s content-type: ~s size: ~b", [MediaName, ContentType, Size]),

    ShoutHeader = kz_media_proxy_util:get_shout_header(MediaName, Url),
    RespFun = response_fun(ChunkSize, Bin, ShoutHeader, 'true'),
    Req1 = kz_media_proxy_util:set_resp_headers(Req0, ChunkSize, ContentType, MediaName, Url),
    cowboy_req:set_resp_body_fun(Size, RespFun, Req1);
set_resp_body_fun(Req0, _Meta, Bin, ContentType) ->
    Size = byte_size(Bin),
    ChunkSize = min(Size, ?CHUNKSIZE),

    lager:debug("media: ~s content-type: ~s size: ~b"
               ,[kz_json:get_binary_value(<<"media_name">>, _Meta, <<>>), ContentType, Size]
               ),

    Req1 = kz_media_proxy_util:set_resp_headers(Req0, ContentType),
    cowboy_req:set_resp_body_fun(Size, response_fun(ChunkSize, Bin), Req1).

-type response_fun() :: fun((any(), module()) -> 'ok').
-spec response_fun(pos_integer(), binary()) ->
                          response_fun().
-spec response_fun(pos_integer(), binary(), 'undefined' | kz_media_proxy_util:shout_header(), boolean()) ->
                          response_fun().
response_fun(ChunkSize, Bin) ->
    response_fun(ChunkSize, Bin, 'undefined', 'false').

response_fun(ChunkSize, Bin, ShoutHeader, Bool) ->
    fun(Socket, Transport) ->
            lager:debug("ready to stream file using transport ~p to socket ~p", [Transport, Socket]),
            kz_media_proxy_util:stream(Socket, Transport, ChunkSize, Bin
                                      ,ShoutHeader
                                      ,Bool
                                      )
    end.


-spec terminate(any(), Req, state()) -> Req.
terminate(_Reason, Req, _State) ->
    lager:debug("terminating proxy req: ~p", [_Reason]),
    Req.

-spec maybe_strip_extension(file:filename_all()) -> file:filename_all().
maybe_strip_extension(Id) -> filename:rootname(Id).

-spec media_data(pid(), stream_type()) -> {kz_json:object(), binary()}.
media_data(Pid, Function) ->
    kz_media_file_cache:Function(Pid).
