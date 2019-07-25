%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_proxy_handler).
-behaviour(cowboy_handler).

-export([init/2
        ,terminate/3
        ]).

-include("media.hrl").

-define(STATE(Metadata, MediaBinary)
       ,{Metadata, MediaBinary}
       ).
-type state() :: ?STATE(kz_json:object(), binary()).
-type stream_type() :: 'single' | 'continuous'.

-spec init(cowboy_req:req(), any()) -> {'ok', cowboy_req:req(), state() | 'ok'}.
init(Req, [StreamType]) ->
    kz_util:put_callid(kz_binary:rand_hex(16)),
    lager:info("starting ~s media proxy", [StreamType]),
    case cowboy_req:path_info(Req) of
        [<<"tts">>, Id] ->
            init_from_tts(maybe_strip_extension(Id), Req, StreamType);
        [Url, _Name] ->
            init_from_doc(Url, Req, StreamType)
    end.

init_from_tts(Id, Req, StreamType) ->
    lager:debug("fetching tts/~s", [Id]),
    try kz_media_cache_sup:find_tts_server(Id) of
        {'ok', Pid} ->
            {Meta, Bin} = media_data(Pid, StreamType),
            handle(Req, ?STATE(Meta, Bin));
        {'error', _E} ->
            lager:debug("missing tts server for ~s: ~p", [Id, _E]),
            Req1 = cowboy_req:reply(404, Req),
            {'ok', Req1, 'ok'}
    catch
        _E:_R ->
            lager:debug("exception thrown: ~s: ~p", [_E, _R]),
            Req1 = cowboy_req:reply(404, Req),
            {'ok', Req1, 'ok'}
    end.

init_from_doc(Url, Req, StreamType) ->
    {Db, Id, Attachment, _Options} = binary_to_term(base64:decode(Url)),
    lager:debug("fetching ~s/~s/~s", [Db, Id, Attachment]),
    try kz_media_cache_sup:find_file_server(Db, Id, Attachment) of
        {'ok', Pid} ->
            {Meta, Bin} = media_data(Pid, StreamType),
            handle(Req, ?STATE(Meta, Bin));
        {'error', _} ->
            lager:debug("starting file server ~s/~s/~s", [Db, Id, Attachment]),
            case kz_media_cache_sup:start_file_server(Db, Id, Attachment) of
                {'ok', Pid} ->
                    {Meta, Bin} = media_data(Pid, StreamType),
                    handle(Req, ?STATE(Meta, Bin));
                {'error', Error} ->
                    lager:debug("start server failed ~s/~s/~s: ~p", [Db, Id, Attachment, Error]),
                    Req1 = cowboy_req:reply(404, Req),
                    {'ok', Req1, 'ok'}
            end
    catch
        ?STACKTRACE(_E, _R, ST)
        kz_util:log_stacktrace(ST),
        lager:debug("exception thrown: ~s: ~p", [_E, _R]),
        Req1 = cowboy_req:reply(404, Req),
        {'ok', Req1, 'ok'}
        end.

-spec handle(cowboy_req:req(), state()) -> {'ok', cowboy_req:req(), 'ok'}.
handle(Req0, ?STATE(Meta, Bin)) ->
    ContentType = kz_json:get_ne_binary_value(<<"content_type">>, Meta),

    Req1 = start_stream(Req0, Meta, Bin, ContentType),

    lager:debug("sent reply"),

    {'ok', Req1, 'ok'}.

start_stream(Req, Meta, Bin, ContentType)
  when ContentType =:= <<"audio/mpeg">>
       orelse ContentType =:= <<"audio/mp3">> ->
    Size = byte_size(Bin),
    ChunkSize = min(Size, ?CHUNKSIZE),
    MediaName = kz_json:get_binary_value(<<"media_name">>, Meta, <<>>),
    Url = kz_json:get_binary_value(<<"url">>, Meta, <<>>),

    lager:debug("media: ~s content-type: ~s size: ~b", [MediaName, ContentType, Size]),

    Req1 = cowboy_req:stream_reply(200
                                  ,kz_media_proxy_util:resp_headers(ChunkSize, ContentType, MediaName, Url)
                                  ,Req
                                  ),

    ShoutHeader = kz_media_proxy_util:get_shout_header(MediaName, Url),

    kz_media_proxy_util:stream_body(Req1, ChunkSize, Bin, ShoutHeader, 'true'),
    Req1;
start_stream(Req, Meta, Bin, ContentType) ->
    Size = byte_size(Bin),
    ChunkSize = min(Size, ?CHUNKSIZE),

    lager:debug("media: ~s content-type: ~s size: ~b"
               ,[kz_json:get_binary_value(<<"media_name">>, Meta, <<>>), ContentType, Size]
               ),
    Req1 = cowboy_req:stream_reply(200, kz_media_proxy_util:resp_headers(ContentType), Req),
    kz_media_proxy_util:stream_body(Req1, ChunkSize, Bin, 'undefined', 'false'),
    Req1.

-spec terminate(any(), cowboy_req:req(), state()) -> 'ok'.
terminate(_Reason, _Req, _State) ->
    lager:debug("terminating proxy req: ~p", [_Reason]).

-spec maybe_strip_extension(file:filename_all()) -> file:filename_all().
maybe_strip_extension(Id) -> filename:rootname(Id).

-spec media_data(pid(), stream_type()) -> {kz_json:object(), binary()}.
media_data(Pid, Function) ->
    kz_media_file_cache:Function(Pid).
