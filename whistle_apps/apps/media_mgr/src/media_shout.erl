%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Server to stream an MP3 or WAVE file, once or continuously
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media_shout).

-behaviour(gen_server).

%% API
-export([start_link/5, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("media.hrl").

-define(SERVER, ?MODULE).
-define(CONTENT_TYPE_MP3, <<"audio/mpeg">>).
-define(CONTENT_TYPE_WAV, <<"audio/x-wav">>).
-define(MAX_FETCH_RETRIES, 5).

-record(state, {
          media_file = #media_file{} :: #media_file{}
          ,media_id = <<>> :: binary()
          ,lsocket = 'undefined' :: 'undefined' | port()
          ,db = <<>> :: binary()
          ,doc = <<>> :: binary()
          ,attachment = <<>> :: binary()
          ,media_name = <<>> :: binary()
          ,content_type_extension = 'undefined' :: 'undefined' | ne_binary()
          ,send_to = [] :: list(binary()) | []
          ,stream_type = 'single' :: 'single' | 'continuous'
          ,media_loop = 'undefined' :: 'undefined' | pid()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Media, To, Type, Port, CallID) ->
    gen_server:start_link(?MODULE, [Media, To, Type, Port, CallID], []).

stop(Srv) ->
    gen_server:cast(Srv, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Media, To, Type, Port, CallID]) ->
    put(callid, CallID),
    {MediaName, Db, Doc, Attachment, ContentTypeExt} = Media,
    lager:debug("starting a ~s stream server to provide ~s", [Type, MediaName]),
    case inet:getstat(Port) of
        {ok, _} ->
            process_flag(trap_exit, true),
            {ok, #state{
               db=Db
               ,doc=Doc
               ,attachment=Attachment
               ,media_name=MediaName
               ,content_type_extension=wh_util:to_binary(ContentTypeExt)
               ,lsocket=Port
               ,send_to=[To]
               ,stream_type=Type
              }, 0};
        {error, Posix} ->
            lager:debug("stream server failed to start; lsock ~p error ~p", [Port, Posix]),
            {stop, Posix}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(timeout, #state{db=Db, doc=Doc, attachment=Attachment, media_name=MediaName, content_type_extension=ContentTypeExt
                            ,lsocket=LSocket, send_to=SendTo, stream_type=StreamType}=S) ->
    {ok, PortNo} = inet:port(LSocket),

    {ok, Content} = fetch_attachment(Db, Doc, Attachment),
    lager:debug("attachment binary fetched"),

    Size = byte_size(Content),
    ChunkSize = case ?CHUNKSIZE > Size of
                    true -> Size;
                    false -> ?CHUNKSIZE
                end,

    lager:debug("filesize: ~p", [Size]),
    lager:debug("chunksize: ~p", [ChunkSize]),

    Extension = case ContentTypeExt =:= undefined andalso filename:extension(Attachment) of
                    false -> ContentTypeExt; % if content type is known
                    [] -> ContentTypeExt;
                    [$.|Ext] -> wh_util:to_binary(Ext);
                    <<$., Ext/binary>> -> Ext;
                    _ -> ContentTypeExt
                end,
    lager:debug("extension: ~s", [Extension]),

    Hostname = wh_util:get_hostname(),
    lager:debug("hostname: ~s", [Hostname]),

    CallID = get(callid),
    {Resp, Header, CT, StreamUrl} = case Extension of
                                        <<"mp3">> ->
                                            Self = self(),

                                            _Acceptor = spawn(fun() -> put(callid, CallID), start_shout_acceptor(Self, LSocket) end),
                                            lager:debug("spawned shout server for mp3: ~p", [_Acceptor]),

                                            Url = list_to_binary(["vlc://shout://", Hostname, ":", integer_to_list(PortNo), "/stream.mp3"]),

                                            {
                                              wh_shout:get_shout_srv_response(list_to_binary([?APP_NAME, ": ", ?APP_VERSION]), MediaName, ChunkSize, Url, ?CONTENT_TYPE_MP3)
                                              ,{0,wh_shout:get_shout_header(MediaName, Url)}
                                              ,?CONTENT_TYPE_MP3
                                              ,Url
                                            };
                                        <<"wav">> ->
                                            Self = self(),

                                            _Acceptor = spawn(fun() -> put(callid, CallID), start_stream_acceptor(Self, LSocket) end),
                                            lager:debug("spawned shout server for wav: ~p", [_Acceptor]),

                                            {
                                              get_http_response_headers(?CONTENT_TYPE_WAV, Size)
                                              ,undefined
                                              ,?CONTENT_TYPE_WAV
                                              ,list_to_binary(["vlc://http://", Hostname, ":", integer_to_list(PortNo), "/stream.wav"])
                                            }
                                    end,

    lager:debug("sending media_resp AMQP"),

    lists:foreach(fun(To) -> send_media_resp(MediaName, StreamUrl, To) end, SendTo),

    MediaFile = #media_file{stream_url=StreamUrl, contents=Content, content_type=CT, media_name=MediaName, chunk_size=ChunkSize
                            ,shout_response=Resp, shout_header=Header, continuous=(StreamType =:= continuous), pad_response=(CT =:= ?CONTENT_TYPE_MP3)},

    MediaLoop = spawn_link(fun() -> put(callid, CallID), play_media(MediaFile) end),
    lager:debug("spawned media_loop: ~p", [MediaLoop]),

    {noreply, S#state{media_loop=MediaLoop, media_file=MediaFile}, hibernate};

handle_info({add_listener, ListenerQ}, #state{stream_type=single, media_name=MediaName, db=Db, doc=Doc, attachment=Attachment}=S) ->
    CallID = get(callid),
    spawn(fun() ->
                  Media = {MediaName, Db, Doc, Attachment},
                  {ok, ShoutSrv} = media_shout_sup:start_shout(Media, ListenerQ, continuous, media_srv:next_port(), CallID),
                  media_srv:add_stream(MediaName, ShoutSrv)
          end),
    {noreply, S, hibernate};

handle_info({add_listener, ListenerQ}, #state{media_file=#media_file{stream_url=StreamUrl}, media_name=MediaName, send_to=SendTo}=S) ->
    send_media_resp(MediaName, StreamUrl, ListenerQ),
    {noreply, S#state{send_to=[ListenerQ | SendTo]}, hibernate};

handle_info({send_media, Socket}, #state{media_loop=undefined, media_file=MediaFile}=S) ->
    CallID = get(callid),
    lager:debug("starting a new process to satisfy send media request"),
    MediaLoop = spawn_link(fun() -> put(callid, CallID), play_media(MediaFile) end),
    ok = gen_tcp:controlling_process(Socket, MediaLoop),
    MediaLoop ! {add_socket, Socket},
    {noreply, S#state{media_loop = MediaLoop}, hibernate};
handle_info({send_media, Socket}, #state{media_loop=MediaLoop}=S) ->
    ok = gen_tcp:controlling_process(Socket, MediaLoop),
    MediaLoop ! {add_socket, Socket},
    {noreply, S};

handle_info({'EXIT', From, ok}, #state{media_loop=MediaLoop}=S) when From =:= MediaLoop ->
    {stop, normal, S};
handle_info({'EXIT', From, normal}, #state{media_loop=MediaLoop}=S) when From =:= MediaLoop ->
    {stop, normal, S};
handle_info({'EXIT', From, Reason}, #state{media_loop=MediaLoop, media_file=MediaFile}=S) when From =:= MediaLoop ->
    CallID = get(callid),
    MediaLoop1 = spawn_link(fun() -> put(callid, CallID), play_media(MediaFile) end),
    lager:debug("media stream process ~p unexpectly died ~s, restarting", [From, Reason]),
    {noreply, S#state{media_loop = MediaLoop1}, hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{lsocket=undefined}) ->
    lager:debug("stream server ~s termination", [_Reason]);
terminate(_Reason, #state{lsocket=LSock}) ->
    {ok, PortNo} = inet:port(LSock),
    lager:debug("closing port ~b", [PortNo]),
    gen_tcp:close(LSock),
    lager:debug("stream server ~s termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_media_resp(MediaName, Url, To) ->
    Resp = [{<<"Media-Name">>, MediaName}
            ,{<<"Stream-URL">>, Url}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
    lager:debug("notifying requestor that ~s as available at ~s", [MediaName, Url]),
    wapi_media:publish_resp(To, Resp).

start_shout_acceptor(Parent, LSock) ->
    {ok, PortNo} = inet:port(LSock),
    lager:debug("shout acceptor listening on ~b for client connections", [PortNo]),
    case gen_tcp:accept(LSock) of
        {ok, S} ->
            CallID = get(callid),
            spawn(fun() -> put(callid, CallID), start_shout_acceptor(Parent, LSock) end),

            case wh_shout:get_request(S) of
                void ->
                    lager:debug("recieved invalid client request"),
                    gen_tcp:close(S);
                _ ->
                    lager:debug("new client connected"),
                    ok = gen_tcp:controlling_process(S, Parent),
                    Parent ! {send_media, S}
            end;
        _ -> ok
    end.

start_stream_acceptor(Parent, LSock) ->
    {ok, PortNo} = inet:port(LSock),
    lager:debug("raw acceptor listening on ~b for client connections", [PortNo]),
    case gen_tcp:accept(LSock) of
        {ok, S} ->
            CallID = get(callid),
            spawn(fun() -> put(callid, CallID), start_stream_acceptor(Parent, LSock) end),

            {ok, {Address, Port}} = inet:peername(S),
            lager:debug("client connected from ~s:~b", [inet_parse:ntoa(Address), Port]),

            _Req = wh_shout:get_request(S),

            ok = gen_tcp:controlling_process(S, Parent),
            Parent ! {send_media, S};
        _ -> ok
    end.

play_media(#media_file{contents=Contents, shout_header=Header}=MediaFile) ->
    play_media(MediaFile, [], 0, byte_size(Contents), <<>>, Header).

play_media(#media_file{shout_response=ShoutResponse, shout_header=ShoutHeader}=MediaFile, [], _, Stop, _, _) ->
    lager:debug("started new process to stream media to client"),
    receive
        {add_socket, S} ->
            lager:debug("started stream"),
            ok = gen_tcp:send(S, [ShoutResponse]),
            play_media(MediaFile, [S], 0, Stop, <<>>, ShoutHeader);
        shutdown ->
            lager:debug("stream shutdown")
    after ?MAX_WAIT_FOR_LISTENERS ->
            lager:debug("stream stood up waiting for connection, outta here")
    end;
play_media(#media_file{continuous=Continuous, shout_response=ShoutResponse, shout_header=ShoutHeader}=MediaFile
           ,Socks, Offset, Stop, SoFar, Header) ->
    receive
        {add_socket, S} ->
            ok = gen_tcp:send(S, [ShoutResponse]),
            play_media(MediaFile, [S | Socks], Offset, Stop, SoFar, Header);
        shutdown ->
            lager:debug("stream shutdown")
    after 0 ->
            lager:debug("playing at offset ~p", [Offset]),
            case wh_shout:play_chunk(MediaFile, Socks, Offset, Stop, SoFar, Header) of
                {Socks1, Header1, Offset1, SoFar1} ->
                    lager:debug("continue playing at offset ~p", [Offset1]),
                    play_media(MediaFile, Socks1, Offset1, Stop, SoFar1, Header1);
                {done, Socks1} ->
                    case Continuous of
                        true ->
                            lager:debug("end of stream, looping"),
                            play_media(MediaFile, Socks1, 0, Stop, <<>>, ShoutHeader);
                        false ->
                            lager:debug("end of stream"),
                            [gen_tcp:close(S) || S <- Socks1]
                    end
            end
    end.

get_http_response_headers(CT, CL) ->
    ["HTTP/1.1 200 OK\r\n"
     ,"Server: ", ?APP_NAME, "/", ?APP_VERSION, "\r\n"
     ,"Content-Type: ", wh_util:to_list(CT), "\r\n"
     ,"Content-Disposition: identity\r\n"
     ,"Content-Length: ", wh_util:to_list(CL), "\r\n\r\n"].

-spec fetch_attachment/3 :: (binary(), binary(), binary()) -> {'ok', binary()} | {'error', 'timeout'}.
fetch_attachment(Db, Doc, Attachment) ->
    fetch_attachment(Db, Doc, Attachment, 0).

fetch_attachment(_DB, _Doc, _A, ?MAX_FETCH_RETRIES) ->
    lager:debug("Failed to retrieve attachment"),
    lager:debug("DB: ~s", [_DB]),
    lager:debug("Doc: ~s", [_Doc]),
    lager:debug("Attachment: ~s", [_A]),
    {error, timeout};
fetch_attachment(Db, Doc, Attachment, Retries) ->
    case couch_mgr:fetch_attachment(Db, Doc, Attachment) of
        {ok, _Content}=OK -> OK;
        {error, _Err} ->
            lager:debug("Error getting attachment: ~s", [_Err]),
            timer:sleep(100 * Retries),
            fetch_attachment(Db, Doc, Attachment, Retries+1)
    end.
