%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(media_shout).

-behaviour(gen_server).

%% API
-export([start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("media.hrl").

-define(SERVER, ?MODULE).

-record(state, {
	  media_file = #media_file{} :: #media_file{}
	  ,media_id = <<>> :: binary()
	  ,lsocket = undefined :: undefined | port()
	  ,db = <<>> :: binary()
	  ,doc = <<>> :: binary()
	  ,attachment = <<>> :: binary()
          ,media_name = <<>> :: binary()
	  ,send_to = [] :: list(binary()) | []
	  ,stream_type = single :: single | continuous
          ,media_loop = undefined :: undefined | pid()
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
start_link(Media, To, Type, Port) ->
    gen_server:start_link(?MODULE, [Media, To, Type, Port], []).

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
init([Media, To, Type, Port]) ->
    {MediaName, Db, Doc, Attachment} = Media,
    logger:format_log(info, "Starting up SHOUT on ~p for Media ~p of type ~p~n", [Port, MediaName, Type]),
    case inet:getstat(Port) of
	{ok, _} ->
	    process_flag(trap_exit, true),
	    {ok, #state{
	       db=Db
	       ,doc=Doc
	       ,attachment=Attachment
	       ,media_name=MediaName
	       ,lsocket=Port
	       ,send_to=[To]
	       ,stream_type=Type
	      }, 0};
	{error, Posix} ->
	    logger:format_log(error, "SHOUT server failed to start; lsock ~p error: ~p~n", [Port, Posix]),
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
    Reply = ok,
    {reply, Reply, State}.

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
    {stop, externally_stopped, State}.

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

%% handle_info(_, #state{stream_type=continuous}=S) ->
%%     {stop, continuous_not_implemented, S};

handle_info(timeout, #state{db=Db, doc=Doc, attachment=Attachment, media_name=MediaName}=S) ->
    try
	CT = <<"audio/mpeg">>,
	{ok, Content} = couch_mgr:fetch_attachment(Db, Doc, Attachment),

	{ok, PortNo} = inet:port(S#state.lsocket),
	StreamUrl = list_to_binary(["shout://", net_adm:localhost(), ":", integer_to_list(PortNo), "/stream.mp3"]),
	logger:format_log(info, "SHOUT(~p): Send ~p to ~p~n", [self(), StreamUrl, S#state.send_to]),

	lists:foreach(fun(To) -> send_media_resp(MediaName, StreamUrl, To) end, S#state.send_to),

	logger:format_log(info, "SHOUT(~p): URL: ~p~n", [self(), StreamUrl]),

	Self = self(),
	spawn(fun() -> start_acceptor(Self, S#state.lsocket) end),

	Size = binary:referenced_byte_size(Content),
	ChunkSize = case ?CHUNKSIZE > Size of
			true -> Size;
			false -> ?CHUNKSIZE
		    end,

	Resp = response(MediaName, ChunkSize, StreamUrl, CT),
	Header = get_header(MediaName, StreamUrl),

	MediaFile = #media_file{stream_url=StreamUrl, contents=Content, content_type=CT, media_name=MediaName, chunk_size=ChunkSize
				,shout_response=Resp, shout_header={0,Header}, continuous=(S#state.stream_type =:= continuous)},
	MediaLoop = spawn_link(fun() -> play_media(MediaFile) end),
		
	{noreply, S#state{media_loop=MediaLoop, media_file=MediaFile}}
    catch A:B ->
	    logger:format_log(info, "Exception Thrown: ~p:~p~n~p~n", [A, B, erlang:get_stacktrace()]),
	    {stop, normal, S}
    end;

handle_info({add_listener, ListenerQ}, #state{stream_type=single, media_name=MediaName, db=Db, doc=Doc, attachment=Attachment}=S) ->
    spawn(fun() ->
                  Media = {MediaName, Db, Doc, Attachment},
		  {ok, ShoutSrv} = media_shout_sup:start_shout(Media, ListenerQ, continuous, media_srv:next_port()),
		  media_srv:add_stream(MediaName, ShoutSrv)
	  end),
    {noreply, S};

handle_info({add_listener, ListenerQ}, #state{media_name=MediaName, media_file=Media}=S) ->
    send_media_resp(MediaName, Media#media_file.stream_url, ListenerQ),
    {noreply, S#state{send_to=[ListenerQ | S#state.send_to]}};

handle_info({send_media, Socket}, #state{media_loop=undefined, media_file=MediaFile}=S) ->
    MediaLoop = spawn_link(fun() -> play_media(MediaFile) end),
    gen_tcp:controlling_process(Socket, MediaLoop),
    MediaLoop ! {add_socket, Socket},
    logger:format_log(info, "SHOUT(~p): MediaLoop started at ~p~n", [self(), MediaLoop]),
    {noreply, S#state{media_loop = MediaLoop}};
handle_info({send_media, Socket}, #state{media_loop=MediaLoop}=S) ->
    gen_tcp:controlling_process(Socket, MediaLoop),
    MediaLoop ! {add_socket, Socket},
    {noreply, S};

handle_info({'EXIT', From, ok}, #state{media_loop=MediaLoop}=S) when From =:= MediaLoop ->
    logger:format_log(error, "SHOUT(~p): MediaLoop ~p went down ok, stopping~n", [self(), From]),
    {stop, media_finished, S};
handle_info({'EXIT', From, Reason}, #state{media_loop=MediaLoop, media_file=MediaFile}=S) when From =:= MediaLoop ->
    logger:format_log(error, "SHOUT(~p): MediaLoop ~p went down: ~p~n", [self(), From, Reason]),
    MediaLoop1 = spawn_link(fun() -> play_media(MediaFile) end),
    logger:format_log(info, "SHOUT(~p): MediaLoop restarted at ~p~n", [self(), MediaLoop1]),
    {noreply, S#state{media_loop = MediaLoop1}};

handle_info(_Info, State) ->
    logger:format_log(info, "MEDIA_SHOUT(~p): Recv info ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{lsocket=LSock}) ->
    logger:format_log(error, "SHOUT(~p): Shutting down: ~p~n", [self(), _Reason]),
    gen_tcp:close(LSock),
    ok.

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
    Prop = [{<<"Media-Name">>, MediaName}
	    ,{<<"Stream-URL">>, Url}
	    | whistle_api:default_headers(<<>>, <<"media">>, <<"media_resp">>, ?APP_NAME, ?APP_VERSION)],

    {ok, JSON} = whistle_api:media_resp(Prop),
    logger:format_log(info, "SHOUT(~p): Sending ~p to ~p~n", [self(), JSON, To]),
    amqp_util:targeted_publish(whapps_controller:get_amqp_host(), To, JSON).

start_acceptor(Parent, LSock) ->
    logger:format_log(info, "SHOUT.accept(~p): Acceptor started~n", [self()]),
    case gen_tcp:accept(LSock) of
	{ok, S} ->
	    spawn(fun() -> start_acceptor(Parent, LSock) end),
	    logger:format_log(info, "SHOUT.accept(~p): Listening on ~p~n", [self(), S]),
	    case get_request(S, []) of
		void ->
		    logger:format_log(info, "SHOUT.accept(~p): Socket ~p closed~n", [self(), S]),
		    gen_tcp:close(S);
		Request ->
		    logger:format_log(info, "SHOUT.accept(~p): Request for data received: ~p~n", [self(), Request]),
		    gen_tcp:controlling_process(S, Parent),
		    Parent ! {send_media, S}
	    end;
	_ -> ok
    end.

-spec(get_request/2 :: (S :: port(), L :: list()) -> no_return()).
get_request(S, L) ->
    inet:setopts(S, [{active,once}, binary]),
    receive
	{tcp, S, Bin} ->
	    L1 = L ++ binary_to_list(Bin),
	    %% split checks if the header is complete
	    case split(L1, []) of
		more ->
		    %% the header is incomplete we need more data
		    get_request(S, L1);
		{Request, _Rest} ->
		    %% header is complete
		    Request
	    end;

	{tcp_closed, _Socket} ->
	    void;

	_Any  ->
	    %% skip this
	    get_request(S, L)
    end.

split("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.

response(MediaName, ChunkSize, Url, CT) ->
    ["ICY 200 OK\r\n",
     "icy-notice1: MediaMgr WhApp SHOUTcast<BR>\r\n",
     "icy-name: ", whistle_util:to_list(MediaName), "\r\n",
     "icy-genre: Whistle Media\r\n",
     "icy-url: ", whistle_util:to_list(Url) ,"\r\n",
     "content-type: ", whistle_util:to_list(CT), "\r\n",
     "icy-pub: 1\r\n",
     "icy-metaint: ",integer_to_list(ChunkSize),"\r\n",
     "icy-br: 96\r\n\r\n"]. 

get_header(MediaName, Url) ->
    Bin = list_to_binary(["StreamTitle='",whistle_util:to_list(MediaName)
			  ,"';StreamUrl='",whistle_util:to_list(Url) ,"';"]),
    Nblocks = ((size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - size(Bin), 
    Extra = lists:duplicate(NPad, 0),
    list_to_binary([Nblocks, Bin, Extra]).

play_media(MediaFile) ->
    play_media(MediaFile, [], 0, binary:referenced_byte_size(MediaFile#media_file.contents), <<>>, MediaFile#media_file.shout_header).

play_media(MediaFile, [], _, Stop, _, _) ->
    receive
	{add_socket, S} ->
	    logger:format_log(info, "SHOUT.mloop(~p): Adding first socket: ~p~n", [self(), S]),
	    gen_tcp:send(S, [MediaFile#media_file.shout_response]),
	    play_media(MediaFile, [S], 0, Stop, <<>>, MediaFile#media_file.shout_header);
	shutdown ->
	    logger:format_log(info, "SHOUT.mloop(~p): shutdown: going down~n", [self()]),
	    exit(ok)
    after ?MAX_WAIT_FOR_LISTENERS ->
	    logger:format_log(info, "SHOUT.mloop(~p): have heard from anyone in ~p ms, going down~n", [self(), ?MAX_WAIT_FOR_LISTENERS]),
	    ok
    end;
play_media(MediaFile, Socks, Offset, Stop, SoFar, Header) ->
    receive
	{add_socket, S} ->
	    logger:format_log(info, "SHOUT.mloop(~p): Adding socket: ~p~n", [self(), S]),
	    gen_tcp:send(S, [MediaFile#media_file.shout_response]),
	    play_media(MediaFile, [S | Socks], Offset, Stop, SoFar, Header);
	shutdown ->
	    logger:format_log(info, "SHOUT.mloop(~p): shutdown: going down~n", [self()]),
	    exit(ok)
    after 0 ->
	    %% logger:format_log(info, "SHOUT.mloop(~p): Playing from ~p (~p)~n", [self(), Offset, Stop]),
	    case play_chunk(MediaFile, Socks, Offset, Stop, SoFar, Header) of
		{Socks1, Header1, Offset1, SoFar1} ->
		    %% logger:format_log(info, "SHOUT.mloop(~p): Continue with ~p (~p)~n", [self(), Offset1, Stop]),
		    play_media(MediaFile, Socks1, Offset1, Stop, SoFar1, Header1);
		{done, Socks1} ->
		    case MediaFile#media_file.continuous of
			true ->
			    %% logger:format_log(info, "SHOUT.mloop(~p): looping to play again~n", [self()]),
			    play_media(MediaFile, Socks1, 0, Stop, <<>>, MediaFile#media_file.shout_header);
			false ->
			    %% logger:format_log(info, "SHOUT.mloop(~p): done playing, bye!~n", [self()]),
			    exit(ok)
		    end
	    end
    end.

%% OffSet = first byte to play
%% Stop   = The last byte we can play
play_chunk(MediaFile, Socks, Offset, Stop, SoFar, Header) ->
    Need = MediaFile#media_file.chunk_size - size(SoFar),
    Last = Offset + Need,
    case Last >= Stop of
	true ->
	    %% not enough data so read as much as possible and return
	    Max = Stop - Offset,
	    Bin = binary:part(MediaFile#media_file.contents, Offset, Max),
	    StillActive = write_data(Socks, SoFar, Bin, Header, MediaFile#media_file.chunk_size),
	    {done, StillActive};
	false ->
	    Bin = binary:part(MediaFile#media_file.contents, Offset, Need),
	    StillActive = write_data(Socks, SoFar, Bin, Header, MediaFile#media_file.chunk_size),
	    {StillActive, bump(Header), Offset + Need, <<>>}
    end.


write_data(Sockets, B0, B1, Header, ChunkSize) ->
    %% Check that we really have got a block of the right size
    %% this is a very useful check that our program logic is
    %% correct
    case size(B0) + size(B1) of
	ChunkSize ->
	    Send = [B0, B1, the_header(Header)],
	    lists:filter(fun(S) ->
				 Write = gen_tcp:send(S, Send),
				 logger:format_log(info, "write_Data: ~p: ~p~n", [S, Send]),
				 Write =:= ok
			 end, Sockets);
	Size when Size < ChunkSize ->
	    H = the_header(Header),
	    Padding = binary:copy(<<0>>, ChunkSize-Size-size(Header)),
	    Send = [B0, B1, H, Padding],
	    lists:filter(fun(S) ->
				 Write = gen_tcp:send(S, Send),
				 logger:format_log(info, "write_Data1: ~p: ~p~n", [S, Send]),
				 Write =:= ok
			 end, Sockets);
	_Other ->
	    %% don't send the block - report an error
	    logger:format_log(info, "Block length Error: B0 = ~p b1=~p~n", [size(B0), size(B1)]),
	    Sockets
    end.

bump({K, H})     -> {K+1, H}.

the_header({K, H}) ->
    case K rem 5 of
	0 -> H;
	_ -> <<0>>
    end.
