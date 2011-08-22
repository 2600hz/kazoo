%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Open a socket to receive a SHOUTCast stream; store the media data
%%% to the passed in FilePath.
%%% @end
%%% Created :  4 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_shout).

%% API
-export([start_link/2, get_recv_url/1, get_srv_url/1, get_path/1]).

%% gen_server callbacks
-export([init_recv/2, init_srv/2, init_auth/3]).

-include("ecallmgr.hrl").
-include_lib("whistle/include/wh_media.hrl").

-define(SERVER, ?MODULE).
-define(PORT_OPTIONS, [binary, {packet,0}, {active,false}, {reuseaddr, true}]).
-define(TIMEOUT, 10000). % wait ten seconds for the connection, then quit
-define(PW_ENCODED, base64:encode(<<"foo:bar">>)).
-define(AUTHN_RESP, <<"HTTP/1.0 200 OK\r\nServer: Whistle/1.0\r\nice-auth-user: 1\r\nice-auth-message: OK\r\n\r\n">>).

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

-spec(start_link/2 :: (Path :: binary(), Type :: srv | recv) -> tuple(ok, pid())).
start_link(Path, recv) ->
    proc_lib:start_link(?MODULE, init_recv, [self(), binary:replace(Path, <<".wav">>, <<".mp3">>)]
			,infinity, [{fullsweep_after, 0}]
		       );
start_link(Path, srv) ->
    proc_lib:start_link(?MODULE, init_srv, [self(), binary:replace(Path, <<".wav">>, <<".mp3">>)]
			,infinity, [{fullsweep_after, 0}]
		       ).

-spec(get_recv_url/1 :: (Srv :: pid()) -> binary() | timeout).
get_recv_url(Srv) ->
    Srv ! {self(), Ref=make_ref(), get_recv_url},
    receive
	{Ref, Url} ->
	    Url
    after 5000 ->
	    timeout
    end.

-spec(get_srv_url/1 :: (Srv :: pid()) -> binary() | timeout).
get_srv_url(Srv) ->
    Srv ! {self(), Ref=make_ref(), get_srv_url},
    receive
	{Ref, Url} ->
	    Url
    after 5000 ->
	    timeout
    end.

-spec(get_path/1 :: (Srv :: pid()) -> binary()).
get_path(Srv) ->
    Srv ! {self(), Ref=make_ref(), get_path},
    receive
	{Ref, Path} ->
	    Path
    after 5000 ->
	    timeout
    end.

%% start the receiving socket for a "record" stream
init_recv(Parent, Path) ->
    {ok, LSock} = gen_tcp:listen(0, ?PORT_OPTIONS),
    proc_lib:init_ack(Parent, {ok, self()}),

    {ok, Port} = inet:port(LSock),
    SrvRef = make_ref(),

    spawn_link(?MODULE, init_auth, [LSock, self(), SrvRef]),

    main_loop(Path, Port, SrvRef, LSock).

-spec(init_auth/3 :: (LSock :: port(), Parent :: pid(), Ref :: reference()) -> no_return()).
init_auth(LSock, Parent, Ref) ->
    {ok, S} = gen_tcp:accept(LSock),
    ok = gen_tcp:controlling_process(S, self()),
    auth_loop(S, [], Parent, Ref).

%% start a socket to stream a local file for "playback"
-spec(init_srv/2 :: (Parent :: pid(), Path :: binary()) -> no_return()).
init_srv(Parent, Path) ->
    {ok, LSock} = gen_tcp:listen(0, ?PORT_OPTIONS),
    proc_lib:init_ack(Parent, {ok, self()}),

    {ok, Port} = inet:port(LSock),
    SrvRef = make_ref(),
    Self = self(),

    spawn_link(fun() ->
		       {ok, S} = gen_tcp:accept(LSock),
		       ok = gen_tcp:controlling_process(S, self()),
		       send_loop(S, Path, Self, SrvRef)
	       end),
    main_loop(Path, Port, SrvRef, LSock).

%% the server's main loop, so other processing can get info about this shout server
-spec(main_loop/4 :: (Path :: binary(), Port :: integer(), SrvRef :: reference(), LSock :: port()) -> no_return()).
main_loop(Path, Port, SrvRef, LSock) ->
    receive
	{Sender, Ref, get_path} ->
	    Sender ! {Ref, Path},
	    main_loop(Path, Port, SrvRef, LSock);
	{Sender, Ref, get_recv_url} ->
	    Host = wh_util:to_binary(net_adm:localhost()),
	    PortBin = wh_util:to_binary(Port),
	    Base = filename:basename(Path),
	    Url = <<"shout://foo:bar@", Host/binary, ":", PortBin/binary, "/fs_", Base/binary>>,
	    Sender ! {Ref, Url},
	    main_loop(Path, Port, SrvRef, LSock);
	{Sender, SrvRef, lsock} ->
	    Sender ! {SrvRef, LSock},
	    main_loop(Path, Port, SrvRef, LSock);
	{Sender, Ref, get_srv_url} ->
	    Host = wh_util:to_binary(net_adm:localhost()),
	    PortBin = wh_util:to_binary(Port),
	    Base = filename:basename(Path),
	    Url = <<"shout://", Host/binary, ":", PortBin/binary, "/", Base/binary>>,
	    Sender ! {Ref, Url},
	    main_loop(Path, Port, SrvRef, LSock);
	{error, SrvRef} ->
	    ?LOG("SHOUT main loop for path: ~p: error, going down", [Path]),
	    gen_tcp:close(LSock),
	    throw(shout_writer_error);
	{done, SrvRef} ->
	    ?LOG("SHOUT main loop for path: ~p: done, going down", [Path]),
	    gen_tcp:close(LSock);
	_Other ->
	    ?LOG("SHOUT main loop for path: ~p: Unhandled message: ~p", [Path, _Other]),
	    main_loop(Path, Port, SrvRef, LSock)
    end.

%% receive the auth request and header(s) from FreeSWITCH, respond, and receive mp3 data
-spec(auth_loop/4 :: (Sock :: port(), Data :: iolist(), Parent :: pid(), SrvRef :: reference()) -> no_return()).
auth_loop(Sock, Data, Parent, SrvRef) ->
    ok = inet:setopts(Sock, [{active,once}, binary]),
    receive
	{tcp, Sock, Bin} ->
	    Headers = list_to_binary(lists:reverse([Bin | Data])),
	    Path = ?MODULE:get_path(Parent),

	    case is_authn_req_headers(Headers, Path) of
		true ->
		    ok = gen_tcp:send(Sock, ?AUTHN_RESP),
		    writer_loop(Sock, [], Parent, SrvRef);
		false ->
		    auth_loop(Sock, [Bin | Data], Parent, SrvRef)
	    end;
	{tcp_closed, Sock} ->
	    gen_tcp:close(Sock)
    after ?TIMEOUT ->
	    exit(timeout)
    end.

%% recv mp3 data from FreeSWITCH and write to local storage
-spec(writer_loop/4 :: (Sock :: port(), Data :: list(), Parent :: pid(), SrvRef :: reference()) -> no_return()).
writer_loop(Sock, Data, Parent, SrvRef) ->
    ok = inet:setopts(Sock, [{active,once}, binary]),
    receive
	{tcp, Sock, Bin} ->
	    writer_loop(Sock, [Bin | Data], Parent, SrvRef);
	{tcp_closed, Sock} ->
	    Path = ecallmgr_shout:get_path(Parent),
	    ok = file:write_file(Path, lists:reverse(Data)),
	    Parent ! {done, SrvRef},
	    gen_tcp:close(Sock);
	_Other ->
	    ?LOG("ECALL_SHOUT_WRITER: unknown receive ~p", [_Other]),
	    writer_loop(Sock, Data, Parent, SrvRef)
    after ?TIMEOUT ->
	    exit(timeout)
    end.

-spec(send_loop/4 :: (Sock :: port(), Path :: binary(), Parent :: pid(), SrvRef :: reference()) -> ok).
send_loop(Sock, Path, Parent, SrvRef) ->
    case wh_shout:get_request(Sock) of
	void ->
	    ?LOG("ECALL_SHOUT_SEND: Socket ~p closed", [Sock]),
	    gen_tcp:close(Sock);
	_Request ->
	    {ok, Contents} = file:read_file(Path),

	    Size = byte_size(Contents),
	    ChunkSize = case ?CHUNKSIZE > Size of
			    true -> Size;
			    false -> ?CHUNKSIZE
			end,

	    StreamUrl = ?MODULE:get_srv_url(Parent),
	    MediaName = filename:basename(Path),
	    CT = <<"audio/mpeg">>,
	    Resp = wh_shout:get_shout_srv_response(list_to_binary([?APP_NAME, ": ", ?APP_VERSION]), MediaName, ChunkSize, StreamUrl, CT),
	    Header = wh_shout:get_shout_header(MediaName, StreamUrl),

	    ok = gen_tcp:send(Sock, [Resp]),

	    MediaFile = #media_file{stream_url=StreamUrl, contents=Contents, content_type=CT, media_name=MediaName, chunk_size=ChunkSize
				,shout_header={0,Header}},
	    play_media(MediaFile, Sock),

	    Parent ! {done, SrvRef},
	    gen_tcp:close(Sock)
    end.

play_media(#media_file{contents=Contents, shout_header=Header}=MediaFile, Sock) ->
    play_media(MediaFile, Sock, 0, byte_size(Contents), <<>>, Header).

play_media(MediaFile, Sock, Offset, Stop, SoFar, Header) ->
    case wh_shout:play_chunk(MediaFile, Sock, Offset, Stop, SoFar, Header) of
	{Socks1, Header1, Offset1, SoFar1} ->
	    play_media(MediaFile, Socks1, Offset1, Stop, SoFar1, Header1);
	{done, _} ->
	    ok
    end.

%% check the headers
-spec(is_authn_req_headers/2 :: (Headers :: binary(), Path :: binary()) -> boolean()).
is_authn_req_headers(Headers, Path) ->
    Base = filename:basename(Path),
    lists:all(fun(H) -> is_authn_req_header(H, Base) end, binary:split(Headers, <<"\r\n">>, [global])).

%% check a header
-spec(is_authn_req_header/2 :: (Header :: binary(), Base :: binary()) -> boolean()).
is_authn_req_header(<<"SOURCE /", Rest/binary>>, Base) ->
    Base1 = <<"fs_", Base/binary>>,
    binary:longest_common_prefix([Rest, Base1]) =:= byte_size(Base1);
is_authn_req_header(<<"Authorization: Basic ", Base64/binary>>, _) ->
    Base64 =:= ?PW_ENCODED;
is_authn_req_header(<<"User-Agent: libshout/2.2.2">>, _) ->
    true;
is_authn_req_header(<<"Content-Type: audio/mpeg">>, _) ->
    true;
is_authn_req_header(<<>>, _) ->
    true;
is_authn_req_header(<<"ice-", _/binary>>, _) ->
    true;
is_authn_req_header(H, _) ->
    ?LOG("ECALL_SHOUT: IS_AUTHN_REQ_H unknown header: ~s", [H]),
    false.
