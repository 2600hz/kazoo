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
-export([start_link/1, get_stream_url/1, get_path/1]).

%% gen_server callbacks
-export([init/2]).

-define(SERVER, ?MODULE).
-define(PORT_OPTIONS, [binary, {packet,0}, {active,false}, {reuseaddr, true}]).
-define(TIMEOUT, 10000). % wait ten seconds for the connection, then quit
-define(PW_ENCODED, base64:encode(<<"foo:bar">>)).
-define(AUTH_RESP, <<"HTTP/1.0 200 OK\r\nServer: Whistle/1.0\r\nice-auth-user: 1\r\nice-auth-message: OK\r\n\r\n">>).

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

-spec(start_link/1 :: (Path :: binary()) -> tuple(ok, pid())).
start_link(Path) ->
    logger:format_log(info, "SHOUT start link for ~p", [Path]),
    proc_lib:start_link(?MODULE, init, [self(), binary:replace(Path, <<".wav">>, <<".mp3">>)]).

-spec(get_stream_url/1 :: (Srv :: pid()) -> binary() | timeout).
get_stream_url(Srv) ->
    Srv ! {self(), Ref=make_ref(), get_url},
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

init(Parent, Path) ->
    {ok, LSock} = gen_tcp:listen(0, ?PORT_OPTIONS),
    proc_lib:init_ack(Parent, {ok, self()}),

    {ok, Port} = inet:port(LSock),
    WriterRef = make_ref(),
    Self = self(),

    spawn_link(fun() ->
		       {ok, S} = gen_tcp:accept(LSock),
		       auth_loop(S, [], Self, WriterRef)
	       end),
    loop(Path, Port, WriterRef, LSock).

-spec(loop/4 :: (Path :: binary(), Port :: integer(), WriterRef :: reference(), LSock :: port()) -> no_return()).
loop(Path, Port, WriterRef, LSock) ->
    receive
	{Sender, Ref, get_path} ->
	    Sender ! {Ref, Path},
	    loop(Path, Port, WriterRef, LSock);
	{Sender, Ref, get_url} ->
	    Host = whistle_util:to_binary(net_adm:localhost()),
	    PortBin = whistle_util:to_binary(Port),
	    Base = filename:basename(Path),
	    Url = <<"shout://foo:bar@", Host/binary, ":", PortBin/binary, "/", Base/binary>>,
	    Sender ! {Ref, Url},
	    loop(Path, Port, WriterRef, LSock);
	{Sender, WriterRef, lsock} ->
	    Sender ! {WriterRef, LSock},
	    loop(Path, Port, WriterRef, LSock);
	{error, WriterRef} ->
	    logger:format_log(info, "SHOUT main loop for path: ~p: error, going down~n", [Path]),
	    gen_tcp:close(LSock),
	    throw(shout_writer_error);
	{done, WriterRef} ->
	    logger:format_log(info, "SHOUT main loop for path: ~p: done, going down~n", [Path]),
	    gen_tcp:close(LSock),
	    ok;
	_Other ->
	    logger:format_log(info, "SHOUT main loop for path: ~p: Other ~p~n", [Path, _Other]),
	    loop(Path, Port, WriterRef, LSock)
    end.

-spec(auth_loop/4 :: (Sock :: port(), Data :: list(), Parent :: pid(), WriterRef :: reference()) -> no_return()).
auth_loop(Sock, Data, Parent, WriterRef) ->
    inet:setopts(Sock, [{active,once}, binary]),
    receive
	{tcp, Sock, Bin} ->
	    Headers = whistle_util:to_binary(lists:reverse([Bin | Data])),
	    Path = ?MODULE:get_path(Parent),

	    case is_auth_req_header(Headers, Path) of
		true ->
		    ok = gen_tcp:send(Sock, ?AUTH_RESP),
		    writer_loop(Sock, [], Parent, WriterRef);
		false ->
		    auth_loop(Sock, [Bin | Data], Parent, WriterRef)
	    end;
	{tcp_closed, Sock} ->
	    gen_tcp:close(Sock),
	    exit(shout_auth_socket_closed)
    after ?TIMEOUT ->
	    exit(timeout)
    end.

-spec(writer_loop/4 :: (Sock :: port(), Data :: list(), Parent :: pid(), WriterRef :: reference()) -> no_return()).
writer_loop(Sock, Data, Parent, WriterRef) ->
    inet:setopts(Sock, [{active,once}, binary]),
    receive
	{tcp, Sock, Bin} ->
	    writer_loop(Sock, [Bin | Data], Parent, WriterRef);
	{tcp_closed, Sock} ->
	    Path = ecallmgr_shout:get_path(Parent),
	    ok = file:write_file(Path, lists:reverse(Data)),
	    gen_tcp:close(Sock),
	    Parent ! {done, WriterRef};
	_Other ->
	    writer_loop(Sock, Data, Parent, WriterRef)
    after ?TIMEOUT ->
	    exit(timeout)
    end.

is_auth_req_header(Headers, Path) ->
    Base = filename:basename(Path),

    lists:all(fun(H) -> is_auth_req_headers(H, Base) end, binary:split(Headers, <<"\r\n">>, [global])).

is_auth_req_headers(<<"SOURCE /", Rest/binary>>, Base) ->
    binary:longest_common_prefix([Rest, Base]) =:= byte_size(Base);
is_auth_req_headers(<<"Authorization: Basic ", Base64/binary>>, _) ->
    Base64 =:= ?PW_ENCODED;
is_auth_req_headers(<<"User-Agent: libshout/2.2.2">>, _) ->
    true;
is_auth_req_headers(<<"Content-Type: audio/mpeg">>, _) ->
    true;
is_auth_req_headers(<<>>, _) ->
    true;
is_auth_req_headers(<<"ice-", _/binary>>, _) ->
    true;
is_auth_req_headers(H, _) ->
    logger:format_log(error, "IS_AUTH_REQ_H: unknown h: ~p~n", [H]),
    false.
