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
start_link(Path) ->
    proc_lib:start_link(?MODULE, init, [self(), Path]).

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

    spawn(fun() ->
		  case gen_tcp:accept(LSock) of
		      {ok, S} -> writer_loop(S, [], Self, WriterRef);
		      _ -> Self ! {error, WriterRef}
		  end
	  end),
    loop(Path, Port, WriterRef).

loop(Path, Port, WriterRef) ->
    receive
	{Sender, Ref, get_path} ->
	    Sender ! {Ref, Path},
	    loop(Path, Port, WriterRef);
	{Sender, Ref, get_url} ->
	    Host = whistle_util:to_binary(net_adm:localhost()),
	    PortBin = whistle_util:to_binary(Port),
	    Url = <<"http://", Host/binary, ":", PortBin/binary, "/stream.mp3">>,
	    Sender ! {Ref, Url},
	    loop(Path, Port, WriterRef);
	{error, WriterRef} ->
	    throw(shout_writer_error);
	{done, WriterRef} ->
	    ok;
	_ -> loop(Path, Port, WriterRef)
    end.

writer_loop(Sock, Data, Parent, Ref) ->
    inet:setopts(Sock, [{active,once}, binary]),
    receive
	{tcp, Sock, Bin} -> writer_loop(Sock, [Bin | Data], Parent, Ref);
	{tcp_closed, Sock} ->
	    Path = ecallmgr_shout:get_path(Parent),
	    ok = file:write_file(Path, lists:reverse(Data)),
	    Parent ! {done, Ref};
	_Other ->
	    logger:format_log(info, "Writer(~p): other ~p~n", [self(), _Other]),
	    writer_loop(Sock, Data, Parent, Ref)
    end.
