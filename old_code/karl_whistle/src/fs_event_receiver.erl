%%%-------------------------------------------------------------------
%%% File    : fs_event_receiver.erl
%%% Author  : Darren Schreiber <d@d-man.org>
%%% Description : FreeSWITCH socket manager. Manages send/receive events to/from FreeSWITCH and Erlang apps
%%%
%%% Created :  01 May 2010
%%%-------------------------------------------------------------------
-module(fs_event_receiver).

-behaviour(gen_server).
-include("../include/common.hrl").

%% API
-export([start_link/1, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(FsSock) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [FsSock], []).

start(FsSock) ->
  gen_server:start({local, ?SERVER}, ?MODULE, [FsSock], []).

stop() ->
  exit(whereis(?SERVER), normal).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([FsSock]) ->


  ?DEBUG("FreeSWITCH event receiver thread started.~n", []),

  spawn(fun() -> loop(FsSock, [], 0) end),

  %% All is well - hold on to our socket port
  {ok, #state{connection = FsSock}}.

% Echo back whatever data we receive on Socket.
loop(Socket, Msg, ContentLength) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
            ?DEBUG("COMPLETE PACKET: ~p~n", [ Msg ]),

            case ContentLength > 0 of
              true ->
                inet:setopts(Socket, [{packet, raw}]),
                {ok, Extra} = gen_tcp:recv(Socket, ContentLength),
                inet:setopts(Socket, [{packet, line}]),
                ?DEBUG("Got extra data:~n~p~n", [Extra]);
              _ -> ok
            end,

            loop(Socket, [], 0);
        {ok, Data} ->
            %%?DEBUG("Received a line: ~p~n.", [Data]),

            %% Parse the line
            Pos = string:str(Data, ": "),
            Key = string:substr(Data, 1, Pos - 1),
            Value = string:substr(Data, Pos + 2),

            Msg1 = Msg ++ [[Key, Value]],

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case string:equal(Key, "Content-Length") of
              true ->
                {Length, _Garbage} = string:to_integer(Value);
              _ ->
                Length = ContentLength
            end,

            loop(Socket, Msg1, Length);
        {error, closed} ->
            ok
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

