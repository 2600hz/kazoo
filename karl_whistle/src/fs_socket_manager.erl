%%%-------------------------------------------------------------------
%%% File    : fs_socket_manager.erl
%%% Author  : Darren Schreiber <d@d-man.org>
%%% Description : FreeSWITCH socket manager. Manages send/receive events to/from FreeSWITCH and Erlang apps
%%%
%%% Created :  01 May 2010
%%%-------------------------------------------------------------------
-module(fs_socket_manager).

-behaviour(gen_server).
-include("../include/common.hrl").

%% API
-export([start_link/1, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection}).
-record(host, {hostname, port, auth}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Params) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Params], []).

start(Params) ->
  gen_server:start({local, ?SERVER}, ?MODULE, [Params], []).

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
init([Params]) ->
  #host{hostname = Hostname, port = Port, auth = Auth} = Params,

  %% TODO: Pause for a half second. This should be changed to a back-off algorithm
  receive after 500 -> ok end,

  %% Start a connection w/ FS Event socket system
  {ok, FsSock} = gen_tcp:connect(Hostname, Port, [list, {active, false}]),
  inet:setopts(FsSock, [{packet, line}]),
  ?DEBUG("Opened FreeSWITCH event socket to ~p~n", [Hostname]),

  ok = gen_tcp:send(FsSock, lists:concat(["auth ", Auth, "\n\n"])),

  %% Start the event sender/receiver threads
  fs_event_receiver:start_link(FsSock),
  fs_msg_sender:start_link(FsSock),

  %% Don't die if a child dies
  %%process_flag(trap_exit, true),

  %% All is well - hold on to our socket port
  {ok, #state{connection = FsSock}}.

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
handle_info({'EXIT', _Pid, Reason}, State) ->
  {stop, Reason, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  #state{connection = FsSock} = State,
  gen_tcp:close(FsSock),
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

