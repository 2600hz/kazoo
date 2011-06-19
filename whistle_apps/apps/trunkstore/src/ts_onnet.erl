%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle calls coming from off-net (carriers), destined for on-net endpoints.
%%% Also handle if the user has configured failover, sending the call back
%%% off-net (either via SIP or PSTN).
%%% @end
%%% Created : 18 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_onnet).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).

-record(state, {
	  is_amqp_up = true :: boolean()
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
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    couch_mgr:db_create(?TS_DB),
    {ok, #state{}, 0}.

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
    {reply, ignored, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(timeout, S) ->
    {ok, CQ} = start_amqp(),
    ?LOG_SYS("Starting up responder with AMQP Queue: ~s", [CQ]),
    {noreply, S#state{is_amqp_up=is_binary(CQ)}, 1000}.

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
terminate(_Reason, _) ->
    ?LOG_SYS("Terminating: ~p~n", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    ?LOG_SYS("Code Change from ~p~n", [_OldVsn]),
    {ok, State}.

-spec(start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error)).
start_amqp() ->
%%     ReqQueue = amqp_util:new_callmgr_queue(?ROUTE_QUEUE_NAME, [{exclusive, false}]),
%%     ReqQueue1 = amqp_util:new_callmgr_queue(?AUTH_QUEUE_NAME, [{exclusive, false}]),

%%     try
%% 	{'basic.qos_ok'} = amqp_util:basic_qos(1), %% control egress of messages from the queue, only send one at time (load balances)

%% 	%% Bind the queue to an exchange
%% 	_ = amqp_util:bind_q_to_callmgr(ReqQueue, ?KEY_ROUTE_REQ),
%% 	_ = amqp_util:bind_q_to_callmgr(ReqQueue1, ?KEY_AUTH_REQ),

%% 	%% Register a consumer to listen to the queue
%% 	amqp_util:basic_consume(ReqQueue, [{exclusive, false}]),
%% 	amqp_util:basic_consume(ReqQueue1, [{exclusive, false}]),

%% 	{ok, ReqQueue}
%%     catch
%% 	_:_ -> {error, amqp_error}
%%     end.
ok.
