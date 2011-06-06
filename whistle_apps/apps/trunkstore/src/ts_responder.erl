%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Trunk-Store responder waits for Auth and Route requests on the broadcast
%%% Exchange, and delievers the requests to the corresponding handler.
%%% TS responder also receives responses from the handlers and returns them
%%% to the requester.
%%% Each request received by TS_RESPONDER should be put into a new spawn()
%%% to avoid blocking on each request.
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_responder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).
-define(ROUTE_QUEUE_NAME, <<"ts_responder.route.queue">>).
-define(AUTH_QUEUE_NAME, <<"ts_responder.auth.queue">>).

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
    logger:debug("~p.~p(~p): Starting up responder with AMQP Queue: ~s", [?MODULE, ?LINE, self(), CQ]),
    {noreply, S#state{is_amqp_up=is_binary(CQ)}, 1000};

handle_info({amqp_host_down, H}, S) ->
    logger:debug("~p.~p(~p): AMQP Host(~s) down", [?MODULE, ?LINE, self(), H]),
    {noreply, S#state{is_amqp_up=false}, 1000};

handle_info(Req, #state{is_amqp_up=false}=S) ->
    case start_amqp() of
	{ok, CQ} when is_binary(CQ) ->
	    handle_info(Req, S#state{is_amqp_up=true});
	_ ->
	    logger:debug("~p.~p(~p): Dropping request ~p~n", [?MODULE, ?LINE, self(), Req]),
	    {noreply, S}
    end;

%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload) end),
    {noreply, State};

handle_info(#'basic.consume_ok'{}, S) ->
    {noreply, S};

%% catch all so we don't lose state
handle_info(_Unhandled, State) ->
    logger:debug("~p.~p(~p): unknown message: ~p~n", [?MODULE, ?LINE, self(), _Unhandled]),
    {noreply, State, 1000}.

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
    logger:debug("~p.~p(~p): terminating: ~p~n", [?MODULE, ?LINE, self(), _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    logger:debug("~p.~p(~p): Code Change from ~p~n", [?MODULE, ?LINE, self(), _OldVsn]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(handle_req/2 :: (ContentType :: binary(), Payload :: binary()) -> no_return()).
handle_req(<<"application/json">>, Payload) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),

    CallID = wh_json:get_value(<<"Call-ID">>, JObj, wh_json:get_value(<<"Msg-ID">>, JObj, <<"0000000000">>)),
    logger:info("~s | Start | ~p.~p(~p): Received ~s", [CallID, ?MODULE, ?LINE, self(), Payload]),

    process_req(get_msg_type(JObj), JObj, CallID);
handle_req(_ContentType, _Payload) ->
    logger:err("~p.~p(~p): Received payload with unknown content type: ~p", [?MODULE, ?LINE, self(), _ContentType]).

-spec(get_msg_type/1 :: (JObj :: json_object()) -> tuple(binary(), binary())).
get_msg_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), CallID :: binary()) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, JObj, CallID) ->
    try
    case whistle_api:auth_req_v(JObj) andalso ts_Auth:handle_req(JObj) of
	false ->
	    logger:info("~s | End | ~p.~p(~p): Failed to validate authentication request", [CallID, ?MODULE, ?LINE, self()]);
	{ok, JSON} ->
	    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
	    logger:info("~s | Pass | ~s | ~p.~p(~p): Responding with ~s", [CallID, RespQ, ?MODULE, ?LINE, self(), JSON]),
	    send_resp(JSON, RespQ),
	    logger:info("~s | End | ~p.~p(~p): Finished authentication request", [CallID, ?MODULE, ?LINE, self()]);
	{error, _Msg} ->
	    logger:info("~s | End | ~p.~p(~p): Authentication request error: ~p", [CallID, ?MODULE, ?LINE, self(), _Msg])
    end
    catch
	A:B ->
	    logger:info("~s | End | ~p.~p(~p): Authentication request exception: ~p:~p", [CallID, ?MODULE, ?LINE, self(), A, B])
    end;

process_req({<<"dialplan">>,<<"route_req">>}, JObj, CallID) ->
    try
    case whistle_api:route_req_v(JObj) andalso ts_route:handle_req(JObj) of
	false ->
	    logger:info("~s | End | ~p.~p(~p): Failed to validate route request", [CallID, ?MODULE, ?LINE, self()]);
	{ok, JSON} ->
	    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
	    logger:info("~s | Pass | ~s | ~p.~p(~p): Responding with ~s", [CallID, RespQ, ?MODULE, ?LINE, self(), JSON]),
	    send_resp(JSON, RespQ),
	    logger:info("~s | End | ~p.~p(~p): Finished route request", [CallID, ?MODULE, ?LINE, self()]);
	{error, _Msg} ->
	    logger:info("~s | End | ~p.~p(~p): Route request error: ~p", [CallID, ?MODULE, ?LINE, self(), _Msg])
    end
    catch
	A:B ->
	    logger:info("~s | End | ~p.~p(~p): Route request exception: ~p:~p", [CallID, ?MODULE, ?LINE, self(), A, B])
    end;

process_req(_MsgType, _Prop, CallID) ->
    logger:err("~s | End | ~p.~p(~p): Unhandled request of type ~p", [CallID, ?MODULE, ?LINE, self(), _MsgType]).

-spec(send_resp/2 :: (JSON :: iolist(), RespQ :: binary()) -> no_return()).
send_resp(JSON, RespQ) ->
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).

-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    ReqQueue = amqp_util:new_callmgr_queue(?ROUTE_QUEUE_NAME, [{exclusive, false}]),

    ReqQueue1 = amqp_util:new_callmgr_queue(?AUTH_QUEUE_NAME, [{exclusive, false}]),

    amqp_util:basic_qos(1), %% control egress of messages from the queue, only send one at time (load balances)

    %% Bind the queue to an exchange
    amqp_util:bind_q_to_callmgr(ReqQueue, ?KEY_ROUTE_REQ),
    amqp_util:bind_q_to_callmgr(ReqQueue1, ?KEY_AUTH_REQ),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(ReqQueue, [{exclusive, false}]),
    amqp_util:basic_consume(ReqQueue1, [{exclusive, false}]),

    {ok, ReqQueue}.
