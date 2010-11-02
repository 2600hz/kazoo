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
-export([start_link/0, set_couch_host/1, set_amqp_host/1, add_post_handler/2, rm_post_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-define(SERVER, ?MODULE). 

-record(state, {amqp_host = "" :: string()
		,couch_host = "" :: string()
                ,broad_q = <<>> :: binary()
                ,tar_q = <<>> :: binary()
		,post_handlers = [] :: list(tuple(binary(), pid())) %% [ {CallID, PostHandlerPid} ]
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_couch_host(CHost) ->
    gen_server:call(?SERVER, {set_couch_host, CHost}, infinity).

set_amqp_host(AHost) ->
    gen_server:call(?SERVER, {set_amqp_host, AHost}, infinity).

add_post_handler(CallID, Pid) ->
    gen_server:call(?SERVER, {add_post_handler, CallID, Pid}, infinity).

rm_post_handler(CallID) ->
    gen_server:cast(?SERVER, {rm_post_handler, CallID}).

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
    {ok, #state{}}.

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
handle_call({add_post_handler, CallID, Pid}, _From, #state{post_handlers=Posts}=State) ->
    format_log(info, "TS_RESPONDER(~p): Add handler(~p) for ~p~n", [self(), Pid, CallID]),
    {reply, ok, State#state{post_handlers=[{CallID, Pid} | Posts]}};
handle_call({set_couch_host, CHost}, _From, #state{couch_host=OldCHost}=State) ->
    format_log(info, "TS_RESPONDER(~p): Updating couch host from ~p to ~p~n", [self(), OldCHost, CHost]),
    ts_couch:set_host(CHost),
    ts_carrier:force_carrier_refresh(),
    ts_credit:force_rate_refresh(),
    {reply, ok, State#state{couch_host=CHost}};
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=""}=State) ->
    format_log(info, "TS_RESPONDER(~p): Setting couch host to ~p~n", [self(), AHost]),
    {ok, BQ, TQ} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host=AHost, broad_q=BQ, tar_q=TQ}};
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=OldAHost, broad_q=OBQ, tar_q=OTQ}=State) ->
    format_log(info, "TS_RESPONDER(~p): Updating couch host from ~p to ~p~n", [self(), OldAHost, AHost]),
    amqp_util:queue_delete(OldAHost, OBQ),
    amqp_util:queue_delete(OldAHost, OTQ),
    amqp_manager:close_channel(self(), OldAHost),

    {ok, BQ, TQ} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host=AHost, broad_q=BQ, tar_q=TQ}};
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
handle_cast({rm_post_handler, CallID}, #state{post_handlers=Posts}=State) ->
    format_log(info, "TS_RESPONDER(~p): Remove handler for ~p~n", [self(), CallID]),
    case lists:keyfind(CallID, 1, Posts) of
	false ->
	    {reply, {error, no_handler, CallID}, State};
	{CallID, _Pid} ->
	    {noreply, State#state{post_handlers=lists:keydelete(CallID, 1, Posts)}}
    end;
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
handle_info({'EXIT', _Pid, Reason}, State) ->
    format_log(error, "TS_RESPONDER(~p): Received EXIT(~p) from ~p...~n", [self(), Reason, _Pid]),
    {noreply, Reason, State};
%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};
%% catch all so we don't lose state
handle_info(_Unhandled, State) ->
    format_log(info, "TS_RESPONDER(~p): unknown info request: ~p~n", [self(), _Unhandled]),
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
terminate(_Reason, #state{amqp_host=AHost, broad_q=BQ, tar_q=TQ}) ->
    amqp_util:queue_delete(AHost, BQ),
    amqp_util:queue_delete(AHost, TQ),
    format_log(error, "TS_RESPONDER(~p): Going down(~p)...~n", [self(), _Reason]),
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
    format_log(info, "TS_RESPONDER(~p): Code Change called~n", [self()]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_req(ContentType, Payload, State) ->
    case ContentType of
	<<"application/json">> ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "TS_RESPONDER(~p): Recv CT: ~p~nPayload: ~p~n", [self(), ContentType, Prop]),
	    process_req(get_msg_type(Prop), Prop, State);
	_ ->
	    format_log(info, "TS_RESPONDER(~p): recieved unknown msg type: ~p~n", [self(), ContentType])
    end.

get_msg_type(Prop) ->
    { get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop) }.

process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    case whistle_api:auth_req_v(Prop) of
	false ->
	    format_log(error, "TS_RESPONDER.auth(~p): Failed to validate auth_req~n", [self()]);
	true ->
	    case ts_auth:handle_req(Prop) of
		{ok, JSON} ->
		    RespQ = get_value(<<"Server-ID">>, Prop),
		    send_resp(JSON, RespQ, State);
		{error, _Msg} ->
		    format_log(error, "TS_RESPONDER.auth(~p) ERROR: ~p~n", [self(), _Msg])
	    end
    end,
    State;
process_req({<<"dialplan">>,<<"route_req">>}, Prop, #state{tar_q=TQ}=State) ->
    case whistle_api:route_req_v(Prop) andalso ts_route:handle_req(Prop, TQ) of
	false ->
	    format_log(error, "TS_RESPONDER.route(~p): Failed to validate route_req~n", [self()]);
	{ok, JSON, Flags} ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    send_resp(JSON, RespQ, State),
	    start_post_handler(Prop, Flags, State);
	{error, _Msg} ->
	    format_log(error, "TS_RESPONDER.route(~p) ERROR: ~p~n", [self(), _Msg])
    end,
    State;
%% What to do with post route processing?
process_req({<<"dialplan">>,<<"route_win">>}, Prop, #state{post_handlers=Posts}) ->
    spawn(fun() ->
		  %% extract ctl queue for call, and send to the post_handler process associated with the call-id
		  case lists:keyfind(get_value(<<"Call-ID">>, Prop), 1, Posts) of
		      false ->
			  format_log(error, "TS_RESPONDER(~p): Unknown post handler for winning api msg~n~p~n", [self(), Prop]);
		      {CallID, Pid} ->
			  case erlang:is_process_alive(Pid) of
			      true ->
				  Pid ! {ctl_queue, CallID, get_value(<<"Control-Queue">>, Prop)};
			      false ->
				  ?MODULE:rm_post_handler(CallID)
			  end
		  end
	  end);
process_req(_MsgType, _Prop, _State) ->
    io:format("Unhandled Msg ~p~nJSON: ~p~n", [_MsgType, _Prop]).

%% Prop - RouteReq API Proplist
start_post_handler(Prop, Flags, #state{amqp_host=AmqpHost}) ->
    CallID = get_value(<<"Call-ID">>, Prop),
    Pid = ts_call_handler:start(CallID, Flags, AmqpHost),
    ?MODULE:add_post_handler(CallID, Pid).

-spec(send_resp/3 :: (JSON :: iolist(), RespQ :: binary(), tuple()) -> no_return()).
send_resp(JSON, RespQ, #state{amqp_host=AHost}) ->
    format_log(info, "TS_RESPONDER(~p): Sending to ~p~n", [self(), RespQ]),
    amqp_util:targeted_publish(AHost, RespQ, JSON, <<"application/json">>).

-spec(start_amqp/1 :: (AHost :: string()) -> tuple(ok, binary(), binary())).
start_amqp(AHost) ->
    BroadName = ["ts_responder.", net_adm:localhost()],
    TarName = ["ts_responder.callctl.", net_adm:localhost()],

    amqp_util:broadcast_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    BroadQueue = amqp_util:new_broadcast_queue(AHost, BroadName),
    TarQueue = amqp_util:new_targeted_queue(AHost, TarName),

    %% Bind the queue to an exchange
    amqp_util:bind_q_to_broadcast(AHost, BroadQueue),
    format_log(info, "TS_RESPONDER(~p): Bound ~p~n", [self(), BroadQueue]),
    amqp_util:bind_q_to_targeted(AHost, TarQueue, TarQueue),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(AHost, BroadQueue),
    amqp_util:basic_consume(AHost, TarQueue),

    format_log(info, "TS_RESPONDER(~p): Consuming on B(~p) and T(~p)~n", [self(), BroadQueue, TarQueue]),
    {ok, BroadQueue, TarQueue}.
