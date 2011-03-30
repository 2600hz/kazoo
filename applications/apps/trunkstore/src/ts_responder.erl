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

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
		callmgr_q = <<>> :: binary() | tuple(error, term())
		,is_amqp_up = true :: boolean()
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
    format_log(info, "TS_RESPONDER(~p): starting up amqp, will retry in a bit if doesn't work~n", [self()]),
    {ok, CQ} = start_amqp(),
    {noreply, S#state{callmgr_q=CQ, is_amqp_up=is_binary(CQ)}, 1000};

handle_info({amqp_host_down, H}, S) ->
    format_log(info, "TS_RESPONDER(~p): amqp host ~s went down, waiting a bit then trying again~n", [self(), H]),
    {ok, CQ} = start_amqp(),
    {noreply, S#state{callmgr_q=CQ, is_amqp_up=is_binary(CQ)}, 1000};

handle_info(Req, #state{callmgr_q={error, _}}=S) ->
    format_log(info, "TS_RESPONDER(~p): starting up amqp, will retry in a bit if doesn't work~n", [self()]),
    {ok, CQ} = start_amqp(),
    handle_info(Req, S#state{callmgr_q=CQ, is_amqp_up=is_binary(CQ)});

handle_info(Req, #state{is_amqp_up=false, callmgr_q=Q}=S) ->
    amqp_util:queue_delete(Q),
    format_log(info, "TS_RESPONDER(~p): starting up amqp, will retry in a bit if doesn't work~n", [self()]),
    {ok, CQ} = start_amqp(),
    handle_info(Req, S#state{callmgr_q=CQ, is_amqp_up=is_binary(CQ)});

%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};

handle_info(#'basic.consume_ok'{}, S) ->
    {noreply, S};

%% catch all so we don't lose state
handle_info(_Unhandled, State) ->
    format_log(info, "TS_RESPONDER(~p): unknown info request: ~p~n", [self(), _Unhandled]),
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
terminate(_Reason, #state{callmgr_q=CQ}) ->
    amqp_util:queue_delete(CQ),
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
-spec(handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> no_return()).
handle_req(<<"application/json">>, Payload, State) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "TS_RESPONDER(~p): Recv JSON~nPayload: ~p~n", [self(), Prop]),
    process_req(get_msg_type(Prop), Prop, State);
handle_req(_ContentType, _Payload, _State) ->
    format_log(info, "TS_RESPONDER(~p): recieved unknown msg type: ~p~n", [self(), _ContentType]).

-spec(get_msg_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary())).
get_msg_type(Prop) ->
    { get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    case whistle_api:auth_req_v(Prop) of
	false ->
	    format_log(error, "TS_RESPONDER.auth(~p): Failed to validate auth_req~n", [self()]);
	true ->
	    case ts_auth:handle_req(Prop) of
		{ok, JSON} ->
		    RespQ = get_value(<<"Server-ID">>, Prop),
		    send_resp(JSON, RespQ);
		{error, _Msg} ->
		    format_log(error, "TS_RESPONDER.auth(~p) ERROR: ~p~n", [self(), _Msg])
	    end
    end;
process_req({<<"dialplan">>,<<"route_req">>}, Prop, State) ->
    case whistle_api:route_req_v(Prop) andalso ts_route:handle_req(Prop) of
	false ->
	    format_log(error, "TS_RESPONDER.route(~p): Failed to validate route_req~n", [self()]);
	{ok, JSON} ->
	    RespQ = get_value(<<"Server-ID">>, Prop),
	    send_resp(JSON, RespQ);
	{error, _Msg} ->
	    format_log(error, "TS_RESPONDER.route(~p) ERROR: ~s~n", [self(), _Msg])
    end;
process_req(_MsgType, _Prop, _State) ->
    io:format("Unhandled Msg ~p~nJSON: ~p~n", [_MsgType, _Prop]).

-spec(send_resp/2 :: (JSON :: iolist(), RespQ :: binary()) -> no_return()).
send_resp(JSON, RespQ) ->
    format_log(info, "TS_RESPONDER(~p): JSON to ~s: ~s~n", [self(), RespQ, JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).

-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    CallmgrQueue = amqp_util:new_callmgr_queue(<<>>),

    %% Bind the queue to an exchange
    amqp_util:bind_q_to_callmgr(CallmgrQueue, ?KEY_AUTH_REQ),
    amqp_util:bind_q_to_callmgr(CallmgrQueue, ?KEY_ROUTE_REQ),
    amqp_util:bind_q_to_targeted(CallmgrQueue, CallmgrQueue),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(CallmgrQueue),

    format_log(info, "TS_RESPONDER(~p): Consuming on CM(~p)~n", [self(), CallmgrQueue]),
    {ok, CallmgrQueue}.
