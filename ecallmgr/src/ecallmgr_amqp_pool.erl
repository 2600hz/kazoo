%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Manage a pool of amqp queues
%%% @end
%%% Created : 28 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, route_req/1, route_req/2, reg_query/1, reg_query/2, media_req/1, media_req/2]).
-export([authn_req/1, authn_req/2, authz_req/1, authz_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(WORKER_COUNT, 10).
-define(DEFAULT_TIMEOUT, 5000).

%% every X ms, compare RequestsPer to WorkerCount
%% If RP < WC, reduce Ws by max(WC-RP, OrigWC)
-define(BACKOFF_PERIOD, 2500). % arbitrary at this point

-record(state, {
	  worker_count = ?WORKER_COUNT :: integer()
          ,orig_worker_count = ?WORKER_COUNT :: integer() % scale back workers after a period of time
          ,workers = queue:new() :: queue()
          ,requests_per = 0 :: non_neg_integer()
	  ,amqp_ref = undefined :: undefined | reference()
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?WORKER_COUNT], []).

start_link(WorkerCount) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WorkerCount], []).

authn_req(Prop) ->
    authn_req(Prop, ?DEFAULT_TIMEOUT).
authn_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun wh_api:authn_req/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_AUTHN_REQ) end
			      }, Timeout).

authz_req(Prop) ->
    authz_req(Prop, ?DEFAULT_TIMEOUT).
authz_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun wh_api:authz_req/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_AUTHZ_REQ) end
			     }, Timeout).

route_req(Prop) ->
    route_req(Prop, ?DEFAULT_TIMEOUT).
route_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun wh_api:route_req/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_ROUTE_REQ) end
			     }, Timeout).

reg_query(Prop) ->
    reg_query(Prop, ?DEFAULT_TIMEOUT).

reg_query(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun wh_api:reg_query/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_REG_QUERY) end
			     }, Timeout).

media_req(Prop) ->
    media_req(Prop, ?DEFAULT_TIMEOUT).

media_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun wh_api:media_req/1
			      ,fun(JSON) -> amqp_util:callevt_publish(JSON) end
			     }, Timeout).

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
init([Count]) ->
    process_flag(trap_exit, true),
    {ok, #state{worker_count=Count, orig_worker_count=Count}, 0}.

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
handle_call({request, {struct, Prop}, ApiFun, PubFun}, From, State) ->
    handle_call({request, Prop, ApiFun, PubFun}, From, State);
handle_call({request, Prop, ApiFun, PubFun}, From, #state{workers=W, worker_count=WC, requests_per=RP}=State) ->
    case queue:out(W) of
	{{value, Worker}, W1} ->
	    Worker ! {request, Prop, ApiFun, PubFun, From, self()},
	    {noreply, State#state{workers=W1, requests_per=RP+1}, hibernate};
	{empty, _} ->
	    Worker = start_worker(),
	    ?LOG("starting additional worker ~p", [Worker]),
	    Worker ! {request, Prop, ApiFun, PubFun, From, self()},
	    {noreply, State#state{worker_count=WC+1, requests_per=RP+1}, hibernate}
    end.

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
handle_info(timeout, #state{worker_count=WC, workers=Ws, orig_worker_count=OWC}=State) ->
    ?LOG("checking worker totals: WC: ~b Q len: ~b", [WC, queue:len(Ws)]),
    QLen = queue:len(Ws),
    Count = case WC-QLen of
		X when X =< 0 andalso QLen =:= 0 -> OWC;
		X when X < 0 -> 0;
		X -> X
	    end,
    ?LOG("adding ~b workers", [Count]),
    Ws1 = lists:foldr(fun(W, Ws0) -> queue:in(W, Ws0) end, Ws, [ start_worker() || _ <- lists:seq(1, Count) ]),
    erlang:start_timer(?BACKOFF_PERIOD, self(), reduce_labor_force),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1)}, hibernate};

handle_info({worker_free, W}, #state{workers=Ws}=State) ->
    {noreply, State#state{workers=queue:in(W, Ws)}, hibernate};

handle_info({'EXIT', W, amqp_host_down}, #state{workers=Ws, amqp_ref=undefined}=State) ->
    ?LOG("AMQP host down, ~p exited, starting timer", [W]),
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),
    Ref = erlang:start_timer(1000, self(), {amqp_check, 1000}),
    {noreply, State#state{worker_count=queue:len(Ws1), workers=Ws1, amqp_ref=Ref}, hibernate};

handle_info({'EXIT', W, amqp_host_down}, #state{workers=Ws}=State) ->
    ?LOG("AMQP host down, ~p exited", [W]),
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),
    {noreply, State#state{worker_count=queue:len(Ws1), workers=Ws1}, hibernate};

handle_info({timeout, Ref, {amqp_check, LastTimeout}}, #state{amqp_ref=Ref}=State) ->
    ?LOG("checking AMQP host connectivity"),
    case amqp_util:is_host_available() of
	true -> {noreply, State#state{worker_count=0, workers=queue:new(), amqp_ref=undefined}, 0};
	false ->
	    NewT = LastTimeout*2,
	    Timeout = case NewT > ?BACKOFF_PERIOD of true -> ?BACKOFF_PERIOD; false -> NewT end,
	    Ref1 = erlang:start_timer(Timeout, self(), {amqp_check, Timeout}),
	    {noreply, State#state{amqp_ref=Ref1}, hibernate}
    end;

handle_info({'EXIT', W, _Reason}, #state{workers=Ws, amqp_ref=Ref}=State) when is_reference(Ref) ->
    ?LOG("Worker down, amqp_ref set so wait: ~p", [_Reason]),
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1)}, hibernate};

handle_info({'EXIT', W, _Reason}, #state{workers=Ws, worker_count=WC, orig_worker_count=OWC}=State) when WC < OWC ->
    ?LOG("Worker down: ~p", [_Reason]),
    Ws1 = queue:in(start_worker(), queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws)),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1)}, hibernate};

handle_info({'EXIT', W, _Reason}, #state{workers=Ws}=State) ->
    ?LOG("Worker down: ~p", [_Reason]),
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1)}, hibernate};

handle_info({timeout, _, reduce_labor_force}
	    ,#state{workers=Ws, worker_count=WC, requests_per=RP, orig_worker_count=OWC}=State) when RP < OWC andalso WC > OWC ->
    ?LOG("reducing back to original labor force of ~p from ~p", [OWC, WC]),
    Ws1 = lists:foldl(fun(_, Q0) ->
			      case queue:len(Q0) =< OWC of
				  true -> Q0;
				  false ->
				      {{value, W}, Q1} = queue:out(Q0),
				      W ! shutdown,
				      Q1
			      end
		      end, Ws, lists:seq(1,WC-OWC)),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1), requests_per=0}, hibernate};

handle_info({timeout, _, reduce_labor_force}
	    ,#state{workers=Ws, worker_count=WC, requests_per=RP, orig_worker_count=OWC}=State) when RP < WC andalso WC > OWC ->
    ?LOG("scaling back labor force from ~p to ~p", [WC, WC-RP]),
    Ws1 = lists:foldl(fun(_, Q0) ->
			      case queue:len(Q0) =< OWC of
				  true -> Q0;
				  false ->
				      {{value, W}, Q1} = queue:out(Q0),
				      W ! shutdown,
				      Q1
			      end
		      end, Ws, lists:seq(1,WC-RP)),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1), requests_per=0}, hibernate};

handle_info({timeout, _, reduce_labor_force}, State) ->
    {noreply, State#state{requests_per=0}, hibernate};

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
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
terminate(_Reason, _State) ->
    ?LOG("Terminating: ~p", [_Reason]).

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
start_worker() ->
    spawn_link(fun() -> worker_init() end).

worker_init() ->
    put(callid, <<"0000000000">>),
    try
	Q = amqp_util:new_targeted_queue(),
	_ = amqp_util:bind_q_to_targeted(Q),
	_ = amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),

	worker_free(Q)
    catch
	_:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
	    exit(amqp_host_down)
    end.

-spec(worker_free/1 :: (Q :: binary()) -> no_return()).
worker_free(Q) ->
    receive
	{request, Prop, ApiFun, PubFun, {Pid, _}=From, Parent} ->
	    Prop1 = [ {<<"Server-ID">>, Q} | lists:keydelete(<<"Server-ID">>, 1, Prop)],
	    case ApiFun(Prop1) of
		{ok, JSON} ->
                    put_callid(Prop1),
		    Ref = erlang:monitor(process, Pid),
		    PubFun(JSON),
		    ?LOG("worker recieved task from ~w", [Pid]),
		    ?LOG("worker sent ~s", [JSON]),
		    worker_busy(Q, From, Ref, Parent);
		{error, _}=E ->
		    gen_server:reply(From, E),
		    worker_free(Q)
	    end;
	#'basic.consume_ok'{} ->
	    worker_free(Q);
	{amqp_host_down, _} ->
	    ?LOG_SYS("lost AMQP connection"),
	    exit(amqp_host_down);
	{amqp_lost_channel,no_connection} ->
	    ?LOG_SYS("lost AMQP connection"),
	    exit(amqp_host_down);
	shutdown ->
	    ?LOG_SYS("going on permanent leave");
	_Other ->
	    worker_free(Q)
    end.

worker_busy(Q, From, Ref, Parent) ->
    Start = erlang:now(),
    receive
	{amqp_host_down, _} ->
	    ?LOG("lost AMQP connection"),
	    exit(amqp_host_down);
	{amqp_lost_channel,no_connection} ->
	    ?LOG("lost AMQP connection"),
	    exit(amqp_host_down);
	{_, #amqp_msg{payload = Payload}} ->
	    ?LOG("recieved response after ~b ms, ~s", [timer:now_diff(erlang:now(), Start) div 1000, Payload]),
	    gen_server:reply(From, {ok, mochijson2:decode(Payload)});
	{'DOWN', Ref, process, Pid, _Info} ->
	    ?LOG_END("requestor (~w) down, giving up on task", [Pid])
    end,
    erlang:demonitor(Ref, [flush]),
    Parent ! {worker_free, self()},

    put(callid, <<"0000000000">>),
    worker_free(Q).

put_callid(Props) ->
    case props:get_value(<<"Call-ID">>, Props) of
	undefined -> put(callid, props:get_value(<<"Msg-ID">>, Props, <<"0000000000">>));
	CallID -> put(callid, CallID)
    end.
