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

-export([worker_free/3, worker_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(WORKER_COUNT, 10).
-define(DEFAULT_TIMEOUT, 5000).

%% every X ms, compare RequestsPer to WorkerCount
%% If RP < WC, reduce Ws by max(WC-RP, OrigWC)
-define(BACKOFF_PERIOD, 1000). % arbitrary at this point

-record(state, {
	  worker_count = ?WORKER_COUNT :: integer()
          ,orig_worker_count = ?WORKER_COUNT :: integer() % scale back workers after a period of time
          ,workers = queue:new() :: queue()
          ,requests_per = 0 :: non_neg_integer()
	  ,elapsed_micro_per = 0 :: non_neg_integer()
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

worker_free(Srv, Worker, Elapsed) ->
    gen_server:cast(Srv, {worker_free, Worker, Elapsed}).

worker_count() ->
    ecallmgr_amqp_pool_worker_sup:worker_count().

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

    'ok' = ecallmgr_amqp_pool_worker_sup:release_all(),

    erlang:send_after(?BACKOFF_PERIOD, self(), reduce_labor_force),

    Ws = lists:foldr(fun(W, Ws0) -> queue:in(W, Ws0) end, queue:new(), [ start_worker() || _ <- lists:seq(1, Count) ]),

    {ok, #state{worker_count=Count, workers=Ws, orig_worker_count=Count}}.

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
handle_call({request, Prop, ApiFun, PubFun}, From, #state{workers=W, worker_count=WC, requests_per=RP}=State) ->
    case queue:out(W) of
	{{value, Worker}, W1} ->
	    ecallmgr_amqp_pool_worker:start_req(Worker, Prop, ApiFun, PubFun, From, self()),
	    {noreply, State#state{workers=W1, requests_per=RP+1}, hibernate};
	{empty, _} ->
	    Worker = start_worker(),
	    ?LOG("starting additional worker ~p", [Worker]),
	    ecallmgr_amqp_pool_worker:start_req(Worker, Prop, ApiFun, PubFun, From, self()),
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
handle_cast({worker_free, _Worker, _Elapsed}=Req, State) ->
    handle_info(Req, State).

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
handle_info({worker_free, W, Elapsed}, #state{workers=Ws, elapsed_micro_per=EMP}=State) ->
    {noreply, State#state{workers=queue:in(W, Ws), elapsed_micro_per=EMP+Elapsed}, hibernate};

handle_info({'EXIT', W, _Reason}, #state{workers=Ws, worker_count=WC, orig_worker_count=OWC}=State) when WC < OWC ->
    ?LOG("Worker down: ~p", [_Reason]),
    Ws1 = queue:in(start_worker(), queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws)),
    {noreply, State#state{workers=Ws1, worker_count=worker_count()}, hibernate};

handle_info({'EXIT', W, _Reason}, #state{workers=Ws}=State) ->
    ?LOG("Worker down: ~p", [_Reason]),
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),

    {noreply, State#state{workers=Ws1, worker_count=worker_count()}, hibernate};

handle_info(reduce_labor_force
	    ,#state{workers=Ws, worker_count=WC, requests_per=RP, orig_worker_count=OWC, elapsed_micro_per=EMP}=State)
  when RP > 0 andalso EMP > 0 andalso WC > OWC ->
    AvgMicro = EMP div RP, % average micro per request
    ?LOG("Req per ~b: ~b", [?BACKOFF_PERIOD, RP]),
    ?LOG("Avg micro per req: ~b (~b total micro)", [AvgMicro, EMP]),
    WsNeeded = round((1 / AvgMicro) * (?BACKOFF_PERIOD * 1000)), % avg workers needed
    ?LOG("WsNeeded: ~b (have ~b)", [WsNeeded, WC]),

    erlang:send_after(?BACKOFF_PERIOD, self(), reduce_labor_force),

    case round((WC - WsNeeded) * 0.1) of
	Reduce when Reduce > 0 ->
	    ?LOG_SYS("Reducing worker count from ~b by ~b", [WC, Reduce]),
	    Ws1 = reduce_workers(Ws, Reduce, OWC),
	    {noreply, State#state{workers=Ws1, worker_count=worker_count(), requests_per=0, elapsed_micro_per=0}, hibernate};
	_Other ->
	    ?LOG_SYS("Not reducing workers (~b suggested)", [_Other]),
	    {noreply, State#state{requests_per=0, elapsed_micro_per=0}}
    end;

handle_info(reduce_labor_force, #state{requests_per=RP, worker_count=WC, orig_worker_count=OWC, workers=Ws}=State) ->
    erlang:send_after(?BACKOFF_PERIOD, self(), reduce_labor_force),

    case round((WC - RP) * 0.1) of
	Reduce when Reduce > 0 andalso WC > OWC ->
	    ?LOG("Reducing worker count from ~b by ~b", [WC, Reduce]),
	    Ws1 = reduce_workers(Ws, Reduce, OWC),
	    ?LOG("Queue len before ~b and after ~b", [queue:len(Ws), queue:len(Ws1)]),
	    {noreply, State#state{requests_per=0, elapsed_micro_per=0, workers=Ws1, worker_count=worker_count()}, hibernate};
	_Else ->
	    {noreply, State#state{requests_per=0, elapsed_micro_per=0}, hibernate}
    end;

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
    {noreply, State}.

reduce_workers(Ws, Reduce, OWC) ->
    lists:foldl(fun(_, Q0) ->
			case queue:len(Q0) =< OWC of
			    true -> Q0;
			    false ->
				{{value, W}, Q1} = queue:out(Q0),
				ecallmgr_amqp_pool_worker:stop(W),
				Q1
			end
		end, Ws, lists:seq(1,Reduce)).

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
    {ok, Pid} = ecallmgr_amqp_pool_worker_sup:start_child(),
    link(Pid),
    ?LOG("Worker ~p started", [Pid]),
    Pid.
