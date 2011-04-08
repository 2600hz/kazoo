%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Manage a pool of amqp queues
%%% @end
%%% Created : 28 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_amqp_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, route_req/1, route_req/2, reg_query/1, reg_query/2, media_req/1, media_req/2]).
-export([auth_req/1, auth_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(WORKER_COUNT, 10).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {
	  worker_count = ?WORKER_COUNT :: integer()
          ,orig_worker_count = ?WORKER_COUNT :: integer() % scale back workers after a period of time
          ,workers = queue:new() :: queue()
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

auth_req(Prop) ->
    auth_req(Prop, ?DEFAULT_TIMEOUT).
auth_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun whistle_api:auth_req/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_AUTH_REQ) end
			      }, Timeout).

route_req(Prop) ->
    route_req(Prop, ?DEFAULT_TIMEOUT).
route_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun whistle_api:route_req/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_ROUTE_REQ) end
			     }, Timeout).

reg_query(Prop) ->
    reg_query(Prop, ?DEFAULT_TIMEOUT).

reg_query(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun whistle_api:reg_query/1
			      ,fun(JSON) -> amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_REG_QUERY) end
			     }, Timeout).

media_req(Prop) ->
    media_req(Prop, ?DEFAULT_TIMEOUT).

media_req(Prop, Timeout) ->
    gen_server:call(?SERVER, {request, Prop, fun whistle_api:media_req/1
			      ,fun(JSON) -> amqp_util:callevt_publish(JSON, media) end
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
handle_call({request, Prop, ApiFun, PubFun}, From, #state{workers=W}=State) ->
    case queue:out(W) of
	{{value, Worker}, W1} ->
	    Worker ! {request, Prop, ApiFun, PubFun, From, self()},
	    {noreply, State#state{workers=W1}};
	{empty, _} ->
	    Worker = start_worker(),
	    Worker ! {request, Prop, ApiFun, PubFun, From, self()},
	    {noreply, State#state{worker_count=State#state.worker_count + 1}}
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
handle_info(timeout, #state{worker_count=WC, workers=Ws}=State) ->
    Count = case WC-queue:len(Ws) of X when X < 0 -> 0; Y -> Y end,
    Ws1 = lists:foldr(fun(W, Ws0) -> queue:in(W, Ws0) end, Ws, [ start_worker() || _ <- lists:seq(1, Count) ]),
    {noreply, State#state{workers=Ws1, worker_count=queue:len(Ws1)}};
handle_info({worker_free, W}, #state{workers=Ws}=State) ->
    {noreply, State#state{workers=queue:in(W, Ws)}};
handle_info({'EXIT', W, Reason}, #state{workers=Ws}=State) ->
    Ws1 = queue:filter(fun(W1) when W =:= W1 -> false; (_) -> true end, Ws),
    logger:format_log(info, "CALL_POOL(~p): Worker ~p down: ~p~n", [self(), W, Reason]),
    {noreply, State#state{workers=queue:in(start_worker(), Ws1)}};
handle_info(_Info, State) ->
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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_worker() ->
    spawn_link(fun() -> worker_init() end).

worker_init() ->
    Q = amqp_util:new_targeted_queue(),
    amqp_util:bind_q_to_targeted(Q),
    amqp_util:basic_consume(Q),
    logger:format_log(info, "WORKER(~p): Listening to ~p~n", [self(), Q]),
    worker_free(Q).

worker_free(Q) ->
    receive
	{request, Prop, ApiFun, PubFun, {Pid, _}=From, Parent} ->
	    Prop1 = [ {<<"Server-ID">>, Q} | lists:keydelete(<<"Server-ID">>, 1, Prop)],
	    case ApiFun(Prop1) of
		{ok, JSON} ->
		    Ref = erlang:monitor(process, Pid),
		    PubFun(JSON),
		    logger:format_log(info, "WORKER(~p): Working for ~p~n", [self(), Pid]),
		    worker_busy(Q, From, Ref, Parent);
		{error, _}=E ->
		    gen_server:reply(From, E),
		    worker_free(Q)
	    end;
	#'basic.consume_ok'{} ->
	    worker_free(Q);
	shutdown ->
	    logger:format_log(info, "WORKER(~p): Going on permanent leave~n", [self()]);
	_Other ->
	    logger:format_log(info, "WORKER(~p): Recv other ~p~n", [self(), _Other]),
	    worker_free(Q)
    end.

worker_busy(Q, From, Ref, Parent) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload = Payload}} ->
	    logger:format_log(info, "WORKER(~p): Recv payload response (~p ms)~n", [self(), timer:now_diff(erlang:now(), Start) div 1000]),
	    gen_server:reply(From, {ok, mochijson2:decode(Payload)});
	{'DOWN', Ref, process, Pid, Info} ->
	    logger:format_log(error, "WORKER(~p): Requestor(~p) down: ~p~n", [self(), Pid, Info])
    end,
    erlang:demonitor(Ref, [flush]),
    Parent ! {worker_free, self()},
    worker_free(Q).
