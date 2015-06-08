%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is worker pool manager. It is used to create workers for a requests from cm_listener.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_pool_mgr).
-behaviour(gen_server).

-include("circlemaker.hrl").

-record(state, {workers =[]}).

% TODO: should be part of timeout
-define(CM_TIMEOUT, 5000).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0, do_request/1]).

-spec start_link() -> startlink_ret().
start_link() ->
    lager:debug([{'trace', 'true'}], "", []),
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec do_request(api_object()) -> 'ok'.
do_request(Request) ->
    lager:debug([{'trace', 'true'}], "Request=~p~n", [Request]),
    gen_server:cast(?MODULE, {'request', Request}).

%% Internal API

-spec init([]) -> {'ok', tuple()}.
init([]) ->
    lager:debug([{'trace', 'true'}], "", []),
    {'ok', #state{}}.

handle_call(Request, _From, State) ->
    lager:debug([{'trace', 'true'}], "Request=~p~nState=~p~n", [Request, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

handle_cast({'request', JObj}, State) ->
    lager:debug([{'trace', 'true'}], "JObj:~n~p~nState:~n~p~n", [JObj, State]),
    {'noreply', State#state{workers=dist_workers(JObj, State#state.workers)}, ?CM_TIMEOUT};
handle_cast({'response', Response, Worker}, State) ->
    lager:debug([{'trace', 'true'}], "Response=~p~nWorker=~p~nState=~p~n", [Response, Worker, State]),
    poolboy:checkin(?WORKER_POOL, Worker),
    % send response to queue, which is aaa_listener of the registrar app
    Queue = props:get_value(<<"Response-Queue">>, Response),
    wapi_aaa:publish_resp(Queue, Response),
    {'noreply', State#state{workers=[W || W <- State#state.workers, Worker =/= W]}, ?CM_TIMEOUT};
handle_cast({'error', Response, Worker}, State) ->
    lager:debug([{'trace', 'true'}], "Response=~p~nWorker=~p~nState=~p~n", [Response, Worker, State]),
    poolboy:checkin(?WORKER_POOL, Worker),
    {'noreply', State#state{workers=[W || W <- State#state.workers, Worker =/= W]}, ?CM_TIMEOUT};
handle_cast(Message, State) ->
    lager:debug([{'trace', 'true'}], "Message=~p~nState=~p~n", [Message, State]),
    {'noreply', State, ?CM_TIMEOUT}.

dist_workers(JObj, Workers) ->
    lager:debug([{'trace', 'true'}], "JObj=~p~nWorkers=~p~n", [JObj, Workers]),
    case catch poolboy:checkout(?WORKER_POOL) of
        Worker when is_pid(Worker) ->
            gen_server:cast(Worker, {'authn_req', self(), JObj}),
            [Worker | Workers];
        _Else -> Workers
    end.

handle_info(Info, State) ->
    lager:debug([{'trace', 'true'}], "Info=~p~nState=~p~n", [Info, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

terminate(Reason, _State) ->
    lager:debug("Circlemaker worker terminating: ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
