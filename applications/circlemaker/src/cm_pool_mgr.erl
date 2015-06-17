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

-record(state, {workers = [] :: list(pid())}).
-type pool_mgr_state() :: #state{}.

% TODO: should be part of timeout
-define(CM_TIMEOUT, 5000).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0, do_request/1]).

-spec start_link() -> startlink_ret().
start_link() ->
    lager:debug(""),
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec do_request(wh_json:object()) -> 'ok'.
do_request(Request) ->
    lager:debug("Request=~p~n"),
    gen_server:cast(?MODULE, {'request', Request}).

%%--------------------------------------------------------------------
%% @doc
%% Stores a new worker process into the Pool Manager's state
%% @end
%%--------------------------------------------------------------------
-spec insert_worker(pid(), pool_mgr_state()) -> pool_mgr_state().
insert_worker(Worker, State) ->
    State#state{workers=[Worker | State#state.workers]}.

%%--------------------------------------------------------------------
%% @doc
%% Removes a new worker process from the Pool Manager's state
%% @end
%%--------------------------------------------------------------------
-spec remove_worker(pid(), pool_mgr_state()) -> pool_mgr_state().
remove_worker(Worker, State) ->
    State#state{workers=lists:delete(Worker, State#state.workers)}.

%% Internal API

-spec init([]) -> {'ok', tuple()}.
init([]) ->
    lager:debug(""),
    {'ok', #state{}}.

handle_call(Request, _From, State) ->
    lager:debug("Request=~p~nState=~p~n", [Request, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

handle_cast({'request', JObj}, State) ->
    lager:debug("JObj:~n~p~nState:~n~p~n", [JObj, State]),
    {'noreply', dist_workers(JObj, State), ?CM_TIMEOUT};
handle_cast({'response', Response, Worker}, State) ->
    lager:debug("Response=~p~nWorker=~p~nState=~p~n", [Response, Worker, State]),
    poolboy:checkin(?WORKER_POOL, Worker),
    % send response to queue, which is aaa_listener of the registrar app
    Queue = props:get_value(<<"Response-Queue">>, Response),
    wapi_aaa:publish_resp(Queue, Response),
    {'noreply', remove_worker(Worker, State), ?CM_TIMEOUT};
handle_cast({'error', Response, Worker}, State) ->
    lager:debug("Response=~p~nWorker=~p~nState=~p~n", [Response, Worker, State]),
    poolboy:checkin(?WORKER_POOL, Worker),
    {'noreply', remove_worker(Worker, State), ?CM_TIMEOUT};
handle_cast(Message, State) ->
    lager:debug("Message=~p~nState=~p~n", [Message, State]),
    {'noreply', State, ?CM_TIMEOUT}.

%%--------------------------------------------------------------------
%% @doc
%% Starts new worker for processing a request
%% @end
%%--------------------------------------------------------------------
-spec dist_workers(wh_json:object(), pool_mgr_state()) -> pool_mgr_state().
dist_workers(JObj, State) ->
    lager:debug("JObj=~p~nWorkers=~p~n", [JObj, State]),
    case catch poolboy:checkout(?WORKER_POOL) of
        Worker when is_pid(Worker) ->
            cm_worker:handle_authn(Worker, JObj, self()),
            insert_worker(Worker, State);
        _Else -> State
    end.

handle_info(Info, State) ->
    lager:debug("Info=~p~nState=~p~n", [Info, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

terminate(Reason, _State) ->
    lager:debug("Circlemaker worker terminating: ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
