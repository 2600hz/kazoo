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

-record(state, {workers = [] :: pids()}).
-type pool_mgr_state() :: #state{}.
-type authn_response() :: {'ok', 'aaa_mode_off'} | {'ok', tuple()}.

-define(CM_TIMEOUT, 5000).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,start_link/0
         ,send_authn_response/4
         ,send_authn_error/4
         ,do_request/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    lager:debug(""),
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Makes an AAA request
%% @end
%%--------------------------------------------------------------------
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handler for a response message for AuthN
%% @end
%%--------------------------------------------------------------------
-spec send_authn_response(pid(), authn_response(), wh_json:object(), pid()) -> 'ok'.
send_authn_response(SenderPid, Response, JObj, Self) ->
    gen_server:cast(SenderPid, {'response', Response, JObj, Self}).

%%--------------------------------------------------------------------
%% @doc
%% Handler for an error message for AuthN
%% @end
%%--------------------------------------------------------------------
-spec send_authn_error(pid(), {'error', 'no_respond'}, wh_json:object(), pid()) -> 'ok'.
send_authn_error(SenderPid, Reason, JObj, Self) ->
    gen_server:cast(SenderPid, {'error', Reason, JObj, Self}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', tuple()}.
init([]) ->
    lager:debug(""),
    {'ok', #state{}}.

handle_call(Request, _From, State) ->
    lager:debug("Request=~p~nState=~p~n", [Request, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

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
handle_cast({'request', JObj}, State) ->
    lager:debug("requst message is ~p", [JObj]),
    {'noreply', dist_workers(JObj, State), ?CM_TIMEOUT};
handle_cast({'response', Response, JObj, Worker}, State) ->
    lager:debug("response is ~p", [Response]),
    poolboy:checkin(?WORKER_POOL, Worker),
    Result = case Response of
                 {'ok', {'ok', {'radius_request', _, 'accept', _, _, _, _, _}}} ->
                     <<"accept">>;
                 {'ok', {'ok', {'radius_request', _, 'reject', _, _, _, _, _}}} ->
                     <<"reject">>;
                 _ ->
                     <<"error">>
             end,
    Queue = wh_json:get_value(<<"Response-Queue">>, JObj),
    Password = wh_json:get_value(<<"User-Password">>, JObj),
    JObj1 = wh_json:set_values([{<<"AAA-Result">>, Result},
                                {<<"Auth-Password">>, Password},
                                {<<"Event-Name">>,<<"aaa_authn_resp">>}],
                                JObj),
    % send response to the registrar_listener queue
    wapi_aaa:publish_resp(Queue, JObj1),
    {'noreply', remove_worker(Worker, State), ?CM_TIMEOUT};
handle_cast({'error', Response, _JObj, Worker}, State) ->
    lager:debug("error is ~p", [Response]),
    poolboy:checkin(?WORKER_POOL, Worker),
    {'noreply', remove_worker(Worker, State), ?CM_TIMEOUT};
handle_cast(Message, State) ->
    lager:debug("Message=~p~nState=~p~n", [Message, State]),
    {'noreply', State, ?CM_TIMEOUT}.

handle_info(Info, State) ->
    lager:debug("Info=~p~nState=~p~n", [Info, State]),
    {'reply', {'error', 'not_implemented'}, State, ?CM_TIMEOUT}.

terminate(Reason, _State) ->
    lager:debug("Circlemaker worker terminating: ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts new worker for processing a request
%% @end
%%--------------------------------------------------------------------
-spec dist_workers(wh_json:object(), pool_mgr_state()) -> pool_mgr_state().
dist_workers(JObj, State) ->
    lager:debug("Trying to start new worker..."),
    case catch poolboy:checkout(?WORKER_POOL) of
        Worker when is_pid(Worker) ->
            lager:debug("Worker started sucessfully"),
            gen_server:cast(Worker, {'authn_req', self(), JObj}),
            insert_worker(Worker, State);
        _Else ->
            lager:error("Failed to start a worker"),
            State
    end.
