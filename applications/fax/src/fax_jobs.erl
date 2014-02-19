%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(fax_jobs).

-behaviour(gen_server).

-export([start_link/0]).
-export([cleanup_jobs/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("fax.hrl").

-define(POLLING_INTERVAL, 5000).

-record(state, {jobs=[]}).

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
    spawn(?MODULE, 'cleanup_jobs', []),
    MaxTime = whapps_config:get_integer(?CONFIG_CAT, <<"job_timeout">>, 180000),
    _ = erlang:send_after(MaxTime, self(), 'expire_jobs'),
    {'ok', #state{}, 0}.

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
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

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
handle_cast({'job_complete', Worker}, #state{jobs=Jobs}=State) ->
    poolboy:checkin('fax_worker_pool', Worker),
    {'noreply', State#state{jobs=distribute_jobs(Jobs)}, ?POLLING_INTERVAL};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
handle_info('expire_jobs', State) ->
    MaxTime = whapps_config:get_integer(?CONFIG_CAT, <<"job_timeout">>, 180000),
    ViewOptions = [{<<"key">>, wh_util:to_binary(node())}],
    case couch_mgr:get_results(?WH_FAXES, <<"faxes/processing_by_node">>, ViewOptions) of
        {'ok', JObjs} ->
            [begin
                 DocId = wh_json:get_value(<<"id">>, JObj),
                 lager:debug("moving expired job ~s status to pending", [DocId]),
                 couch_mgr:update_doc(?WH_FAXES, DocId, [{<<"pvt_job_status">>, <<"pending">>}])
             end
             || JObj <- JObjs
                    ,(wh_util:current_tstamp() - wh_json:get_integer_value([<<"value">>,<<"modified">>], JObj, 0)) > (MaxTime div 1000)
            ],
            'ok';
        {'error', _R} ->
            lager:debug("unable to expire jobs: ~p", [_R]),
            'ok'
    end,
    _ = erlang:send_after(MaxTime, self(), 'expire_jobs'),
    {'noreply', State, ?POLLING_INTERVAL};
handle_info('timeout', #state{jobs=[]}=State) ->
    ViewOptions = [{'limit', 100}],
    case couch_mgr:get_results(?WH_FAXES, <<"faxes/jobs">>, ViewOptions) of
        {'ok', []} -> {'noreply', State, ?POLLING_INTERVAL};
        {'ok', Jobs} ->
            lager:debug("fetched ~b jobs, attempting to distribute to workers", [length(Jobs)]),
            {'noreply', State#state{jobs=distribute_jobs(Jobs)}, ?POLLING_INTERVAL};
        {'error', _Reason} ->
            lager:debug("failed to fetch fax jobs: ~p", [_Reason]),
            {'noreply', State, ?POLLING_INTERVAL}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
    lager:debug("fax jobs terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec distribute_jobs(wh_json:objects()) -> wh_json:objects().
distribute_jobs([]) -> [];
distribute_jobs([Job|Jobs]) ->
    case catch poolboy:checkout('fax_worker_pool', 'false', 1000) of
        Worker when is_pid(Worker) ->
            gen_server:cast(Worker, {'attempt_transmission', self(), Job}),
            distribute_jobs(Jobs);
        _Else -> Jobs
    end.

-spec cleanup_jobs() -> 'ok'.
cleanup_jobs() ->
    ViewOptions = [{<<"key">>, wh_util:to_binary(node())}],
    case couch_mgr:get_results(?WH_FAXES, <<"faxes/processing_by_node">>, ViewOptions) of
        {'ok', JObjs} ->
            [begin
                 DocId = wh_json:get_value(<<"id">>, JObj),
                 lager:debug("moving zombie job ~s status to pending", [DocId]),
                 couch_mgr:update_doc(?WH_FAXES, DocId, [{<<"pvt_job_status">>, <<"pending">>}])
             end
             || JObj <- JObjs
            ],
            'ok';
        {'error', _R} -> lager:debug("unable to cleanup jobs: ~p", [_R])
    end.
