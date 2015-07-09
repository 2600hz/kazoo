%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cb_jobs_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,publish_new_job/1
         ,handle_job/2
         ,start_recovery/0
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../crossbar.hrl").

-record(state, {recovery_ref :: reference()}).

-define(APP_ROUTING, wh_util:to_binary(?MODULE)).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resources">>).
-define(RECOVERY_TIMEOUT_S
        ,whapps_config:get_integer(?MOD_CONFIG_CAT, <<"job_recovery_timeout_s">>, ?SECONDS_IN_HOUR * 6)
       ).
-define(RECOVERY_MESSAGE, 'start_recovery').
-define(RECOVERY_THRESHOLD_S
        ,whapps_config:get_integer(?MOD_CONFIG_CAT, <<"job_recovery_threshold_s">>, ?SECONDS_IN_HOUR)
       ).

-define(KEY_SUCCESS, <<"success">>).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'delegate', [{'app_name', ?APP_ROUTING}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_job'}
                      ,{<<"delegate">>, <<"job">>}
                     }
                    ]).
-define(QUEUE_NAME, <<"resource_jobs_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[]
                           ).

-spec publish_new_job(cb_context:context()) -> 'ok'.
publish_new_job(Context) ->
    AccountId = cb_context:account_id(Context),
    JobId = wh_doc:id(cb_context:doc(Context)),
    ReqId = cb_context:req_id(Context),

    publish(AccountId, JobId, ReqId).

-spec publish(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish(AccountId, JobId, ReqId) ->
    Work = wh_json:from_list([{<<"Account-ID">>, AccountId}
                              ,{<<"Job-ID">>, JobId}
                             ]),
    wh_amqp_worker:cast([{<<"Delegate-Message">>, Work}
                         ,{<<"Msg-ID">>, ReqId}
                         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                        ]
                        ,fun(API) -> wapi_delegate:publish_delegate(?APP_ROUTING, API) end
                       ).

-spec handle_job(wh_json:object(), wh_proplist()) -> 'ok'.
handle_job(JObj, _Props) ->
    'true' = wapi_delegate:delegate_v(JObj),
    wh_util:put_callid(wh_json:get_first_defined([<<"Msg-ID">>, [<<"Delegate-Message">>, <<"Job-ID">>]], JObj)),
    Job = wh_json:get_value(<<"Delegate-Message">>, JObj),
    process_job(wh_json:get_value(<<"Account-ID">>, Job)
                ,wh_json:get_value(<<"Job-ID">>, Job)
               ).

-spec process_job(ne_binary(), ne_binary()) -> 'ok'.
process_job(<<_/binary>> = AccountId, <<_/binary>> = JobId) ->
    JobModb = job_modb(AccountId, JobId),
    {'ok', Job} = couch_mgr:open_cache_doc(JobModb, JobId),
    lager:debug("processing job ~s for account ~s", [JobId, AccountId]),
    maybe_start_job(Job, wh_json:get_value(<<"pvt_status">>, Job)).

-spec maybe_start_job(wh_json:object(), ne_binary()) -> 'ok'.
maybe_start_job(_Job, <<"complete">>) ->
    lager:debug("job is complete, nothing to do");
maybe_start_job(Job, <<"pending">>) ->
    lager:debug("job is pending, let's execute it!"),
    start_job(update_status(Job, <<"running">>)
              ,wh_doc:account_id(Job)
              ,wh_json:get_value(<<"pvt_auth_account_id">>, Job)
              ,select_carrier_module(Job)
              ,wh_json:get_value(<<"numbers">>, Job)
             );
maybe_start_job(Job, <<"running">>) ->
    lager:debug("job is running, ~s is in charge", [wh_json:get_value(<<"pvt_node">>, Job)]).

-spec start_job(wh_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
start_job(Job, _AccountId, _AuthAccountId, _CarrierModule, []) ->
    update_status(Job, <<"complete">>),
    lager:debug("successfully finished job");
start_job(Job, AccountId, AuthAccountId, CarrierModule, [Number|Numbers]) ->
    Job1 = maybe_create_number(Job, AccountId, AuthAccountId, CarrierModule, Number),
    start_job(Job1, AccountId, AuthAccountId, CarrierModule, Numbers).

-spec select_carrier_module(wh_json:object()) -> ne_binary().
select_carrier_module(Job) ->
    ResourceId = wh_json:get_value(<<"resource_id">>, Job),
    case couch_mgr:open_cache_doc(?WH_OFFNET_DB, ResourceId) of
        {'ok', _} ->
            lager:debug("found resource ~s in ~s, using wnm_other", [ResourceId, ?WH_OFFNET_DB]),
            <<"wnm_other">>;
        {'error', _E} ->
            lager:debug("resource ~s is not a system resource(~p), using wnm_local", [ResourceId, _E]),
            <<"wnm_local">>
    end.

-spec maybe_create_number(wh_json:object(), ne_binary(), ne_binary(), api_binary(), ne_binary()) ->
                                 wh_json:object().
maybe_create_number(Job, AccountId, AuthAccountId, CarrierModule, Number) ->
    case wh_json:get_first_defined([[?KEY_SUCCESS, Number]
                                    ,[<<"errors">>, Number]
                                   ], Job)
    of
        'undefined' -> create_number(Job, AccountId, AuthAccountId, CarrierModule, Number);
        _JObj ->
            lager:debug("number ~s already processed in this job", [Number]),
            Job
    end.

-spec create_number(wh_json:object(), ne_binary(), ne_binary(), api_binary(), ne_binary()) ->
                                 wh_json:object().
create_number(Job, AccountId, AuthAccountId, CarrierModule, Number) ->
    try wh_number_manager:create_number(Number
                                        ,AccountId
                                        ,AuthAccountId
                                        ,build_number_properties(Job)
                                        ,'false'
                                        ,CarrierModule
                                       )
    of
        {'ok', NumberJObj} ->
            lager:debug("successfully created number ~s for account ~s", [Number, AccountId]),
            update_status(wh_json:set_value([?KEY_SUCCESS, Number], NumberJObj, Job)
                          ,<<"running">>
                         );
        {Failure, JObj} ->
            update_with_failure(Job, AccountId, Number, Failure, JObj)
    catch
        E:_R ->
            lager:debug("exception creating number ~s for account ~s: ~s: ~p", [Number, AccountId, E, _R]),
            update_status(wh_json:set_value([<<"errors">>, Number]
                                            ,wh_json:from_list([{<<"reason">>, wh_util:to_binary(E)}
                                                               ])
                                            ,Job
                                           )
                          ,<<"running">>
                         )
    end.

-spec update_with_failure(wh_json:object(), ne_binary(), ne_binary(), atom(), wh_json:object()) ->
                                 wh_json:object().
update_with_failure(Job, AccountId, Number, Failure, JObj) ->
    lager:debug("failed to create number ~s for account ~s: ~p ~p", [Number, AccountId, Failure, JObj]),
    case wh_json:is_json_object(JObj)
        andalso wh_json:get_values(JObj)
    of
        {[V], [K]} ->
            update_status(wh_json:set_value([<<"errors">>, Number]
                                            ,wh_json:from_list([{<<"reason">>, K}
                                                                ,{<<"message">>, V}
                                                               ])
                                            ,Job
                                           )
                          ,<<"running">>
                         );
        _ ->
            update_status(wh_json:set_value([<<"errors">>, Number], JObj, Job)
                          ,<<"running">>
                         )
    end.

-spec build_number_properties(wh_json:object()) -> wh_json:object().
build_number_properties(JObj) ->
    wh_json:from_list([{<<"resource_id">>, wh_json:get_value(<<"resource_id">>, JObj)}]).

-spec update_status(wh_json:object(), ne_binary()) -> wh_json:object().
update_status(Job, Status) ->
    {'ok', Job1} = couch_mgr:save_doc(wh_doc:account_db(Job)
                                      ,wh_json:set_values([{<<"pvt_status">>, Status}
                                                           ,{<<"pvt_node">>, wh_util:to_binary(node())}
                                                          ]
                                                          ,wh_doc:update_pvt_modified(Job)
                                                         )
                                     ),
    Job1.

-spec start_recovery() -> 'ok'.
start_recovery() ->
    wh_util:put_callid(?MODULE),
    {Year, Month, _} = erlang:date(),
    maybe_recover_jobs(Year, Month, whapps_util:get_all_accounts('raw')).

-spec maybe_recover_jobs(wh_year(), wh_month(), ne_binaries()) -> 'ok'.
maybe_recover_jobs(Year, Month, Accounts) ->
    _ = [catch maybe_recover_account_jobs(Year, Month, AccountId) || AccountId <- Accounts],
    lager:debug("finished recovering account jobs").

-spec maybe_recover_account_jobs(wh_year(), wh_month(), ne_binary()) -> 'ok'.
maybe_recover_account_jobs(Year, Month, AccountId) ->
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    case couch_mgr:get_results(Modb, <<"resources/status_listing">>, [{'keys', [<<"pending">>
                                                                                ,<<"running">>
                                                                               ]}
                                                                      ,'include_docs'
                                                                     ])
    of
        {'ok', []} -> 'ok';
        {'ok', IncompleteJobs} ->
            maybe_recover_incomplete_jobs(IncompleteJobs);
        {'error', _} -> 'ok'
    end.

-spec maybe_recover_incomplete_jobs(wh_json:objects()) -> 'ok'.
maybe_recover_incomplete_jobs(IncompleteJobs) ->
    _ = [maybe_recover_incomplete_job(wh_json:get_value(<<"doc">>, Job)) || Job <- IncompleteJobs],
    'ok'.

-spec maybe_recover_incomplete_job(wh_json:object()) -> 'ok'.
maybe_recover_incomplete_job(Job) ->
    Now = wh_util:current_tstamp(),
    case (Now - wh_doc:modified(Job)) > ?RECOVERY_THRESHOLD_S of
        'false' -> 'ok';
        'true' ->
            lager:debug("job ~s in ~s is old and incomplete, attempting to restart it"
                        ,[wh_doc:id(Job), wh_doc:account_id(Job)]
                       ),
            recover_incomplete_job(Job)
    end.

-spec recover_incomplete_job(wh_json:object()) -> 'ok'.
recover_incomplete_job(Job) ->
    Db = wh_doc:account_db(Job),
    case couch_mgr:save_doc(Db
                            ,wh_json:set_value(<<"pvt_recovering">>, 'true', wh_doc:update_pvt_modified(Job))
                           )
    of
        {'ok', Job1} -> republish_job(Job1);
        {'error', 'conflict'} -> lager:debug("someone updated the job while we were trying");
        {'error', _} -> 'ok'
    end.

-spec republish_job(wh_json:object()) -> 'ok'.
republish_job(Job) ->
    JobId = wh_doc:id(Job),
    ReqId = wh_json:get_value(<<"pvt_request_id">>, Job, wh_util:to_binary(?MODULE)),
    publish(wh_doc:account_id(Job), JobId, ReqId).

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
    {'ok', #state{recovery_ref=start_timer()}}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

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
handle_info({'timeout', Ref, ?RECOVERY_MESSAGE}
            ,#state{recovery_ref=Ref}=State
           ) ->
    _Pid = wh_util:spawn(?MODULE, 'start_recovery', []),
    lager:debug("starting recovery walker in ~p", [_Pid]),
    {'noreply', State#state{recovery_ref=start_timer()}, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

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
-spec job_modb(ne_binary(), ne_binary()) -> ne_binary().
job_modb(AccountId, <<Year:4/binary, Month:2/binary, "-", _/binary>>) ->
    wh_util:format_account_mod_id(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month));
job_modb(AccountId, <<Year:4/binary, Month:1/binary, "-", _/binary>>) ->
    wh_util:format_account_mod_id(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)).

-spec start_timer() -> reference().
start_timer() ->
    Time = ?RECOVERY_TIMEOUT_S * ?MILLISECONDS_IN_SECOND,
    erlang:start_timer(Time, self(), ?RECOVERY_MESSAGE).
