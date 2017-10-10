%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
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

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(SERVER, ?MODULE).

-record(state, {recovery_ref :: reference()
               }).
-type state() :: #state{}.

-define(APP_ROUTING, kz_term:to_binary(?MODULE)).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resources">>).
-define(RECOVERY_TIMEOUT_S
       ,kapps_config:get_integer(?MOD_CONFIG_CAT, <<"job_recovery_timeout_s">>, ?SECONDS_IN_HOUR * 6)
       ).
-define(RECOVERY_MESSAGE, 'start_recovery').
-define(RECOVERY_THRESHOLD_S
       ,kapps_config:get_integer(?MOD_CONFIG_CAT, <<"job_recovery_threshold_s">>, ?SECONDS_IN_HOUR)
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
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
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
    JobId = kz_doc:id(cb_context:doc(Context)),
    ReqId = cb_context:req_id(Context),
    publish(AccountId, JobId, ReqId).

-spec publish(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish(AccountId, JobId, ReqId) ->
    Work = kz_json:from_list([{<<"Account-ID">>, AccountId}
                             ,{<<"Job-ID">>, JobId}
                             ]),
    kz_amqp_worker:cast([{<<"Delegate-Message">>, Work}
                        ,{<<"Msg-ID">>, ReqId}
                         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                        ]
                       ,fun(API) -> kapi_delegate:publish_delegate(?APP_ROUTING, API) end
                       ).

-spec handle_job(kz_json:object(), kz_proplist()) -> 'ok'.
handle_job(JObj, _Props) ->
    'true' = kapi_delegate:delegate_v(JObj),
    kz_util:put_callid(kz_json:get_first_defined([<<"Msg-ID">>, [<<"Delegate-Message">>, <<"Job-ID">>]], JObj)),
    Job = kz_json:get_value(<<"Delegate-Message">>, JObj),
    process_job(kz_json:get_value(<<"Account-ID">>, Job)
               ,kz_json:get_value(<<"Job-ID">>, Job)
               ).

-spec process_job(ne_binary(), ne_binary()) -> 'ok'.
process_job(<<_/binary>> = AccountId, <<_/binary>> = JobId) ->
    JobModb = job_modb(AccountId, JobId),
    {'ok', Job} = kz_datamgr:open_cache_doc(JobModb, JobId),
    lager:debug("processing job ~s for account ~s", [JobId, AccountId]),
    maybe_start_job(Job, kz_json:get_value(<<"pvt_status">>, Job)).

-spec maybe_start_job(kz_json:object(), ne_binary()) -> 'ok'.
maybe_start_job(_Job, <<"complete">>) ->
    lager:debug("job is complete, nothing to do");
maybe_start_job(Job, <<"pending">>) ->
    lager:debug("job is pending, let's execute it!"),
    start_job(update_status(Job, <<"running">>)
             ,kz_doc:account_id(Job)
             ,kz_json:get_value(<<"pvt_auth_account_id">>, Job)
             ,select_carrier_module(Job)
             ,kz_json:get_value(<<"numbers">>, Job)
             );
maybe_start_job(Job, <<"running">>) ->
    lager:debug("job is running, ~s is in charge", [kz_json:get_value(<<"pvt_node">>, Job)]).

-spec start_job(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
start_job(Job, _AccountId, _AuthAccountId, _CarrierModule, []) ->
    update_status(Job, <<"complete">>),
    lager:debug("successfully finished job");
start_job(Job, AccountId, AuthAccountId, CarrierModule, [Number|Numbers]) ->
    Job1 = maybe_create_number(Job, AccountId, AuthAccountId, CarrierModule, Number),
    start_job(Job1, AccountId, AuthAccountId, CarrierModule, Numbers).

-spec select_carrier_module(kz_json:object()) -> ne_binary().
select_carrier_module(Job) ->
    ResourceId = kz_json:get_value(<<"resource_id">>, Job),
    case kz_datamgr:open_cache_doc(?KZ_OFFNET_DB, ResourceId) of
        {'ok', _} ->
            lager:debug("found resource ~s in ~s, using ~s"
                       ,[ResourceId, ?KZ_OFFNET_DB, ?CARRIER_OTHER]
                       ),
            ?CARRIER_OTHER;
        {'error', _E} ->
            lager:debug("resource ~s is not a system resource(~p), using ~s"
                       ,[ResourceId, _E, ?CARRIER_LOCAL]
                       ),
            ?CARRIER_LOCAL
    end.

-spec maybe_create_number(kz_json:object(), ne_binary(), ne_binary(), api_binary(), ne_binary()) ->
                                 kz_json:object().
maybe_create_number(Job, AccountId, AuthAccountId, CarrierModule, Number) ->
    case kz_json:get_first_defined([[?KEY_SUCCESS, Number]
                                   ,[<<"errors">>, Number]
                                   ], Job)
    of
        'undefined' -> create_number(Job, AccountId, AuthAccountId, CarrierModule, Number);
        _JObj ->
            lager:debug("number ~s already processed in this job", [Number]),
            Job
    end.

-spec create_number(kz_json:object(), ne_binary(), ne_binary(), api_binary(), ne_binary()) ->
                           kz_json:object().
create_number(Job, AccountId, AuthAccountId, CarrierModule, DID) ->
    Options = [{'assign_to', AccountId}
              ,{'auth_by', AuthAccountId}
              ,{'state', ?NUMBER_STATE_RESERVED}
              ,{'public_fields', build_number_properties(Job)}
              ,{'dry_run', 'false'}
              ,{'module_name', CarrierModule}
              ],
    try knm_number:create(DID, Options) of
        {'ok', Number} ->
            PhoneNumber = knm_number:phone_number(Number),
            lager:debug("successfully created number ~s for account ~s"
                       ,[knm_phone_number:number(PhoneNumber), AccountId]),
            update_status(kz_json:set_value([?KEY_SUCCESS, knm_phone_number:number(PhoneNumber)]
                                           ,knm_phone_number:to_public_json(PhoneNumber)
                                           ,Job
                                           )
                         ,<<"running">>
                         );
        {Failure, JObj} ->
            update_with_failure(Job, AccountId, DID, Failure, JObj)
    catch
        E:_R ->
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            lager:debug("exception creating number ~s for account ~s: ~s: ~p"
                       ,[DID, AccountId, E, _R]),
            update_status(kz_json:set_value([<<"errors">>, DID]
                                           ,kz_json:from_list([{<<"reason">>, kz_term:to_binary(E)}])
                                           ,Job
                                           )
                         ,<<"running">>
                         )
    end.

-spec update_with_failure(kz_json:object(), ne_binary(), ne_binary(), atom(), kz_json:object()) ->
                                 kz_json:object().
update_with_failure(Job, AccountId, Number, Failure, JObj) ->
    lager:debug("failed to create number ~s for account ~s: ~p ~p", [Number, AccountId, Failure, JObj]),
    case kz_json:is_json_object(JObj)
        andalso kz_json:get_values(JObj)
    of
        {[V], [K]} ->
            update_status(kz_json:set_value([<<"errors">>, Number]
                                           ,kz_json:from_list([{<<"reason">>, K}
                                                              ,{<<"message">>, V}
                                                              ])
                                           ,Job
                                           )
                         ,<<"running">>
                         );
        _ ->
            update_status(kz_json:set_value([<<"errors">>, Number], JObj, Job)
                         ,<<"running">>
                         )
    end.

-spec build_number_properties(kz_json:object()) -> kz_json:object().
build_number_properties(JObj) ->
    kz_json:from_list([{<<"resource_id">>, kz_json:get_value(<<"resource_id">>, JObj)}]).

-spec update_status(kz_json:object(), ne_binary()) -> kz_json:object().
update_status(Job, Status) ->
    {'ok', Job1} = kz_datamgr:save_doc(kz_doc:account_db(Job)
                                      ,kz_json:set_values([{<<"pvt_status">>, Status}
                                                          ,{<<"pvt_node">>, kz_term:to_binary(node())}
                                                          ]
                                                         ,kz_doc:update_pvt_modified(Job)
                                                         )
                                      ),
    Job1.

-spec start_recovery() -> 'ok'.
start_recovery() ->
    kz_util:put_callid(?MODULE),
    {Year, Month, _} = erlang:date(),
    maybe_recover_jobs(Year, Month, kapps_util:get_all_accounts('raw')).

-spec maybe_recover_jobs(kz_year(), kz_month(), ne_binaries()) -> 'ok'.
maybe_recover_jobs(Year, Month, Accounts) ->
    _ = [catch maybe_recover_account_jobs(Year, Month, AccountId) || AccountId <- Accounts],
    lager:debug("finished recovering account jobs").

-spec maybe_recover_account_jobs(kz_year(), kz_month(), ne_binary()) -> 'ok'.
maybe_recover_account_jobs(Year, Month, AccountId) ->
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    case kz_datamgr:get_results(Modb, <<"resources/status_listing">>, [{'keys', [<<"pending">>
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

-spec maybe_recover_incomplete_jobs(kz_json:objects()) -> 'ok'.
maybe_recover_incomplete_jobs(IncompleteJobs) ->
    _ = [maybe_recover_incomplete_job(kz_json:get_value(<<"doc">>, Job)) || Job <- IncompleteJobs],
    'ok'.

-spec maybe_recover_incomplete_job(kz_json:object()) -> 'ok'.
maybe_recover_incomplete_job(Job) ->
    Now = kz_time:now_s(),
    case (Now - kz_doc:modified(Job)) > ?RECOVERY_THRESHOLD_S of
        'false' -> 'ok';
        'true' ->
            lager:debug("job ~s in ~s is old and incomplete, attempting to restart it"
                       ,[kz_doc:id(Job), kz_doc:account_id(Job)]
                       ),
            recover_incomplete_job(Job)
    end.

-spec recover_incomplete_job(kz_json:object()) -> 'ok'.
recover_incomplete_job(Job) ->
    Db = kz_doc:account_db(Job),
    case kz_datamgr:save_doc(Db
                            ,kz_json:set_value(<<"pvt_recovering">>, 'true', kz_doc:update_pvt_modified(Job))
                            )
    of
        {'ok', Job1} -> republish_job(Job1);
        {'error', 'conflict'} -> lager:debug("someone updated the job while we were trying");
        {'error', _} -> 'ok'
    end.

-spec republish_job(kz_json:object()) -> 'ok'.
republish_job(Job) ->
    JobId = kz_doc:id(Job),
    ReqId = kz_json:get_value(<<"pvt_request_id">>, Job, kz_term:to_binary(?MODULE)),
    publish(kz_doc:account_id(Job), JobId, ReqId).

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
-spec init([]) -> {'ok', state()}.
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'timeout', Ref, ?RECOVERY_MESSAGE}
           ,#state{recovery_ref=Ref}=State
           ) ->
    _Pid = kz_util:spawn(fun start_recovery/0),
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
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
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
-spec terminate(any(), state()) -> 'ok'.
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
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec job_modb(ne_binary(), ne_binary()) -> ne_binary().
job_modb(AccountId, ?MATCH_MODB_PREFIX(Year,Month,_)) ->
    kz_util:format_account_mod_id(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month));
job_modb(AccountId, ?MATCH_MODB_PREFIX_M1(Year,Month,_)) ->
    kz_util:format_account_mod_id(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)).

-spec start_timer() -> reference().
start_timer() ->
    Time = ?RECOVERY_TIMEOUT_S * ?MILLISECONDS_IN_SECOND,
    erlang:start_timer(Time, self(), ?RECOVERY_MESSAGE).
