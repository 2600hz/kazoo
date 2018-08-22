%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_jobs).
-behaviour(gen_listener).

-export([start_link/1]).

-export([handle_start_account/2]).
-export([is_running/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-define(STALE_LIMIT, 360).
-define(POLLING_INTERVAL, 5 * ?MILLISECONDS_IN_SECOND).
-define(DEFAULT_LIMITS(AccountId), kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"max_outbound">>, 10)).

-define(MAX_WAIT_FOR_PENDING, 120 * ?MILLISECONDS_IN_SECOND).
-define(MAX_WAIT_FOR_RUNNING, ?MILLISECONDS_IN_HOUR).

-define(INIT_JOBS, #{distribute => []
                    ,serialize => []
                    ,pending => #{}
                    ,running => #{}
                    ,numbers => #{}
                    }).

-record(state, {account_id :: kz_term:ne_binary()
               ,queue = 'undefined' :: kz_term:api_binary()
               ,jobs = #{}
               ,limits = #{}
               ,stale = 0 :: integer()
               }).

-type state() :: #state{}.

-define(BINDINGS, [{'self', []}
                  ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(SERVER(AccountId), {'via', 'kz_globals', ?FAX_OUTBOUND_SERVER(AccountId)}).

-define(JOB_STATUS(J), {'job_status', {kapi_fax:job_id(J), kapi_fax:state(J), J}}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(AccountId) ->
    case gen_listener:start_link(?SERVER(AccountId)
                                ,?MODULE
                                , [{'bindings', ?BINDINGS}
                                  ,{'responders', ?RESPONDERS}
                                  ,{'queue_name', ?QUEUE_NAME}
                                  ,{'queue_options', ?QUEUE_OPTIONS}
                                  ,{'consume_options', ?CONSUME_OPTIONS}
                                  ]
                                , [AccountId])
    of
        {'error', {'already_started', Pid}}
          when is_pid(Pid) ->
            erlang:link(Pid),
            {'ok', Pid};
        Other -> Other
    end.


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kz_term:ne_binary()]) -> {'ok', state(), timeout()}.
init([AccountId]) ->
    _ = kz_util:spawn(fun cleanup_jobs/1, [AccountId]),
    State = #state{account_id=AccountId
                  ,limits = #{account => ?DEFAULT_LIMITS(AccountId) }
                  ,jobs = ?INIT_JOBS
                  },
    {'ok', State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'job_status',{JobId, <<"start">>, JObj}}, #state{jobs=CurJobs
                                                             ,account_id=AccountId
                                                             }=State) ->
    Jobs = handle_job_start(JobId, AccountId, JObj, CurJobs),
    {'noreply', State#state{jobs=Jobs}, ?POLLING_INTERVAL};
handle_cast({'job_status',{JobId, <<"end">>, JObj}}, #state{jobs=CurJobs
                                                           ,account_id=AccountId
                                                           }=State) ->
    Jobs = handle_job_end(JobId, AccountId, JObj, CurJobs),
    {'noreply', State#state{jobs=Jobs}, ?POLLING_INTERVAL};
handle_cast({'job_status',{JobId, <<"error">>, JObj}}, #state{jobs=CurJobs
                                                             ,account_id=AccountId
                                                             }=State) ->
    Jobs = handle_job_error(JobId, AccountId, JObj, CurJobs),
    {'noreply', State#state{jobs=Jobs}, ?POLLING_INTERVAL};
handle_cast({'gen_listener',{'created_queue', Queue}}, State) ->
    {'noreply', State#state{queue=Queue}, ?POLLING_INTERVAL};
handle_cast({'gen_listener',{'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State, ?POLLING_INTERVAL};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', #state{queue='undefined'}=State) ->
    {'noreply', State, ?POLLING_INTERVAL};
handle_info('timeout', #state{account_id=AccountId
                             ,stale=?STALE_LIMIT
                             }=State) ->
    lager:debug("stale limit of ~B reached for account ~s. exiting", [?STALE_LIMIT, AccountId]),
    {'stop', 'normal', State};
handle_info('timeout', #state{account_id=AccountId
                             ,jobs=#{distribute := Distribute
                                    ,pending := Pending
                                    ,running := Running
                                    }=Map0
                             ,stale=Stale
                             }=State) ->
    Map2 = #{running := Running} = maps:fold(fun check_pending/3, Map0, Pending),
    Map1 = maps:fold(fun check_running/3, Map2, Running),
    Map = Map1#{distribute => Distribute ++ get_account_jobs(AccountId)},
    {'noreply', distribute_jobs(State#state{jobs=Map, stale = Stale + 1}), ?POLLING_INTERVAL};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, _State) ->
    case kapi_fax:status_v(JObj) of
        'true' -> gen_server:cast(self(), ?JOB_STATUS(JObj));
        'false' -> 'ok'
    end,
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{account_id=AccountId}) ->
    lager:debug("terminating fax jobs for account ~s: ~p", [AccountId, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec distribute_jobs(state()) -> state().
distribute_jobs(#state{jobs=#{distribute := []
                             ,serialize := []
                             ,pending := #{}
                             ,running := #{}
                             }
                      }=State) -> State;
distribute_jobs(#state{jobs=#{distribute := []
                             ,serialize := []
                             }
                      }=State) ->
    State#state{stale=0};
distribute_jobs(#state{jobs=#{distribute := []
                             ,serialize := Serialize
                             }=Jobs
                      }=State) ->
    State#state{jobs=Jobs#{distribute => Serialize
                          ,serialize => []
                          }
               ,stale=0};
distribute_jobs(#state{account_id=AccountId
                      ,limits= #{account := MaxAccount}
                      ,jobs=#{pending := Pending
                             ,running := Running
                             }
                      }=State)
  when map_size(Pending) + map_size(Running) >= MaxAccount ->
    lager:warning("fax outbound limits (~b) reached for account ~s", [MaxAccount, AccountId]),
    State#state{stale=0};
distribute_jobs(#state{jobs=#{distribute := [Job | Jobs]}=Map}=State) ->
    maybe_distribute_job(Job, State#state{jobs=Map#{distribute => Jobs}}).

-spec maybe_distribute_job(kz_json:object(), state()) -> state().
maybe_distribute_job(Job, #state{account_id=AccountId}=State) ->
    Number = number(Job),
    case is_number_valid(Number, AccountId) of
        'true' -> distribute_job(knm_converters:normalize(Number, AccountId), Job, State#state{stale=0});
        'false' -> invalidate_job(Job, State)
    end.

-spec invalidate_job(kz_json:object(), state()) -> state().
invalidate_job(Job, #state{account_id=AccountId}=State) ->
    lager:error("fax job ~s of account ~s is invalid. removing it.", [kz_doc:id(Job), AccountId]),
    kz_datamgr:del_doc(?KZ_FAXES_DB, kz_doc:id(Job)),
    State.

-spec distribute_job(kz_term:ne_binary(), kz_json:object(), state()) -> state().
distribute_job(ToNumber, Job, #state{jobs=#{numbers := Numbers}}=State) ->
    JobId = kz_doc:id(Job),
    case ?SERIALIZE_OUTBOUND_NUMBER
        andalso maps:is_key(ToNumber, Numbers)
    of
        'true' -> distribute_jobs(serialize_job(JobId, Job, State));
        'false' -> distribute_jobs(start_job(JobId, ToNumber, Job, State))
    end.

-spec start_job(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), state()) -> state().
start_job(JobId, ToNumber, Job, #state{account_id=AccountId
                                      ,queue=Queue
                                      }=State) ->
    Payload = start_job_payload(AccountId, JobId, ToNumber, Queue),
    lager:debug("starting job: ~s for account id: ~s fax destination: ~s", [JobId, AccountId, ToNumber]),
    case kz_amqp_worker:call(Payload, fun kapi_fax:publish_start_job/1, fun validate_job_started/1) of
        {'error', 'timeout'} -> serialize_job(JobId, Job, State);
        {'ok', Reply} -> set_pending_job(JobId, kz_api:node(Reply), Job, ToNumber, State)
    end.

-spec validate_job_started(kz_json:object()) -> boolean().
validate_job_started(JObj) ->
    lager:debug("job start reply for ~s : ~s", [kapi_fax:job_id(JObj), kapi_fax:state(JObj)]),
    kapi_fax:state(JObj) =:= <<"start">>.

-spec start_job_payload(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> list().
start_job_payload(AccountId, JobId, ToNumber, Queue) ->
    [{<<"Job-ID">>, JobId}
    ,{<<"Account-ID">>, AccountId}
    ,{<<"To-Number">>, ToNumber}
    ,{<<"Control-Queue">>, Queue}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec serialize_job(kz_term:ne_binary(), kz_json:object(), state()) -> state().
serialize_job(JobId, Job, #state{jobs=#{serialize := Serialize}=Jobs}=State) ->
    lager:debug("serializing job: ~s", [JobId]),
    State#state{jobs=Jobs#{serialize => [Job | Serialize]}}.

-spec set_pending_job(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), state()) -> state().
set_pending_job(JobId, Node, Job, ToNumber, #state{jobs=#{pending := Pending
                                                         ,numbers := Numbers
                                                         }=Jobs
                                                  }=State) ->
    lager:debug("setting job: ~s  to pending status", [JobId]),
    State#state{jobs=Jobs#{pending => Pending#{JobId => #{number => ToNumber
                                                         ,start => kz_time:now_ms()
                                                         ,job => Job
                                                         ,node => Node
                                                         }
                                              }
                          ,numbers => Numbers#{ToNumber => JobId}
                          }}.

-spec handle_start_account(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_start_account(JObj, _Props) ->
    'true' = kapi_fax:start_account_v(JObj),
    AccountId = kapi_fax:account_id(JObj),
    lager:debug("starting account jobs worker for account id: ~s", [AccountId]),
    case is_running(AccountId) of
        'true' -> 'ok';
        'false' ->
            _ = fax_jobs_sup:start_account_jobs(AccountId),
            'ok'
    end.

-spec is_running(kz_term:ne_binary()) -> boolean().
is_running(AccountId) ->
    kz_globals:where_is(?FAX_OUTBOUND_SERVER(AccountId)) =/= 'undefined'.

-spec is_number_valid(kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_number_valid('undefined', _AccountId) -> 'false';
is_number_valid(<<>>, _AccountId) -> 'false';
is_number_valid(Number, AccountId) ->
    knm_converters:is_reconcilable(Number, AccountId).

-spec number(kz_json:object()) -> kz_term:api_binary().
number(JObj) ->
    case kz_json:get_value([<<"value">>, <<"to">>], JObj) of
        'undefined' -> 'undefined';
        <<>> -> <<>>;
        Number -> Number
    end.

-spec check_pending(kz_term:ne_binary(), map(), map()) -> map().
check_pending(JobId, #{number := ToNumber, start := Start}, #{pending := Pending, numbers := Numbers} = Map) ->
    case kz_time:now_ms() - Start > ?MAX_WAIT_FOR_PENDING of
        'true'  ->
            lager:debug("recovering pending fax job ~s", [JobId]),
            Map#{pending => maps:remove(JobId, Pending)
                ,numbers => maps:remove(ToNumber, Numbers)
                };
        'false' ->
            Map
    end.

-spec check_running(kz_term:ne_binary(), map(), map()) -> map().
check_running(JobId, #{number := ToNumber, start := Start}, #{running := Running, numbers := Numbers} = Map) ->
    case kz_time:now_ms() - Start > ?MAX_WAIT_FOR_RUNNING of
        'true'  ->
            lager:debug("recovering running fax job ~s", [JobId]),
            Map#{running => maps:remove(JobId, Running)
                ,numbers => maps:remove(ToNumber, Numbers)
                };
        'false' ->
            Map
    end.

-spec get_account_jobs(kz_term:ne_binary()) -> kz_json:objects().
get_account_jobs(AccountId) ->
    Upto = kz_time:now_s(),
    ViewOptions = [{'limit', 100}
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, Upto]}
                  ],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/jobs_by_account">>, ViewOptions) of
        {'ok', Jobs} -> lock_account_jobs(Jobs);
        {'error', _Reason} ->
            lager:debug("failed to fetch fax jobs for account ~s : ~p", [AccountId, _Reason]),
            []
    end.

-spec lock_account_jobs(kz_json:objects()) -> kz_json:objects().
lock_account_jobs([]) -> [];
lock_account_jobs(Jobs) ->
    lists:foldr(fun lock_account_jobs_fold/2, [], Jobs).

-spec lock_account_jobs_fold(kz_json:object(), kz_json:objects()) -> kz_json:objects().
lock_account_jobs_fold(JObj, Jobs) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, kz_doc:id(JObj)) of
        {'ok', Doc} -> lock_account_job(Doc, JObj, Jobs);
        {'error', Error} ->
            lager:debug("failed to lock jobid ~s : ~p", [kz_doc:id(JObj), Error]),
            Jobs
    end.

-spec lock_account_job(kz_json:object(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
lock_account_job(Doc, JObj, Jobs) ->
    UpdatedDoc = kz_json:set_value(<<"pvt_job_status">>, <<"locked">>, Doc),
    case kz_datamgr:save_doc(?KZ_FAXES_DB, UpdatedDoc, [{'rev', kz_doc:revision(Doc)}]) of
        {'ok', _U} ->

            lager:debug("fax job ~s locked for executing", [kz_doc:id(Doc)]),
            [JObj | Jobs];
        {'error', Error} ->
            lager:debug("failed to lock jobid ~s", [kz_doc:id(Doc), Error]),
            Jobs
    end.

-spec cleanup_jobs(kz_term:ne_binary()) -> 'ok'.
cleanup_jobs(AccountId) ->
    ViewOptions = [{'key', AccountId}
                  ],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/locked_jobs_by_account">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            lager:debug("cleaning up ~B fax jobs for account_id ~s", [length(JObjs), AccountId]),
            _ = [begin
                     DocId = kz_doc:id(JObj),
                     lager:debug("moving zombie job ~s status to pending", [DocId]),
                     kz_datamgr:update_doc(?KZ_FAXES_DB, DocId, [{<<"pvt_job_status">>, <<"pending">>}])
                 end
                 || JObj <- JObjs
                ],
            'ok';
        {'error', _R} -> lager:debug("unable to cleanup account_id ~s fax jobs: ~p", [AccountId, _R])
    end.

-spec maybe_serialize(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), map()) -> map().
maybe_serialize(<<"not_found">> = Status, <<"acquire">> = Stage, JobId, AccountId, Number, _Job, Jobs) ->
    lager:debug("dropping job ~s/~s from cache in stage ~s : ~s", [AccountId, JobId, Stage, Status]),
    #{pending := Pending
     ,numbers := Numbers
     ,serialize := Serialize
     } = Jobs,
    Jobs#{pending => maps:remove(JobId, Pending)
         ,numbers => maps:remove(Number, Numbers)
         ,serialize => Serialize
         };
maybe_serialize(Status, Stage, JobId, AccountId, Number, Job, Jobs) ->
    lager:debug("serializing job ~s/~s into cache in stage ~s : ~s", [AccountId, JobId, Stage, Status]),
    #{pending := Pending
     ,numbers := Numbers
     ,serialize := Serialize
     } = Jobs,
    Jobs#{pending => maps:remove(JobId, Pending)
         ,numbers => maps:remove(Number, Numbers)
         ,serialize => Serialize ++ [Job]
         }.

-spec handle_job_error(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), map()) -> map().
handle_job_error(JobId, AccountId, JObj, #{pending := Pending} = Jobs) ->
    Status = kz_json:get_ne_binary_value(<<"Status">>, JObj),
    Stage = kz_json:get_ne_binary_value(<<"Stage">>, JObj),
    case kz_maps:get([JobId, job], Pending, 'undefined') of
        'undefined' ->
            lager:warning("received error on account (~s) for a not pending job ~s (~s) : ~s", [AccountId, JobId, Stage, Status]),
            Jobs;
        Job ->
            Number = knm_converters:normalize(number(Job), AccountId),
            lager:warning("received error for fax job ~s/~s/~s on stage ~p: ~p", [AccountId, JobId, Number, Stage, Status]),
            maybe_serialize(Status, Stage, JobId, AccountId, Number, Job, Jobs)
    end.


-spec handle_job_start(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), map()) -> map().
handle_job_start(JobId, AccountId, JObj, #{pending := Pending
                                          ,running := Running
                                          } = Jobs) ->
    ServerId = kz_api:server_id(JObj),
    Status = kz_json:get_ne_binary_value(<<"Status">>, JObj),
    Stage = kz_json:get_ne_binary_value(<<"Stage">>, JObj),
    case kz_maps:get([JobId, number], Pending, 'undefined') of
        'undefined' ->
            lager:warning("received job start on account (~s) for a not pending job ~s (~s) : ~s", [AccountId, JobId, Stage, Status]),
            Jobs#{running => Running#{JobId => #{queue => ServerId
                                                ,start => kz_time:now_ms()
                                                }
                                     }
                 };
        Number ->
            Jobs#{pending => maps:remove(JobId, Pending)
                 ,running => Running#{JobId => #{number => Number
                                                ,queue => ServerId
                                                ,start => kz_time:now_ms()
                                                }
                                     }
                 }
    end.

-spec handle_job_end(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), map()) -> map().
handle_job_end(JobId, AccountId, JObj, #{pending := Pending
                                        ,running := Running
                                        ,numbers := Numbers
                                        } = Jobs) ->
    ServerId = kz_api:server_id(JObj),
    Status = kz_json:get_ne_binary_value(<<"Status">>, JObj),
    Stage = kz_json:get_ne_binary_value(<<"Stage">>, JObj),

    case kz_maps:get(JobId, Running, 'undefined') of
        'undefined' ->
            lager:debug("received fax end control in account ~s for a not controlled job ~s : (~s) : ~s", [AccountId, JobId, Stage, Status]),
            Jobs;

        #{queue := ServerId, number := Number} ->
            lager:debug("received fax end control status in account ~s for ~s : (~s) : ~s", [AccountId, JobId, Stage, Status]),
            Jobs#{pending => maps:remove(JobId, Pending)
                 ,running => maps:remove(JobId, Running)
                 ,numbers => maps:remove(Number, Numbers)
                 };
        #{number := Number, queue := OurServerId} ->
            lager:debug("received fax end control in account ~s for ~s from a different worker ~s / ~s : (~s) : ~s", [AccountId, JobId, ServerId, OurServerId, Stage, Status]),
            Jobs#{pending => maps:remove(JobId, Pending)
                 ,running => maps:remove(JobId, Running)
                 ,numbers => maps:remove(Number, Numbers)
                 };
        _Other ->
            lager:debug("received fax end control in account ~s for ~s from worker ~s : (~s) : ~s", [AccountId, JobId, ServerId, Stage, Status]),
            Jobs#{pending => maps:remove(JobId, Pending)
                 ,running => maps:remove(JobId, Running)
                 }
    end.
