%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
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

-record(state, {account_id :: ne_binary()
               ,queue = 'undefined' :: api_binary()
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

-define(JOB_STATUS(J), {'job_status', {kapi_fax:job_id(J), kapi_fax:state(J), kz_api:server_id(J)}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(ne_binary()) -> startlink_ret().
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
-spec init([ne_binary()]) -> {'ok', state(), kz_timeout()}.
init([AccountId]) ->
    _ = kz_util:spawn(fun cleanup_jobs/1, [AccountId]),
    State = #state{account_id=AccountId
                  ,limits = #{account => ?DEFAULT_LIMITS(AccountId) }
                  ,jobs = ?INIT_JOBS
                  },
    {'ok', State, ?POLLING_INTERVAL}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'job_status',{JobId, <<"start">>, ServerId}}, #state{jobs=#{pending := Pending
                                                                        ,running := Running
                                                                        }=Jobs
                                                                 }=State) ->
    #{JobId := #{number := Number}} = Pending,
    {'noreply', State#state{jobs=Jobs#{pending => maps:remove(JobId, Pending)
                                      ,running => Running#{JobId => #{number => Number
                                                                     ,queue => ServerId
                                                                     ,start => kz_time:now_ms()
                                                                     }
                                                          }
                                      }
                           }, ?POLLING_INTERVAL
    };
handle_cast({'job_status',{JobId, <<"end">>, ServerId}}, #state{jobs=#{pending := Pending
                                                                      ,running := Running
                                                                      ,numbers := Numbers
                                                                      }=Jobs
                                                               }=State) ->
    #{JobId := #{queue := ServerId
                ,number := Number
                }
     } = Running,
    {'noreply', State#state{jobs=Jobs#{pending => maps:remove(JobId, Pending)
                                      ,running => maps:remove(JobId, Running)
                                      ,numbers => maps:remove(Number, Numbers)
                                      }
                           }, ?POLLING_INTERVAL
    };
handle_cast({'gen_listener',{'created_queue', Queue}}, State) ->
    {'noreply', State#state{queue=Queue}, ?POLLING_INTERVAL};
handle_cast({'gen_listener',{'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State, ?POLLING_INTERVAL};
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
terminate(_Reason, #state{account_id=AccountId}) ->
    lager:debug("terminating fax jobs for account ~s: ~p", [AccountId, _Reason]).

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

-spec distribute_job(ne_binary(), kz_json:object(), state()) -> state().
distribute_job(ToNumber, Job, #state{account_id=AccountId
                                    ,queue=Q
                                    ,jobs=#{pending := Pending
                                           ,serialize := Serialize
                                           ,numbers := Numbers
                                           }=Map
                                    }=State) ->
    JobId = kz_doc:id(Job),
    case maps:is_key(ToNumber, Numbers) of
        'true' -> distribute_jobs(State#state{jobs=Map#{serialize => [Job | Serialize]}});
        'false' ->
            Payload = [{<<"Job-ID">>, JobId}
                      ,{<<"Account-ID">>, AccountId}
                      ,{<<"To-Number">>, ToNumber}
                       | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                      ],
            kz_amqp_worker:cast(Payload, fun kapi_fax:publish_start_job/1),
            Start = kz_time:now_ms(),
            distribute_jobs(State#state{jobs=Map#{pending => Pending#{JobId => #{number => ToNumber
                                                                                ,start => Start
                                                                                }
                                                                     }
                                                 ,numbers => Numbers#{ToNumber => JobId}
                                                 }})
    end.

-spec handle_start_account(kz_json:object(), kz_proplist()) -> 'ok'.
handle_start_account(JObj, _Props) ->
    'true' = kapi_fax:start_account_v(JObj),
    AccountId = kapi_fax:account_id(JObj),
    case is_running(AccountId) of
        'true' -> 'ok';
        'false' -> fax_jobs_sup:start_account_jobs(AccountId),
                   'ok'
    end.

-spec is_running(ne_binary()) -> boolean().
is_running(AccountId) ->
    kz_globals:where_is(?FAX_OUTBOUND_SERVER(AccountId)) =/= 'undefined'.

-spec is_number_valid(api_binary(), ne_binary()) -> boolean().
is_number_valid('undefined', _AccountId) -> 'false';
is_number_valid(<<>>, _AccountId) -> 'false';
is_number_valid(Number, AccountId) ->
    knm_converters:is_reconcilable(Number, AccountId).

-spec number(kz_json:object()) -> api_binary().
number(JObj) ->
    case kz_json:get_value([<<"value">>, <<"to">>], JObj) of
        'undefined' -> 'undefined';
        <<>> -> <<>>;
        Number -> Number
    end.

-spec check_pending(ne_binary(), map(), map()) -> map().
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

-spec check_running(ne_binary(), map(), map()) -> map().
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

-spec get_account_jobs(ne_binary()) -> kz_json:objects().
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

-spec cleanup_jobs(ne_binary()) -> 'ok'.
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
