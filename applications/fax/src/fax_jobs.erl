%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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

-define(POLLING_INTERVAL, 5000).
-define(DEFAULT_LIMITS, #{account => 5}).
-define(INIT_JOBS, #{distribute => [], serialize => [], pending => #{}, running => #{}, numbers => #{}}).

-record(state, {account_id :: ne_binary()
               ,queue = 'undefined' :: api_binary()
               ,jobs = #{}
               ,limits = #{}
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
init([AccountId]) ->
    {'ok', #state{account_id=AccountId
                 ,limits = ?DEFAULT_LIMITS
                 ,jobs = ?INIT_JOBS
                 }, ?POLLING_INTERVAL}.

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
handle_cast({'job_status',{JobId, <<"start">>, ServerId}}, #state{jobs=#{pending := Pending
                                                              ,running := Running
                                                              }=Jobs
                                                       }=State) ->
    #{JobId := Number} = Pending,
    {'noreply', State#state{jobs=Jobs#{pending => maps:remove(JobId, Pending)
                                      ,running => Running#{JobId => #{number => Number
                                                                     ,queue => ServerId
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
handle_info('timeout', #state{queue='undefined'}=State) ->
    {'noreply', State, ?POLLING_INTERVAL};
handle_info('timeout', #state{account_id=AccountId
                             ,jobs=#{distribute := []
                                    ,serialize := []
                                    }=Map
                             }=State) ->
    Upto = kz_util:current_tstamp(),
    ViewOptions = [{'limit', 100}
                  ,{'startkey', [AccountId, 0]}
                  ,{'endkey', [AccountId, Upto]}
                  ],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/jobs_by_account">>, ViewOptions) of
        {'ok', []} -> {'noreply', State, ?POLLING_INTERVAL};
        {'ok', Jobs} ->
            lager:debug("fetched ~b jobs for account ~s, attempting to distribute to workers"
                       ,[length(Jobs), AccountId]
                       ),
            {'noreply', distribute_jobs(State#state{jobs=Map#{distribute => Jobs}}), ?POLLING_INTERVAL};
        {'error', _Reason} ->
            lager:debug("failed to fetch fax jobs for account ~s : ~p", [AccountId, _Reason]),
            {'noreply', State, ?POLLING_INTERVAL}
    end;
%% handle_info({'DOWN', _Ref, process, Pid, 'normal'}, #state{jobs=Jobs}=State) ->
%%     lager:debug("fax worker (~p) ended normally",[Pid]),
%%     {'noreply', State#state{jobs=distribute_jobs(Jobs)}, ?POLLING_INTERVAL};
%% handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{jobs=Jobs}=State) ->
%%     lager:debug("fax worker (~p) crashed with reason ~p",[Pid, Reason]),
%%     {'noreply', State#state{jobs=distribute_jobs(Jobs)}, ?POLLING_INTERVAL};
handle_info('timeout', State) ->
    {'noreply', distribute_jobs(State), ?POLLING_INTERVAL};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec distribute_jobs(state()) -> state().
distribute_jobs(#state{jobs=#{distribute := []
                             ,serialize := []
                             }
                      }=State) -> State;
distribute_jobs(#state{jobs=#{distribute := []
                             ,serialize := Serialize
                             }=Jobs
                      }=State) ->
    State#state{jobs=Jobs#{distribute => Serialize
                          ,serialize => []
                          }};
distribute_jobs(#state{account_id=AccountId
                      ,limits= #{account := MaxAccount}
                      ,jobs=#{pending := Pending
                             ,running := Running
                             }
                      }=State)
  when map_size(Pending) + map_size(Running) >= MaxAccount ->
    lager:warning("fax outbound limits (~b) reached for account ~s", [MaxAccount, AccountId]),
    State;
distribute_jobs(#state{account_id=AccountId
                      ,queue=Q
                      ,jobs=#{distribute := [Job | Jobs]
                             ,pending := Pending
                             ,serialize := Serialize
                             ,numbers := Numbers
                             }=Map
                      }=State) ->
    JobId = kz_doc:id(Job),
    Number = kz_json:get_value([<<"value">>, <<"to">>], Job),
    ToNumber = knm_converters:normalize(Number, AccountId),
    case maps:is_key(ToNumber, Numbers) of
        'true' -> distribute_jobs(State#state{jobs=Map#{distribute => Jobs
                                                       ,serialize => [Job | Serialize]
                                                       }});
        'false' ->
            Payload = [{<<"Job-ID">>, JobId}
                       ,{<<"Account-ID">>, AccountId}
                       ,{<<"To-Number">>, ToNumber}
                           | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                      ],
            kz_amqp_worker:cast(Payload, fun kapi_fax:publish_start_job/1),
            distribute_jobs(State#state{jobs=Map#{distribute => Jobs
                                                  ,pending => Pending#{JobId => ToNumber}
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
