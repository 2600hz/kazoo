%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_worker).
-behaviour(gen_listener).

-export([start_link/1]).

-export([handle_start_job/2]).

-export([handle_tx_resp/2
        ,handle_fax_event/2
        ,handle_job_status_query/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-define(SERVER(N), {'via', 'kz_globals', <<"fax_outbound_", N/binary>>}).

-define(MOVE_RETRY_INTERVAL, 5000).
-define(MAX_MOVE_RETRY, 5).
-define(MAX_MOVE_NOTIFY_MSG, "failed to move fax outbound document ~s from faxes database to account ~s modb").

-record(state, {queue_name :: kz_term:api_binary()
               ,job_id :: kz_term:api_binary()
               ,job :: kz_term:api_object()
               ,account_id :: kz_term:api_binary()
               ,status :: kz_term:api_ne_binary()
               ,fax_status :: kz_term:api_object()
               ,pages :: kz_term:api_integer()
               ,page = 0 :: integer()
               ,file :: kz_term:api_ne_binary()
               ,callid :: kz_term:ne_binary()
               ,controller :: kz_term:ne_binary()
               ,stage :: kz_term:api_binary()
               ,resp :: kz_term:api_object()
               ,move_retry = 0 :: integer()
               ,error :: kz_term:api_ne_binary()
               }).
-type state() :: #state{}.

-type release_ret() :: {kz_json:object(), kz_json:object()}.

-define(ORIGINATE_TIMEOUT, ?MILLISECONDS_IN_MINUTE).
-define(NEGOTIATE_TIMEOUT, ?MILLISECONDS_IN_MINUTE * 2).
-define(PAGE_TIMEOUT, ?MILLISECONDS_IN_MINUTE * 6).

-define(BINDINGS(CallId), [{'self', []}
                          ,{'fax', [{'restrict_to', ['query_status']}]}
                          ,{'call', [{'callid', CallId}
                                    ,{'restrict_to', [<<"CHANNEL_FAX_STATUS">>]
                                     }
                                    ]
                           }
                          ]).

-define(RESPONDERS, [{{?MODULE, 'handle_tx_resp'}
                     ,[{<<"resource">>, <<"offnet_resp">>}]
                     }
                    ,{{?MODULE, 'handle_fax_event'}
                     ,[{<<"call_event">>, <<"CHANNEL_FAX_STATUS">>}]
                     }
                    ,{{?MODULE, 'handle_job_status_query'}
                     ,[{<<"fax">>, <<"query_status">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DEFAULT_RETRY_PERIOD, kapps_config:get_integer(?CONFIG_CAT, <<"default_retry_period">>, 300)).
-define(DEFAULT_RETRY_COUNT, kapps_config:get_integer(?CONFIG_CAT, <<"default_retry_count">>, 3)).
-define(DEFAULT_COMPARE_FIELD, kapps_config:get_binary(?CONFIG_CAT, <<"default_compare_field">>, <<"result_cause">>)).


-define(CALLFLOW_LIST, <<"callflows/listing_by_number">>).
-define(ENSURE_CID_KEY, <<"ensure_valid_caller_id">>).
-define(DEFAULT_ENSURE_CID, kapps_config:get_is_true(?CONFIG_CAT, ?ENSURE_CID_KEY, 'true')).

-define(NOTIFICATION_OUTBOUND_EMAIL, [<<"notifications">>
                                     ,<<"outbound">>
                                     ,<<"email">>
                                     ,<<"send_to">>
                                     ]
       ).
-define(NOTIFICATION_EMAIL, [<<"notifications">>
                            ,<<"email">>
                            ,<<"send_to">>
                            ]
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(fax_job()) -> kz_types:startlink_ret().
start_link(FaxJob) ->
    CallId = kz_binary:rand_hex(16),
    gen_listener:start_link(server_name(FaxJob)
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS(CallId)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[FaxJob, CallId]).

-spec server_name(kz_json:object()) -> {'via', 'kz_globals', kz_term:ne_binary()}.
server_name(FaxJob) ->
    case ?SERIALIZE_OUTBOUND_NUMBER of
        'true' ->
            AccountId = kapi_fax:account_id(FaxJob),
            Number = knm_converters:normalize(kapi_fax:to_number(FaxJob), AccountId),
            ?SERVER(Number);
        'false' ->
            JobId = kapi_fax:job_id(FaxJob),
            ?SERVER(JobId)
    end.

-spec handle_tx_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_tx_resp(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'tx_resp', kz_api:msg_id(JObj), JObj}).

-spec handle_fax_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = kz_call_event:authorizing_id(JObj),
    Event = kz_call_event:application_event(JObj),
    gen_server:cast(Srv, {'fax_status', Event , JobId, JObj}).

-spec handle_job_status_query(kz_json:object(), kz_term:proplist()) -> any().
handle_job_status_query(JObj, Props) ->
    'true' = kapi_fax:query_status_v(JObj),
    Srv = props:get_value('server', Props),
    JobId = kz_json:get_value(<<"Job-ID">>, JObj),
    Queue = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    gen_server:cast(Srv, {'query_status', JobId, Queue, MsgId, JObj}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kz_json:object() | kz_term:ne_binary()]) -> {'ok', state()}.
init([FaxJob, CallId]) ->
    CtrlQ = kapi_fax:control_queue(FaxJob),
    JobId = kapi_fax:job_id(FaxJob),
    kz_util:put_callid(JobId),
    {'ok', #state{callid = CallId
                 ,job_id = JobId
                 ,account_id = kapi_fax:account_id(FaxJob)
                 ,controller = CtrlQ
                 ,stage = ?FAX_ACQUIRE
                 }
    }.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'tx_resp', JobId, JObj}, #state{job_id=JobId
                                            ,job=Job
                                            ,resp='undefined'
                                            }=State) ->
    case kz_call_event:response_message(JObj) of
        <<"SUCCESS">> ->
            lager:debug("received successful attempt to originate fax, continue processing"),
            send_status(State, <<"received successful attempt to originate fax">>),
            {'noreply', State#state{stage = ?FAX_NEGOTIATE
                                   ,status = <<"negotiating">>
                                   }, ?NEGOTIATE_TIMEOUT
            };
        _Else ->
            lager:debug("received failed attempt to tx fax, releasing job: ~s", [_Else]),
            send_error_status(State, kz_call_event:error_message(JObj)),
            {Resp, Doc} = release_failed_job('tx_resp', JObj, Job),
            gen_server:cast(self(), 'stop'),
            {'noreply', State#state{job=Doc, resp = Resp}}
    end;
handle_cast({'tx_resp', JobId2, _}, #state{job_id=JobId}=State) ->
    lager:debug("received txresp for ~s but this JobId is ~s",[JobId2, JobId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JobId, JObj}, State) ->
    Data = kz_call_event:application_data(JObj),
    TransferRate = kz_json:get_integer_value(<<"Fax-Transfer-Rate">>, Data, 1),
    lager:debug("fax status - negotiate result - ~s : ~p",[JobId, TransferRate]),
    Status = list_to_binary(["Fax negotiated at ", kz_term:to_list(TransferRate)]),
    send_status(State, Status, Data),
    {'noreply', State#state{status=Status
                           ,fax_status=Data
                           ,stage = ?FAX_SEND
                           }, ?PAGE_TIMEOUT
    };
handle_cast({'fax_status', <<"pageresult">>, JobId, JObj}
           ,#state{pages=Pages}=State
           ) ->
    Data = kz_call_event:application_data(JObj),
    Page = kz_json:get_integer_value(<<"Fax-Transferred-Pages">>, Data, 0),
    lager:debug("fax status - page result - ~s : ~p : ~p"
               ,[JobId, Page, kz_time:now_s()]
               ),
    Status = list_to_binary(["Sent Page ", kz_term:to_list(Page), " of ", kz_term:to_list(Pages)]),
    send_status(State#state{page=Page}, Status, Data),
    {'noreply', State#state{page=Page
                           ,status=Status
                           ,fax_status=Data
                           }, ?PAGE_TIMEOUT
    };
handle_cast({'fax_status', <<"result">>, JobId, JObj}
           ,#state{job_id=JobId
                  ,job=Job
                  ,file=Filepath
                  }=State
           ) ->
    Data = kz_call_event:application_data(JObj),
    {Resp, Doc} = case kz_json:is_true([<<"Fax-Success">>], Data) of
                      'true' ->
                          lager:debug("fax status - successfully transmitted fax ~s", [JobId]),
                          send_status(State, <<"Fax Successfully sent">>, ?FAX_END, Data),
                          release_successful_job(JObj, Job);
                      'false' ->
                          lager:debug("fax status - error transmitting fax ~s", [JobId]),
                          send_status(State, <<"Error sending fax">>, ?FAX_ERROR, Data),
                          release_failed_job('fax_result', JObj, Job)
                  end,
    _ = file:delete(Filepath),
    gen_server:cast(self(), 'stop'),
    {'noreply', State#state{job=Doc, resp = Resp}};
handle_cast({'fax_status', Event, JobId, _}, State) ->
    lager:debug("fax status ~s - ~s event not handled",[JobId, Event]),
    {'noreply', State};
handle_cast({'query_status', JobId, Queue, MsgId, _JObj}
           ,#state{status=Status
                  ,job_id=JobId
                  ,account_id=AccountId
                  ,fax_status=Data
                  }=State
           ) ->
    lager:debug("query fax status ~s handled by this queue",[JobId]),
    send_reply_status(Queue, MsgId, JobId, Status, AccountId, Data),
    {'noreply', State};
handle_cast({'query_status', JobId, Queue, MsgId, _JObj}, State) ->
    lager:debug("query fax status ~s not handled by this queue",[JobId]),
    Status = list_to_binary(["Fax ", JobId, " not being processed by this Queue"]),
    send_reply_status(Queue, MsgId, JobId, Status, <<"*">>,'undefined'),
    {'noreply', State};
handle_cast('attempt_transmission', #state{job_id = JobId
                                          ,queue_name=Q
                                          ,controller=CtrlQ
                                          ,stage=Stage
                                          }=State) ->
    case attempt_to_acquire_job(JobId, Q) of
        {'ok', JObj} ->
            lager:debug("acquired job ~s", [JobId]),
            Status = <<"preparing">>,
            NewState = State#state{job = JObj
                                  ,status = Status
                                  ,page = 0
                                  ,fax_status = 'undefined'
                                  ,stage = ?FAX_PREPARE
                                  },
            send_status(NewState, <<"job acquired">>, ?FAX_START, 'undefined'),
            send_control_status(CtrlQ, Q, JobId, ?FAX_START, Stage),
            gen_server:cast(self(), 'prepare_job'),
            {'noreply', NewState};
        {'error', Reason} ->
            lager:debug("failed to acquire job ~s: ~p", [JobId, Reason]),
            gen_server:cast(self(), 'error'),
            {'noreply', State#state{error=kz_term:to_binary(Reason)}}
    end;
handle_cast('prepare_job', #state{job_id=JobId
                                 ,job=JObj
                                 }=State) ->
    send_status(State, <<"fetching document to send">>, ?FAX_PREPARE, 'undefined'),
    case write_document(JObj, JobId) of
        {'ok', Filepath, Doc} ->
            send_status(State, <<"prepared document for send">>, ?FAX_PREPARE, 'undefined'),
            gen_server:cast(self(), 'send'),
            {'noreply', State#state{job=Doc
                                   ,file=Filepath
                                   ,pages=kz_json:get_integer_value(<<"pvt_pages">>, Doc)
                                   }};
        {'error', Message} ->
            send_error_status(State, kz_term:to_binary(Message)),
            {Resp, Doc} = release_failed_job('bad_file', Message, JObj),
            gen_server:cast(self(), 'stop'),
            {'noreply', State#state{job=Doc, resp=Resp}}
    end;
handle_cast('send', #state{job_id=JobId
                          ,job=JObj
                          ,queue_name=Q
                          ,callid=CallId
                          }=State) ->
    send_status(State, <<"ready to send">>, ?FAX_SEND, 'undefined'),
    send_fax(JobId, kz_json:set_value(<<"Call-ID">>, CallId, JObj), Q),
    {'noreply', State#state{stage=?FAX_ORIGINATE}, ?ORIGINATE_TIMEOUT};
handle_cast({'error', 'invalid_number', Number}, #state{job=JObj
                                                       }=State) ->
    send_error_status(State, <<"invalid fax number">>),
    {Resp, Doc} = release_failed_job('invalid_number', Number, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State#state{job=Doc, resp = Resp}};
handle_cast({'error', 'invalid_cid', Number}, #state{job=JObj
                                                    }=State) ->
    send_error_status(State, <<"invalid fax cid number">>),
    {Resp, Doc} = release_failed_job('invalid_cid', Number, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State#state{job=Doc, resp = Resp}};
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("fax worker discovered queue name ~s", [QueueName]),
    gen_server:cast(self(), 'attempt_transmission'),
    {'noreply', State#state{queue_name=QueueName}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    lager:debug("fax worker is consuming : ~p", [_IsConsuming]),
    {'noreply', State};
handle_cast('error', State) ->
    {'stop', 'normal', State};
handle_cast('stop', State) ->
    gen_listener:cast(self(), 'move_doc'),
    {'noreply', State};
handle_cast('move_doc', #state{account_id=AccountId
                              ,job_id=JobId
                              ,job=JObj
                              ,move_retry=?MAX_MOVE_RETRY
                              } = State) ->
    Props = kz_json:to_proplist(JObj),
    kz_notify:detailed_alert(?MAX_MOVE_NOTIFY_MSG, [JobId, AccountId], Props),
    gen_listener:cast(self(), 'notify'),
    {'noreply', State};
handle_cast('move_doc', #state{job=JObj, move_retry=Tries} = State) ->
    case maybe_move_doc(JObj, kzd_fax:job_status(JObj)) of
        {'ok', Doc} ->
            gen_listener:cast(self(), 'notify'),
            {'noreply', State#state{job=Doc}};
        {'error', Error} ->
            lager:error("error moving fax doc to modb : ~p", [Error]),
            timer:sleep(?MOVE_RETRY_INTERVAL),
            gen_listener:cast(self(), 'move_doc'),
            {'noreply', State#state{move_retry=Tries + 1}}
    end;
handle_cast('notify', #state{job=JObj, resp=Resp} = State) ->
    maybe_notify(JObj, Resp, kzd_fax:job_status(JObj)),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', #state{stage='undefined'}=State) ->
    {'noreply', State};
handle_info('timeout', #state{stage=Stage, job=JObj}=State) ->
    {Resp, Doc} = release_failed_job('job_timeout', Stage, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State#state{job=Doc, resp = Resp}};
handle_info(_Info, State) ->
    lager:debug("fax worker unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate('normal' = _Reason, #state{error=Error, stage=Stage, job_id=JobId, controller=CtrlQ})
  when Error =/= 'undefined' ->
    lager:debug("fax worker ~p terminating on stage ~s with reason : ~p", [self(), Stage, _Reason]),
    send_control_error(JobId, CtrlQ, Stage, Error);
terminate('normal' = _Reason, #state{stage=Stage, job_id=JobId, controller=CtrlQ, queue_name=Q}) ->
    send_control_status(CtrlQ, Q, JobId, ?FAX_END, Stage),
    lager:debug("fax worker ~p terminated on stage ~s with reason : ~p", [self(), Stage, _Reason]);
terminate(_Reason, #state{job=JObj, stage=Stage, job_id=JobId, controller=CtrlQ, queue_name=Q}) ->
    JObj1 = kz_json:set_value(<<"retries">>, 0, JObj),
    _ = release_failed_job('uncontrolled_termination', 'undefined', JObj1),
    send_control_status(CtrlQ, Q, JobId, ?FAX_END, Stage),
    lager:debug("fax worker ~p terminated on stage ~s with reason : ~p", [self(), Stage, _Reason]).

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
-spec attempt_to_acquire_job(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    {'ok', kz_json:object()} |
                                    {'error', any()}.
attempt_to_acquire_job(Id, Q) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, Id) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            attempt_to_acquire_job(JObj, Q, kz_json:get_value(<<"pvt_job_status">>, JObj))
    end.

-spec attempt_to_acquire_job(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary()) ->
                                    {'ok', kz_json:object()} |
                                    {'error', any()}.
attempt_to_acquire_job(JObj, Q, <<"locked">>) ->
    kz_datamgr:save_doc(?KZ_FAXES_DB
                       ,kz_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                           ,{<<"pvt_job_node">>, kz_term:to_binary(node())}
                                           ,{<<"pvt_modified">>, kz_time:now_s()}
                                           ,{<<"pvt_queue">>, Q}
                                           ]
                                          ,JObj
                                          )
                       ,[{'rev', kz_doc:revision(JObj)}]
                       );
attempt_to_acquire_job(JObj, _Q, Status) ->
    lager:debug("job not in an available status: ~s : ~p", [Status, JObj]),
    {'error', 'job_not_available'}.

-spec release_failed_job(atom(), any(), kz_json:object()) -> release_ret().
release_failed_job('bad_file', Msg, JObj) ->
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 0}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ,{<<"fax_bad_rows">>, 0}
             ,{<<"fax_speed">>, 0}
             ,{<<"fax_receiver_id">>, <<>>}
             ,{<<"fax_error_correction">>, 'false'}
             ],
    release_job(Result, JObj);
release_failed_job('tx_resp', Resp, JObj) ->
    Msg = kz_json:get_first_defined([<<"Error-Message">>, <<"Response-Message">>], Resp),
    <<"sip:", Code/binary>> = kz_json:get_value(<<"Response-Code">>, Resp, <<"sip:500">>),
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, kz_term:to_integer(Code)}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    KVs = [{[<<"Application-Data">>, <<"Fax-Result-Text">>], Msg}],
    release_job(Result, JObj, kz_json:set_values(KVs, Resp));
release_failed_job('invalid_number', Number, JObj) ->
    Msg = kz_term:to_binary(io_lib:format("invalid fax number: ~s", [Number])),
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 400}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj);
release_failed_job('invalid_cid', Number, JObj) ->
    Msg = kz_term:to_binary(io_lib:format("invalid fax cid number: ~s", [Number])),
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 400}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj);
release_failed_job('uncontrolled_termination', _, JObj) ->
    Msg = <<"process terminated. please contact your support for details">>,
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 500}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj);
release_failed_job('fax_result', Resp, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:487">>),
    Result = props:filter_undefined(
               [{<<"time_elapsed">>, elapsed_time(JObj)}
               ,{<<"result_code">>, kz_term:to_integer(Code)}
               ,{<<"result_cause">>, kz_json:get_value(<<"Hangup-Cause">>, Resp)}
               ,{<<"success">>, 'false'}
                | fax_util:fax_properties(kz_json:get_value(<<"Application-Data">>, Resp, Resp))
               ]),
    release_job(Result, JObj, Resp);
release_failed_job('job_timeout', 'undefined', JObj) ->
    release_failed_job('job_timeout', <<"undefined">>, JObj);
release_failed_job('job_timeout', Reason, JObj) ->
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 500}
             ,{<<"result_text">>, <<"fax job timed out - ", Reason/binary>>}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ,{<<"fax_bad_rows">>, 0}
             ,{<<"fax_speed">>, 0}
             ,{<<"fax_receiver_id">>, <<>>}
             ,{<<"fax_error_correction">>, 'false'}
             ],
    release_job(Result, JObj).

-spec release_successful_job(kz_json:object(), kz_json:object()) -> release_ret().
release_successful_job(Resp, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:200">>),
    Result = props:filter_undefined(
               [{<<"time_elapsed">>, elapsed_time(JObj)}
               ,{<<"result_code">>, kz_term:to_integer(Code)}
               ,{<<"result_cause">>, kz_json:get_value(<<"Hangup-Cause">>, Resp)}
               ,{<<"pvt_delivered_date">>,
                 case kz_json:is_true([<<"Application-Data">>, <<"Fax-Success">>], Resp) of
                     'true' -> kz_time:now_s();
                     'false' -> 'undefined'
                 end
                }
                | fax_util:fax_properties(kz_json:get_value(<<"Application-Data">>, Resp, Resp))
               ]),
    release_job(Result, JObj, Resp).

-spec release_job(kz_term:proplist(), kz_json:object()) -> release_ret().
release_job(Result, JObj) ->
    release_job(Result, JObj, kz_json:new()).

-spec release_job(kz_term:proplist(), kz_json:object(), kz_json:object()) -> release_ret().
release_job(Result, JObj, Resp) ->
    Success = props:is_true(<<"success">>, Result, 'false'),
    Updaters = [fun(J) ->
                        Attempts = kz_json:get_integer_value(<<"attempts">>, J, 0),
                        kz_json:set_value(<<"attempts">>, Attempts + 1, J)
                end
               ,fun(J) -> kz_json:set_value(<<"tx_result">>, kz_json:from_list(Result), J) end
               ,fun(J) -> kz_json:delete_key(<<"pvt_queue">>, J) end
               ,fun apply_reschedule_logic/1
               ,fun(J) ->
                        Attempts = kz_json:get_integer_value(<<"attempts">>, J, 0),
                        Retries = kz_json:get_integer_value(<<"retries">>, J, 1),
                        lager:debug("releasing job with retries: ~b attempts: ~b", [Retries, Attempts]),
                        case Retries - Attempts >= 1 of
                            _ when Success ->
                                lager:debug("releasing job with status: completed"),
                                kz_json:set_value(<<"pvt_job_status">>, <<"completed">>, J);
                            'true' ->
                                lager:debug("releasing job with status: pending"),
                                kz_json:set_value(<<"pvt_job_status">>, <<"pending">>, J);
                            'false' ->
                                lager:debug("releasing job with status: failed"),
                                kz_json:set_value(<<"pvt_job_status">>, <<"failed">>, J)
                        end
                end
               ],
    Update = lists:foldl(fun(F, J) -> F(J) end, JObj, Updaters),
    {'ok', Saved} = kz_datamgr:ensure_saved(?KZ_FAXES_DB, Update),
    {Resp, Saved}.

-spec apply_reschedule_logic(kz_json:object()) -> kz_json:object().
apply_reschedule_logic(JObj) ->
    Map = kapps_config:get_json(?CONFIG_CAT, <<"reschedule">>, kz_json:new()),
    case apply_reschedule_rules(kz_json:get_values(Map), set_default_update_fields(JObj)) of
        {'no_rules', JObj2} ->
            lager:debug("no rules applied in fax reschedule logic"),
            JObj2;
        {'ok', JObj2} ->
            lager:debug("rule '~s' applied in fax reschedule logic"
                       ,[kz_json:get_value(<<"reschedule_rule">>, JObj2)]),
            JObj2
    end.

-spec apply_reschedule_rules({kz_json:objects(), kz_json:path()}, kz_json:object()) ->
                                    {'ok', kz_json:object()} |
                                    {'no_rules', kz_json:object()}.
apply_reschedule_rules({[], _}, JObj) -> {'no_rules', JObj};
apply_reschedule_rules({[Rule | Rules], [Key | Keys]}, JObj) ->
    Attempts = kz_json:get_integer_value(<<"attempts">>, JObj, 0),
    Result = kz_json:get_value(<<"tx_result">>, JObj, kz_json:new()),
    Field = kz_json:get_value(<<"compare-field">>, Rule, ?DEFAULT_COMPARE_FIELD),
    ValueList = kz_json:get_value(<<"compare-values">>, Rule, []),
    ResultValue = kz_json:get_value(Field, Result),
    Attempt = get_attempt_value(kz_json:get_value(<<"attempt">>, Rule)),
    RetryAfter = kz_json:get_integer_value(<<"retry-after">>, Rule, ?DEFAULT_RETRY_PERIOD),
    Retries = kz_json:get_integer_value(<<"retries">>, Rule, ?DEFAULT_RETRY_COUNT),
    NewRetries = kz_json:get_integer_value(<<"new-retry-count">>, Rule, Retries),
    case (Attempt =:= Attempts
          orelse Attempt =:= -1
         )
        andalso lists:member(ResultValue, ValueList)
    of
        'true' ->
            NewJObj = kz_json:set_values([{<<"retry_after">>, RetryAfter}
                                         ,{<<"retries">>, NewRetries}
                                         ,{<<"reschedule_rule">>, Key}
                                         ]
                                        ,JObj
                                        ),
            {'ok', NewJObj};
        'false' ->
            apply_reschedule_rules({Rules, Keys}, JObj)
    end.

-spec get_attempt_value(kz_term:api_binary() | integer()) -> integer().
get_attempt_value(X) when is_integer(X) -> X;
get_attempt_value('undefined') -> -1;
get_attempt_value(<<"any">>) -> -1;
get_attempt_value(X) -> kz_term:to_integer(X).

-spec set_default_update_fields(kz_json:object()) -> kz_json:object().
set_default_update_fields(JObj) ->
    kz_json:set_values([{<<"pvt_modified">>, kz_time:now_s()}
                       ,{<<"retry_after">>, ?DEFAULT_RETRY_PERIOD}
                       ]
                      ,JObj
                      ).

-spec maybe_notify(kz_json:object(), kz_json:object(), kz_term:ne_binary()) -> any().
maybe_notify(JObj, Resp, <<"completed">>) ->
    Message = notify_fields(JObj, Resp),
    kapps_notify_publisher:cast(Message, fun kapi_notifications:publish_fax_outbound/1);
maybe_notify(JObj, Resp, <<"failed">>) ->
    Message = props:filter_undefined(
                [{<<"Fax-Error">>, fax_error(kz_json:merge_jobjs(JObj, Resp))}
                 | notify_fields(JObj, Resp)
                ]),
    kapps_notify_publisher:cast(Message, fun kapi_notifications:publish_fax_outbound_error/1);
maybe_notify(_JObj, _Resp, Status) ->
    lager:debug("notify Status ~p not handled",[Status]).

-spec maybe_move_doc(kz_json:object(), kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
maybe_move_doc(JObj, <<"completed">>) ->
    move_doc(JObj);
maybe_move_doc(JObj, <<"failed">>) ->
    move_doc(JObj);
maybe_move_doc(JObj, _) ->
    {'ok', JObj}.

-spec move_doc(kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      {'error', any()}.
move_doc(JObj) ->
    FromId = kz_doc:id(JObj),
    {Year, Month, _D} = kz_term:to_date(kz_doc:created(JObj)),
    FromDB = kz_doc:account_db(JObj),
    AccountId = kz_doc:account_id(JObj),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    ToDB = kz_util:format_account_modb(AccountMODb, 'encoded'),
    ToId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), FromId),
    Options = ['override_existing_document'
              ,{'transform', fun(_, B) -> kz_json:set_value(<<"folder">>, <<"outbox">>, B) end}
              ],
    lager:debug("moving fax outbound document ~s from faxes to ~s with id ~s", [FromId, AccountMODb, ToId]),
    kazoo_modb:move_doc(FromDB, {<<"fax">>, FromId}, ToDB, ToId, Options).

-spec fax_error(kz_json:object()) -> kz_term:api_binary().
fax_error(JObj) ->
    kz_json:get_first_defined([ [<<"Application-Data">>, <<"Fax-Result-Text">>]
                              , [<<"tx_result">>, <<"result_text">>]
                              ], JObj).

-spec notify_emails(kz_json:object()) -> kz_term:ne_binaries().
notify_emails(JObj) ->
    Emails = kz_json:get_first_defined([?NOTIFICATION_OUTBOUND_EMAIL
                                       ,?NOTIFICATION_EMAIL
                                       ], JObj, []),
    fax_util:notify_email_list(Emails).

-spec notify_fields(kz_json:object(), kz_json:object()) -> kz_term:proplist().
notify_fields(JObj, Resp) ->
    <<"sip:", HangupCode/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:0">>),
    HangupCause = kz_json:get_value(<<"Hangup-Cause">>, Resp),
    FaxFields = [{<<"Fax-Hangup-Code">>, kz_term:to_integer(HangupCode)}
                ,{<<"Fax-Hangup-Cause">>, HangupCause}
                 | fax_fields(kz_json:get_value(<<"Application-Data">>, Resp))
                ],

    ToNumber = kz_term:to_binary(kz_json:get_value(<<"to_number">>, JObj)),
    ToName = kz_term:to_binary(kz_json:get_value(<<"to_name">>, JObj, ToNumber)),
    Notify = [E || E <- notify_emails(JObj), not kz_term:is_empty(E)],

    props:filter_empty(
      [{<<"Caller-ID-Name">>, kz_json:get_value(<<"from_name">>, JObj)}
      ,{<<"Caller-ID-Number">>, kz_json:get_value(<<"from_number">>, JObj)}
      ,{<<"Callee-ID-Number">>, ToNumber}
      ,{<<"Callee-ID-Name">>, ToName }
      ,{<<"Account-ID">>, kz_doc:account_id(JObj)}
      ,{<<"Account-DB">>, kz_doc:account_db(JObj)}
      ,{<<"Fax-JobId">>, kz_doc:id(JObj)}
      ,{<<"Fax-ID">>, kz_doc:id(JObj)}
      ,{<<"FaxBox-ID">>, kz_json:get_value(<<"faxbox_id">>, JObj)}
      ,{<<"Fax-Notifications">>
       ,kz_json:from_list([{<<"email">>, kz_json:from_list([{<<"send_to">>, Notify}])}])
       }
      ,{<<"Fax-Info">>, kz_json:from_list(FaxFields) }
      ,{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, Resp)}
      ,{<<"Fax-Timestamp">>, kz_time:now_s()}
      ,{<<"Fax-Timezone">>, kz_json:get_value(<<"fax_timezone">>, JObj)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec fax_fields(kz_term:api_object()) -> kz_term:proplist().
fax_fields('undefined') -> [];
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- kz_json:to_proplist(JObj)].

-spec elapsed_time(kz_json:object()) -> non_neg_integer().
elapsed_time(JObj) ->
    Now = kz_time:now_s(),
    Created = kz_doc:created(JObj, Now),
    Now - Created.

-spec write_document(kz_json:object(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary(), kz_json:object()} |
                            {'error', any()}.
write_document(JObj, JobId) ->
    case kz_fax_attachment:fetch_faxable(?KZ_FAXES_DB, JObj) of
        {'ok', Content, _ContentType, Doc} ->
            Filepath = filename:join(?TMP_DIR, <<JobId/binary, ".tiff">>),
            kz_util:write_file(Filepath, Content),
            {'ok', Filepath, Doc};
        Error -> Error
    end.

-spec send_fax(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_fax(JobId, JObj, Q) ->
    SendFax = fun() -> send_fax(JobId, JObj, Q, get_did(JObj)) end,
    maybe_ensure_valid_caller_id(JObj, SendFax).

-spec maybe_ensure_valid_caller_id(kz_json:object(), function()) -> 'ok'.
maybe_ensure_valid_caller_id(JObj, SendFax) ->
    case kz_json:is_true(?ENSURE_CID_KEY, JObj, ?DEFAULT_ENSURE_CID) of
        'false' -> SendFax();
        'true' -> ensure_valid_caller_id(JObj, SendFax)
    end.

-spec ensure_valid_caller_id(kz_json:object(), function()) -> 'ok'.
ensure_valid_caller_id(JObj, SendFax) ->
    CIDNum = kz_json:get_value(<<"from_number">>, JObj),
    case fax_util:is_valid_caller_id(CIDNum, kz_doc:account_id(JObj)) of
        'true' -> SendFax();
        'false' ->
            lager:debug("CIDNum ~s invalid in sending fax", [CIDNum]),
            gen_server:cast(self(), {'error', 'invalid_cid', CIDNum})
    end.

-spec send_fax(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), binary() | 'undefined') -> 'ok'.
send_fax(_JobId, _JObj, _Q, 'undefined') ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(undefined)">>});
send_fax(_JobId, _JObj, _Q, <<>>) ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(empty)">>});
send_fax(JobId, JObj, Q, ToDID) ->
    IgnoreEarlyMedia = 'true', %kz_term:to_binary(kapps_config:get_is_true(?CONFIG_CAT, <<"ignore_early_media">>, 'false')),
    ToNumber = kz_term:to_binary(kz_json:get_value(<<"to_number">>, JObj)),
    ToName = kz_term:to_binary(kz_json:get_value(<<"to_name">>, JObj, ToNumber)),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    ETimeout = kz_term:to_binary(kapps_config:get_integer(?CONFIG_CAT, <<"endpoint_timeout">>, 40)),
    AccountId =  kz_doc:account_id(JObj),
    AccountRealm = kzd_accounts:fetch_realm(AccountId),
    Request = props:filter_undefined(
                [{<<"Outbound-Caller-ID-Name">>, kz_json:get_value(<<"from_name">>, JObj)}
                ,{<<"Outbound-Caller-ID-Number">>, kz_json:get_value(<<"from_number">>, JObj)}
                ,{<<"Outbound-Callee-ID-Number">>, ToNumber}
                ,{<<"Outbound-Callee-ID-Name">>, ToName }
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Account-Realm">>, AccountRealm}
                ,{<<"To-DID">>, ToDID}
                ,{<<"Fax-Identity-Number">>, kz_json:get_value(<<"fax_identity_number">>, JObj)}
                ,{<<"Fax-Identity-Name">>, kz_json:get_value(<<"fax_identity_name">>, JObj)}
                ,{<<"Fax-Timezone">>, kzd_fax_box:timezone(JObj)}
                ,{<<"Flags">>, [<<"fax">> | kz_json:get_value(<<"flags">>, JObj, [])]}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Hunt-Account-ID">>, get_hunt_account_id(AccountId)}
                ,{<<"Msg-ID">>, JobId}
                ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                ,{<<"Custom-Channel-Vars">>, resource_ccvs(JobId)}
                ,{<<"Custom-SIP-Headers">>, kz_json:get_value(<<"custom_sip_headers">>, JObj)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
                ,{<<"Application-Name">>, <<"fax">>}
                ,{<<"Timeout">>,ETimeout}
                ,{<<"Application-Data">>, get_proxy_url(JobId)}
                ,{<<"Origination-Call-ID">>, CallId}
                ,{<<"Bypass-E164">>, kz_json:is_true(<<"bypass_e164">>, JObj)}
                ,{<<"Fax-T38-Enabled">>, false}
                 | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("sending fax originate request ~s with call-id ~s", [JobId, CallId]),
    kapi_offnet_resource:publish_req(Request).

-spec get_hunt_account_id(kz_term:ne_binary()) -> kz_term:api_binary().
get_hunt_account_id(AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Options = [{'key', <<"no_match">>}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, ?CALLFLOW_LIST, Options) of
        {'ok', [JObj]} -> maybe_hunt_account_id(kz_json:get_value([<<"doc">>, <<"flow">>], JObj), AccountId);
        _ -> 'undefined'
    end.

-spec maybe_hunt_account_id(kz_term:api_object(), kz_term:ne_binary()) -> kz_term:api_binary().
maybe_hunt_account_id('undefined', _) -> 'undefined';
maybe_hunt_account_id(JObj, AccountId) ->
    case kz_json:get_value(<<"module">>, JObj) of
        <<"resources">> ->
            kz_json:get_value([<<"data">>, <<"hunt_account_id">>], JObj, AccountId);
        _ ->
            maybe_hunt_account_id(kz_json:get_value([<<"children">>, <<"_">>], JObj), AccountId)
    end.

-spec resource_ccvs(kz_term:ne_binary()) -> kz_json:object().
resource_ccvs(JobId) ->
    kz_json:from_list([{<<"Authorizing-ID">>, JobId}
                      ,{<<"Authorizing-Type">>, <<"outbound_fax">>}
                      ,{<<"RTCP-MUX">>, false}
                      ]).

-spec get_did(kz_json:object()) -> kz_term:api_binary().
get_did(JObj) ->
    case kz_json:is_true(<<"bypass_e164">>, JObj, 'false') of
        'true' -> kz_json:get_value(<<"to_number">>, JObj);
        'false' -> knm_converters:normalize(kz_json:get_value(<<"to_number">>, JObj))
    end.

-spec get_proxy_url(kz_term:ne_binary()) -> kz_term:ne_binary().
get_proxy_url(JobId) ->
    Hostname = kz_network_utils:get_hostname(),
    Port = integer_to_binary(?PORT),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId, ".tiff"]).

-spec send_status(state(), kz_term:ne_binary()) -> any().
send_status(State, Status) ->
    send_status(State, Status, ?FAX_SEND, 'undefined').

-spec send_error_status(state(), kz_term:ne_binary()) -> any().
send_error_status(State, Status) ->
    send_status(State, Status, ?FAX_ERROR, 'undefined').

-spec send_status(state(), kz_term:ne_binary(), kz_term:api_object()) -> any().
send_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_SEND, FaxInfo).

-spec send_status(state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()) -> any().
send_status(#state{job=JObj
                  ,page=Page
                  ,job_id=JobId
                  ,account_id=AccountId
                  }
           ,Status, FaxState, FaxInfo) ->
    FaxboxId = kz_json:get_value(<<"faxbox_id">>, JObj),
    CloudJobId = kz_json:get_value(<<"cloud_job_id">>, JObj),
    CloudPrinterId = kz_json:get_value(<<"cloud_printer_id">>, JObj),
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                ,{<<"FaxBox-ID">>, FaxboxId}
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Cloud-Job-ID">>, CloudJobId}
                ,{<<"Cloud-Printer-ID">>, CloudPrinterId}
                ,{<<"Status">>, Status}
                ,{<<"Fax-State">>, FaxState}
                ,{<<"Fax-Info">>, FaxInfo}
                ,{<<"Direction">>, ?FAX_OUTGOING}
                ,{<<"Page">>, Page}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_fax:publish_status(Payload).

-spec send_reply_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()) -> 'ok'.
send_reply_status(Q, MsgId, JobId, Status, AccountId, JObj) ->
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                ,{<<"Status">>, Status}
                ,{<<"Msg-ID">>, MsgId}
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Fax-Info">>, JObj}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_fax:publish_targeted_status(Q, Payload).

-spec send_control_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_control_status(CtrlQ, Q, JobId, FaxState, Stage) ->
    Payload = [{<<"Job-ID">>, JobId}
              ,{<<"Fax-State">>, FaxState}
              ,{<<"Stage">>, Stage}
               | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_fax:publish_targeted_status(CtrlQ, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_start_job(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_start_job(JObj, _Props) ->
    'true' = kapi_fax:start_job_v(JObj),
    case fax_worker_sup:start_fax_job(JObj) of
        {'ok', _Pid} -> send_start_reply(JObj, <<"start">>, 'undefined');
        {'error', _Reason} -> send_start_reply(JObj, <<"error">>, <<"already running">>)
    end.

-spec send_start_reply(kz_json:object(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> 'ok'.
send_start_reply(JObj, Status, Reason) ->
    JobId = kapi_fax:job_id(JObj),
    CtrlQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    Payload = [{<<"Job-ID">>, JobId}
              ,{<<"Fax-State">>, Status}
              ,{<<"Stage">>, <<"start">>}
              ,{<<"Status">>, Reason}
              ,{?KEY_MSG_ID, MsgId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_fax:publish_targeted_status(CtrlQ, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec send_control_error(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_control_error(JobId, CtrlQ, Stage, Reason) ->
    Payload = [{<<"Job-ID">>, JobId}
              ,{<<"Fax-State">>, <<"error">>}
              ,{<<"Stage">>, Stage}
              ,{<<"Status">>, Reason}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_fax:publish_targeted_status(CtrlQ, P) end,
    kz_amqp_worker:cast(Payload, Publisher).
