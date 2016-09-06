%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_worker).

-behaviour(gen_listener).

-export([start_link/1]).

-export([handle_start_job/2]).

-export([handle_tx_resp/2
        ,handle_fax_event/2
        ,handle_channel_destroy/2
        ,handle_channel_replaced/2
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

-record(state, {queue_name :: api_binary()
               ,job_id :: api_binary()
               ,job :: api_object()
               ,account_id :: api_binary()
               ,status :: binary()
               ,fax_status :: api_object()
               ,pages  :: integer()
               ,page = 0  :: integer()
               ,file :: ne_binary()
               ,callid :: ne_binary()
               ,controller :: ne_binary()
               ,stage :: api_binary()
               }).
-type state() :: #state{}.

-define(ORIGINATE_TIMEOUT, ?MILLISECONDS_IN_MINUTE).
-define(NEGOTIATE_TIMEOUT, ?MILLISECONDS_IN_MINUTE).
-define(PAGE_TIMEOUT, ?MILLISECONDS_IN_MINUTE * 6).

-define(BINDINGS(CallId), [{'self', []}
                          ,{'fax', [{'restrict_to', ['query_status']}]}
                          ,{'call', [{'callid', CallId}
                                    ,{'restrict_to', [<<"CHANNEL_FAX_STATUS">>
                                                     ,<<"CHANNEL_DESTROY">>
                                                     ,<<"CHANNEL_REPLACED">>
                                                     ]
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
                    ,{{?MODULE, 'handle_channel_destroy'}
                     ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                     }
                    ,{{?MODULE, 'handle_channel_replaced'}
                     ,[{<<"call_event">>, <<"CHANNEL_REPLACED">>}]
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

-define(COUNT_PAGES_CMD, <<"echo -n `tiffinfo ~s | grep 'Page Number' | grep -c 'P'`">>).
-define(CONVERT_PDF_CMD, <<"/usr/bin/gs -q "
                           "-r204x98 "
                           "-g1728x1078 "
                           "-dNOPAUSE "
                           "-dBATCH "
                           "-dSAFER "
                           "-sDEVICE=tiffg3 "
                           "-sOutputFile=~s -- ~s > /dev/null "
                           "&& echo -n \"success\""
                         >>).
-define(CONVERT_IMAGE_CMD, <<"convert -density 204x98 "
                             "-units PixelsPerInch "
                             "-size 1728x1078 ~s ~s > /dev/null "
                             "&& echo -n success"
                           >>).
-define(CONVERT_OO_DOC_CMD, <<"unoconv -c ~s -f pdf --stdout ~s "
                              "| /usr/bin/gs -q "
                              "-r204x98 "
                              "-g1728x1078 "
                              "-dNOPAUSE "
                              "-dBATCH "
                              "-dSAFER "
                              "-sDEVICE=tiffg3 "
                              "-sOutputFile=~s - > /dev/null"
                              "&& echo -n success"
                            >>).

-define(CALLFLOW_LIST, <<"callflows/listing_by_number">>).
-define(ENSURE_CID_KEY, <<"ensure_valid_caller_id">>).
-define(DEFAULT_ENSURE_CID, kapps_config:get_is_true(?CONFIG_CAT, ?ENSURE_CID_KEY, 'true')).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(fax_job()) -> startlink_ret().
start_link(FaxJob) ->
    CallId = kz_util:rand_hex_binary(16),
    _JobId = kapi_fax:job_id(FaxJob),
    AccountId = kapi_fax:account_id(FaxJob),
    Number = knm_converters:normalize(kapi_fax:to_number(FaxJob), AccountId),
    gen_listener:start_link(?SERVER(Number)
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS(CallId)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[FaxJob, CallId]).

-spec handle_tx_resp(kz_json:object(), kz_proplist()) -> 'ok'.
handle_tx_resp(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'tx_resp', kz_api:msg_id(JObj), JObj}).

-spec handle_fax_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = kz_call_event:authorizing_id(JObj),
    Event = kz_call_event:application_event(JObj),
    gen_server:cast(Srv, {'fax_status', Event , JobId, JObj}).

-spec handle_channel_destroy(kz_json:object(), kz_proplist()) -> 'ok'.
handle_channel_destroy(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = kz_call_event:authorizing_id(JObj),
    gen_server:cast(Srv, {'channel_destroy', JobId, JObj}).

-spec handle_channel_replaced(kz_json:object(), kz_proplist()) -> 'ok'.
handle_channel_replaced(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = kz_call_event:authorizing_id(JObj),
    gen_server:cast(Srv, {'channel_replaced', JobId, JObj}).

-spec handle_job_status_query(kz_json:object(), kz_proplist()) -> any().
handle_job_status_query(JObj, Props) ->
    'true' = kapi_fax:query_status_v(JObj),
    Srv = props:get_value('server', Props),
    JobId = kz_json:get_value(<<"Job-ID">>, JObj),
    Queue = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    gen_server:cast(Srv, {'query_status', JobId, Queue, MsgId, JObj}).

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
init([FaxJob, CallId]) ->
    CtrlQ = kz_api:server_id(FaxJob),
    JobId = kapi_fax:job_id(FaxJob),
    kz_util:put_callid(JobId),
    {'ok', #state{callid = CallId
                 ,job_id = JobId
                 ,account_id = kapi_fax:account_id(FaxJob)
                 ,controller = CtrlQ
                 }
    }.

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
handle_cast({'tx_resp', JobId, JObj}, #state{job_id=JobId
                                            ,job=Job
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
            release_failed_job('tx_resp', JObj, Job),
            gen_server:cast(self(), 'stop'),
            {'noreply', State}
    end;
handle_cast({'tx_resp', JobId2, _}, #state{job_id=JobId}=State) ->
    lager:debug("received txresp for ~s but this JobId is ~s",[JobId2, JobId]),
    {'noreply', State};
handle_cast({'channel_destroy', JobId, JObj}, #state{job_id=JobId
                                                    ,job=Job
                                                    ,fax_status='undefined'
                                                    }=State) ->
    lager:debug("received channel destroy for ~s : ~p",[JobId, JObj]),
    send_error_status(State, kz_call_event:hangup_cause(JObj)),
    _ = release_failed_job('channel_destroy', JObj, Job),
    gen_server:cast(self(), 'stop'),
    {'noreply', State};
handle_cast({'channel_destroy', JobId, _JObj}, #state{job_id=JobId}=State) ->
    lager:debug("ignoring received channel destroy for ~s",[JobId]),
    {'noreply', State};
handle_cast({'channel_destroy', JobId2, _JObj}, #state{job_id=JobId}=State) ->
    lager:debug("received channel destroy for ~s but this JobId is ~s",[JobId2, JobId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JobId, JObj}, State) ->
    Data = kz_call_event:application_data(JObj),
    TransferRate = kz_json:get_integer_value(<<"Fax-Transfer-Rate">>, Data, 1),
    lager:debug("fax status - negociate result - ~s : ~p",[JobId, TransferRate]),
    Status = list_to_binary(["Fax negotiated at ", kz_util:to_list(TransferRate)]),
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
               ,[JobId, Page, kz_util:current_tstamp()]
               ),
    Status = list_to_binary(["Sent Page ", kz_util:to_list(Page), " of ", kz_util:to_list(Pages)]),
    send_status(State#state{page=Page}, Status, Data),
    {'noreply', State#state{page=Page
                           ,status=Status
                           ,fax_status=Data
                           }, ?PAGE_TIMEOUT
    };
handle_cast({'fax_status', <<"result">>, JobId, JObj}
           ,#state{job_id=JobId
                  ,job=Job
                  }=State
           ) ->
    Data = kz_call_event:application_data(JObj),
    case kz_json:is_true([<<"Fax-Success">>], Data) of
        'true' ->
            send_status(State, <<"Fax Successfuly sent">>, ?FAX_END, Data),
            release_successful_job(JObj, Job);
        'false' ->
            send_status(State, <<"Error sending fax">>, ?FAX_ERROR, Data),
            release_failed_job('fax_result', JObj, Job)
    end,
    gen_server:cast(self(), 'stop'),
    {'noreply', State};
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
                                          }=State) ->
    case attempt_to_acquire_job(JobId, Q) of
        {'ok', JObj} ->
            lager:debug("acquired job ~s", [JobId]),
            Status = <<"preparing">>,
            NewState = State#state{job = JObj
                                  ,status = Status
                                  ,page = 0
                                  ,fax_status = 'undefined'
                                  ,stage = ?FAX_ACQUIRE
                                  },
            send_status(NewState, <<"job acquired">>, ?FAX_START, 'undefined'),
            send_control_status(CtrlQ, Q, JobId, ?FAX_START),
            gen_server:cast(self(), 'prepare_job'),
            {'noreply', NewState};
        {'error', Reason} ->
            lager:debug("failed to acquire job ~s: ~p", [JobId, Reason]),
            gen_server:cast(self(), 'stop'),
            {'noreply', State}
    end;
handle_cast('prepare_job', #state{job_id=JobId
                                 ,job=JObj
                                 }=State) ->
    send_status(State, <<"fetching document to send">>, ?FAX_PREPARE, 'undefined'),
    case fetch_document(JObj) of
        {'ok', 200, RespHeaders, RespContent} ->
            send_status(State, <<"preparing document to send">>, ?FAX_PREPARE, 'undefined'),
            case prepare_contents(JobId, RespHeaders, RespContent) of
                {'error', Cause} ->
                    send_error_status(State, Cause),
                    release_failed_job('bad_file', Cause, JObj),
                    gen_server:cast(self(), 'stop'),
                    {'noreply', State};
                {'ok', OutputFile} ->
                    gen_server:cast(self(), 'count_pages'),
                    {'noreply', State#state{file=OutputFile}}
            end;
        {'ok', Status, _, _} ->
            lager:debug("failed to fetch file for job: http response ~p", [Status]),
            _ = send_error_status(State, integer_to_binary(Status)),
            release_failed_job('fetch_failed', Status, JObj),
            gen_server:cast(self(), 'stop'),
            {'noreply', State};
        {'error', Reason} ->
            lager:debug("failed to fetch file for job: ~p", [Reason]),
            send_error_status(State, <<"failed to fetch file for job">>),
            release_failed_job('fetch_error', Reason, JObj),
            gen_server:cast(self(), 'stop'),
            {'noreply', State}
    end;
handle_cast('count_pages', #state{file=File
                                 ,job=JObj
                                 }=State) ->
    {NumberOfPages, FileSize} = get_sizes(File),
    Values = [{<<"pvt_pages">>, NumberOfPages}
             ,{<<"pvt_size">>, FileSize}
             ],
    NewState = case NumberOfPages of
                   Num when Num == 0 ->
                       State#state{job = kz_json:set_values(Values, JObj)
                                  ,pages = Num
                                  ,status = <<"unknown">>
                                  };
                   _ ->
                       State#state{job=kz_json:set_values(Values, JObj)
                                  ,pages=NumberOfPages
                                  }
               end,
    gen_server:cast(self(), 'send'),
    {'noreply', NewState};
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
    release_failed_job('invalid_number', Number, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State};
handle_cast({'error', 'invalid_cid', Number}, #state{job=JObj
                                                    }=State) ->
    send_error_status(State, <<"invalid fax cid number">>),
    release_failed_job('invalid_cid', Number, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("fax worker discovered queue name ~s", [QueueName]),
    gen_server:cast(self(), 'attempt_transmission'),
    {'noreply', State#state{queue_name=QueueName}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    lager:debug("fax worker is consuming : ~p", [_IsConsuming]),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
                                                % @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('timeout', #state{stage='undefined'}=State) ->
    {'noreply', State};
handle_info('timeout', #state{stage=Stage, job=JObj}=State) ->
    release_failed_job('job_timeout', Stage, JObj),
    gen_server:cast(self(), 'stop'),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("fax worker unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> handle_event_ret().
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
terminate(_Reason, #state{stage='undefined'}) ->
    lager:debug("fax worker ~p terminated on undefined stage: ~p", [self(), _Reason]);
terminate('normal' = _Reason, #state{stage=Stage, job_id=JobId, controller=CtrlQ, queue_name=Q}) ->
    lager:debug("fax worker ~p terminating on stage ~s with reason : ~p", [self(), Stage, _Reason]),
    send_control_status(CtrlQ, Q, JobId, ?FAX_END),
    lager:debug("fax worker ~p terminated on stage ~s with reason : ~p", [self(), Stage, _Reason]);
terminate(_Reason, #state{job=JObj, stage=Stage, job_id=JobId, controller=CtrlQ, queue_name=Q}) ->
    lager:debug("fax worker ~p terminating on stage ~s with reason : ~p", [self(), Stage, _Reason]),
    JObj1 = kz_json:set_value(<<"retries">>, 0, JObj),
    _ = release_failed_job('uncontrolled_termination', 'undefined', JObj1),
    send_control_status(CtrlQ, Q, JobId, ?FAX_END),
    lager:debug("fax worker ~p terminated on stage ~s with reason : ~p", [self(), Stage, _Reason]).

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
-spec attempt_to_acquire_job(ne_binary(), ne_binary()) ->
                                    {'ok', kz_json:object()} |
                                    {'error', any()}.
-spec attempt_to_acquire_job(kz_json:object(), ne_binary(), api_binary()) ->
                                    {'ok', kz_json:object()} |
                                    {'error', any()}.
attempt_to_acquire_job(Id, Q) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, Id) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            attempt_to_acquire_job(JObj, Q, kz_json:get_value(<<"pvt_job_status">>, JObj))
    end.

attempt_to_acquire_job(JObj, Q, <<"pending">>) ->
    kz_datamgr:save_doc(?KZ_FAXES_DB
                       ,kz_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                           ,{<<"pvt_job_node">>, kz_util:to_binary(node())}
                                           ,{<<"pvt_modified">>, kz_util:current_tstamp()}
                                           ,{<<"pvt_queue">>, Q}
                                           ]
                                          ,JObj
                                          )
                       ,[{'rev', kz_doc:revision(JObj)}]
                       );
attempt_to_acquire_job(JObj, _Q, Status) ->
    lager:debug("job not in an available status: ~s : ~p", [Status, JObj]),
    {'error', 'job_not_available'}.

-spec release_failed_job(atom(), any(), kz_json:object()) -> 'failure'.
release_failed_job('fetch_failed', Status, JObj) ->
    Msg = <<"could not retrieve file, http response ~p", (integer_to_binary(Status))/binary>>,
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
release_failed_job('fetch_error', {'conn_failed', _}, JObj) ->
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 0}
             ,{<<"result_text">>, <<"could not connect to document URL">>}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ,{<<"fax_bad_rows">>, 0}
             ,{<<"fax_speed">>, 0}
             ,{<<"fax_receiver_id">>, <<>>}
             ,{<<"fax_error_correction">>, 'false'}
             ],
    release_job(Result, JObj);
release_failed_job('fetch_error', {Cause, _}, JObj) ->
    Msg = kz_util:to_binary(io_lib:format("could not connect to document URL: ~s", [Cause])),
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
release_failed_job('fetch_error', Error, JObj) ->
    Msg = kz_util:to_binary(io_lib:format("could not connect to document URL: ~s", [Error])),
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
             ,{<<"result_code">>, kz_util:to_integer(Code)}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    KVs = [{[<<"Application-Data">>, <<"Fax-Result-Text">>], Msg}],
    release_job(Result, JObj, kz_json:set_values(KVs, Resp));
release_failed_job('invalid_number', Number, JObj) ->
    Msg = kz_util:to_binary(io_lib:format("invalid fax number: ~s", [Number])),
    Result = [{<<"success">>, 'false'}
             ,{<<"result_code">>, 400}
             ,{<<"result_text">>, Msg}
             ,{<<"pages_sent">>, 0}
             ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj);
release_failed_job('invalid_cid', Number, JObj) ->
    Msg = kz_util:to_binary(io_lib:format("invalid fax cid number: ~s", [Number])),
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
release_failed_job('channel_destroy', Resp, JObj) ->
    Result = [{<<"success">>, 'false'}
              | fax_util:collect_channel_props(Resp)
             ],
    release_job(Result, JObj, Resp);
release_failed_job('fax_result', Resp, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:487">>),
    Result = props:filter_undefined(
               [{<<"time_elapsed">>, elapsed_time(JObj)}
               ,{<<"result_code">>, kz_util:to_integer(Code)}
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

-spec release_successful_job(kz_json:object(), kz_json:object()) -> 'ok'.
release_successful_job(Resp, JObj) ->
    <<"sip:", Code/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:200">>),
    Result = props:filter_undefined(
               [{<<"time_elapsed">>, elapsed_time(JObj)}
               ,{<<"result_code">>, kz_util:to_integer(Code)}
               ,{<<"result_cause">>, kz_json:get_value(<<"Hangup-Cause">>, Resp)}
               ,{<<"pvt_delivered_date">>,
                 case kz_json:is_true([<<"Application-Data">>, <<"Fax-Success">>], Resp) of
                     'true' -> kz_util:current_tstamp();
                     'false' -> 'undefined'
                 end
                }
                | fax_util:fax_properties(kz_json:get_value(<<"Application-Data">>, Resp, Resp))
               ]),
    release_job(Result, JObj, Resp).

-spec release_job(kz_proplist(), kz_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj) ->
    release_job(Result, JObj, kz_json:new()).

-spec release_job(kz_proplist(), kz_json:object(), kz_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj, Resp) ->
    Success = props:is_true(<<"success">>, Result, 'false'),
    Updaters = [fun(J) -> kz_json:set_value(<<"tx_result">>, kz_json:from_list(Result), J) end
               ,fun(J) -> kz_json:delete_key(<<"pvt_queue">>, J) end
               ,fun apply_reschedule_logic/1
               ,fun(J) ->
                        Attempts = kz_json:get_integer_value(<<"attempts">>, J, 0),
                        Retries = kz_json:get_integer_value(<<"retries">>, J, 1),
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
               ,fun(J) ->
                        Attempts = kz_json:get_integer_value(<<"attempts">>, J, 0),
                        kz_json:set_value(<<"attempts">>, Attempts + 1, J)
                end
               ],
    Update = lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters),
    {'ok', Saved} = kz_datamgr:ensure_saved(?KZ_FAXES_DB, Update),
    maybe_notify(Result, Saved, Resp, kz_json:get_value(<<"pvt_job_status">>, Saved)),
    case Success of 'true' -> 'ok'; 'false' -> 'failure' end.

-spec apply_reschedule_logic(kz_json:object()) -> kz_json:object().
apply_reschedule_logic(JObj) ->
    Map = kapps_config:get(?CONFIG_CAT, <<"reschedule">>, kz_json:new()),
    case apply_reschedule_rules(kz_json:get_values(Map), set_default_update_fields(JObj)) of
        {'no_rules', JObj2} ->
            lager:debug("no rules applied in fax reschedule logic"),
            JObj2;
        {'ok', JObj2} ->
            lager:debug("rule '~s' applied in fax reschedule logic",
                        [kz_json:get_value(<<"reschedule_rule">>, JObj2)]
                       ),
            JObj2
    end.

-spec apply_reschedule_rules({kz_json:json_terms(), kz_json:keys()}, kz_json:object()) ->
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
                                         ], JObj),
            {'ok', NewJObj};
        'false' ->
            apply_reschedule_rules({Rules, Keys}, JObj)
    end.

-spec get_attempt_value(api_binary() | integer()) -> integer().
get_attempt_value(X) when is_integer(X) -> X;
get_attempt_value('undefined') -> -1;
get_attempt_value(<<"any">>) -> -1;
get_attempt_value(X) -> kz_util:to_integer(X).

-spec set_default_update_fields(kz_json:object()) -> kz_json:object().
set_default_update_fields(JObj) ->
    kz_json:set_values([{<<"pvt_modified">>, kz_util:current_tstamp()}
                       ,{<<"retry_after">>, ?DEFAULT_RETRY_PERIOD}
                       ]
                      ,JObj
                      ).

-spec maybe_notify(kz_proplist(), kz_json:object(), kz_json:object(), ne_binary()) -> any().
maybe_notify(_Result, JObj, Resp, <<"completed">>) ->
    Message = notify_fields(move_doc(JObj), Resp),
    kapi_notifications:publish_fax_outbound(Message);
maybe_notify(_Result, JObj, Resp, <<"failed">>) ->
    Message = [{<<"Fax-Error">>, fax_error(Resp)}
               | notify_fields(move_doc(JObj), Resp)
              ],
    kapi_notifications:publish_fax_outbound_error(props:filter_undefined(Message));
maybe_notify(_Result, _JObj, _Resp, Status) ->
    lager:debug("notify Status ~p not handled",[Status]).

move_doc(JObj) ->
    FromId = kz_doc:id(JObj),
    {Year, Month, _D} = kz_util:to_date(kz_doc:created(JObj)),
    FromDB = kz_doc:account_db(JObj),
    AccountId = kz_doc:account_id(JObj),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    kazoo_modb:create(AccountMODb),
    ToDB = kz_util:format_account_modb(AccountMODb, 'encoded'),
    ToId = ?MATCH_MODB_PREFIX(kz_util:to_binary(Year), kz_util:pad_month(Month), FromId),
    Options = ['override_existing_document'],
    {'ok', Doc} = kz_datamgr:move_doc(FromDB, {<<"fax">>, FromId}, ToDB, ToId, Options),
    Doc.

-spec fax_error(kz_json:object()) -> api_binary().
fax_error(JObj) ->
    kz_json:get_value([<<"Application-Data">>, <<"Fax-Result-Text">>]
                     ,JObj
                     ).

-spec notify_fields(kz_json:object(), kz_json:object()) -> kz_proplist().
notify_fields(JObj, Resp) ->
    <<"sip:", HangupCode/binary>> = kz_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:0">>),
    HangupCause = kz_json:get_value(<<"Hangup-Cause">>, Resp),
    FaxFields = [{<<"Fax-Hangup-Code">>, kz_util:to_integer(HangupCode)}
                ,{<<"Fax-Hangup-Cause">>, HangupCause}
                 | fax_fields(kz_json:get_value(<<"Application-Data">>, Resp))
                ],

    ToNumber = kz_util:to_binary(kz_json:get_value(<<"to_number">>, JObj)),
    ToName = kz_util:to_binary(kz_json:get_value(<<"to_name">>, JObj, ToNumber)),
    Notify = [E || E <- kz_json:get_value([<<"notifications">>,<<"email">>,<<"send_to">>], JObj, [])
                       ,not kz_util:is_empty(E)
             ],

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
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec fax_fields(api_object()) -> kz_proplist().
fax_fields('undefined') -> [];
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- kz_json:to_proplist(JObj)].

-spec elapsed_time(kz_json:object()) -> non_neg_integer().
elapsed_time(JObj) ->
    Now = kz_util:current_tstamp(),
    Created = kz_doc:created(JObj, Now),
    Now - Created.

-spec fetch_document(kz_json:object()) -> kz_http:ret().
fetch_document(JObj) ->
    case kz_doc:attachment_names(JObj) of
        [] -> fetch_document_from_url(JObj);
        AttachmentNames -> fetch_document_from_attachment(JObj, AttachmentNames)
    end.

-spec fetch_document_from_attachment(kz_json:object(), ne_binaries()) -> kz_http:ret().
fetch_document_from_attachment(JObj, [AttachmentName|_]) ->
    Extension = filename:extension(AttachmentName),
    DefaultContentType = kz_mime:from_extension(Extension),
    ContentType = kz_doc:attachment_content_type(JObj, AttachmentName, DefaultContentType),

    Props = [{"Content-Type", ContentType}],
    {'ok', Contents} = kz_datamgr:fetch_attachment(?KZ_FAXES_DB, kz_doc:id(JObj), AttachmentName),
    {'ok', 200, Props, Contents}.

-spec fetch_document_from_url(kz_json:object()) -> kz_http:ret().
fetch_document_from_url(JObj) ->
    FetchRequest = kz_json:get_value(<<"document">>, JObj),
    Url = kz_json:get_string_value(<<"url">>, FetchRequest),
    Method = kz_util:to_atom(kz_json:get_value(<<"method">>, FetchRequest, <<"get">>), 'true'),
    Headers = props:filter_undefined(
                [{"Host", kz_json:get_string_value(<<"host">>, FetchRequest)}
                ,{"Referer", kz_json:get_string_value(<<"referer">>, FetchRequest)}
                ,{"User-Agent", kz_json:get_string_value(<<"user_agent">>, FetchRequest, kz_util:to_list(node()))}
                ,{"Content-Type", kz_json:get_string_value(<<"content_type">>, FetchRequest, <<"text/plain">>)}
                ]),
    Body = kz_json:get_string_value(<<"content">>, FetchRequest, ""),
    lager:debug("making ~s request to '~s'", [Method, Url]),
    kz_http:req(Method, Url, Headers, Body).

-spec prepare_contents(ne_binary(), kz_proplist(), ne_binary()) ->
                              {'ok', ne_binary()} |
                              {'error', ne_binary()}.
prepare_contents(JobId, RespHeaders, RespContent) ->
    lager:debug("preparing fax contents", []),
    TmpDir = kapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
    case fax_util:normalize_content_type(props:get_value("Content-Type", RespHeaders, <<"application/octet-stream">>)) of
        <<"image/tiff">> ->
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            kz_util:write_file(OutputFile, RespContent),
            {'ok', OutputFile};
        <<"application/pdf">> ->
            InputFile = list_to_binary([TmpDir, JobId, ".pdf"]),
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            kz_util:write_file(InputFile, RespContent),
            ConvertCmd = kapps_config:get_binary(?CONFIG_CAT
                                                ,<<"conversion_pdf_command">>
                                                ,kapps_config:get_binary(?CONFIG_CAT
                                                                        ,<<"conversion_command">>
                                                                        ,?CONVERT_PDF_CMD
                                                                        )
                                                ),
            Cmd = io_lib:format(ConvertCmd, [OutputFile, InputFile]),
            lager:debug("attempting to convert pdf: ~s", [Cmd]),
            try "success" = os:cmd(Cmd) of
                "success" ->
                    {'ok', OutputFile}
            catch
                Type:Exception ->
                    lager:debug("could not covert file: ~p:~p", [Type, Exception]),
                    {'error', <<"can not convert file, try uploading a tiff">>}
            end;
        <<"image/", SubType/binary>> ->
            InputFile = list_to_binary([TmpDir, JobId, ".", SubType]),
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            kz_util:write_file(InputFile, RespContent),
            ConvertCmd = kapps_config:get_binary(?CONFIG_CAT, <<"conversion_image_command">>, ?CONVERT_IMAGE_CMD),
            Cmd = io_lib:format(ConvertCmd, [InputFile, OutputFile]),
            lager:debug("attempting to convert ~s: ~s", [SubType, Cmd]),
            try "success" = os:cmd(Cmd) of
                "success" ->
                    {'ok', OutputFile}
            catch
                Type:Exception ->
                    lager:debug("could not covert file: ~p:~p", [Type, Exception]),
                    {'error', <<"can not convert file, try uploading a tiff">>}
            end;
        <<?OPENXML_MIME_PREFIX, _/binary>> = CT ->
            convert_openoffice_document(CT, TmpDir, JobId, RespContent);
        <<?OPENOFFICE_MIME_PREFIX, _/binary>> = CT ->
            convert_openoffice_document(CT, TmpDir, JobId, RespContent);
        CT when ?OPENOFFICE_COMPATIBLE(CT) ->
            convert_openoffice_document(CT, TmpDir, JobId, RespContent);
        Else ->
            lager:debug("unsupported file type: ~p", [Else]),
            {'error', list_to_binary(["file type '", Else, "' is unsupported"])}
    end.

-spec convert_openoffice_document(ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                                         {'ok', ne_binary()} |
                                         {'error', ne_binary()}.
convert_openoffice_document(CT, TmpDir, JobId, RespContent) ->
    Extension = kz_mime:to_extension(CT),
    InputFile = list_to_binary([TmpDir, JobId, ".", Extension]),
    OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
    kz_util:write_file(InputFile, RespContent),
    ConvertCmd = kapps_config:get_binary(?CONFIG_CAT, <<"conversion_openoffice_document_command">>, ?CONVERT_OO_DOC_CMD),
    OpenOfficeServer = kapps_config:get_binary(?CONFIG_CAT, <<"openoffice_server">>, <<"'socket,host=localhost,port=2002;urp;StarOffice.ComponentContext'">>),
    Cmd = io_lib:format(ConvertCmd, [OpenOfficeServer, InputFile, OutputFile]),
    lager:debug("attemting to convert openoffice document: ~s", [Cmd]),
    try "success" = os:cmd(Cmd) of
        "success" ->
            {'ok', OutputFile}
    catch
        Type:Exception ->
            lager:debug("could not covert file: ~p:~p", [Type, Exception]),
            {'error', <<"can not convert file, try uploading a tiff">>}
    end.

-spec get_sizes(ne_binary()) -> {integer(), non_neg_integer()}.
get_sizes(OutputFile) when is_binary(OutputFile) ->
    CmdCount = kapps_config:get_binary(?CONFIG_CAT, <<"count_pages_command">>, ?COUNT_PAGES_CMD),
    Cmd = io_lib:format(CmdCount, [OutputFile]),
    NumberOfPages = try Result = os:cmd(kz_util:to_list(Cmd)),
                         kz_util:to_integer(Result)
                    catch
                        _:_ -> 0
                    end,
    FileSize = filelib:file_size(kz_util:to_list(OutputFile)),
    {NumberOfPages, FileSize}.

-spec send_fax(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
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

-spec send_fax(ne_binary(), kz_json:object(), ne_binary(), binary() | 'undefined') -> 'ok'.
send_fax(_JobId, _JObj, _Q, 'undefined') ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(undefined)">>});
send_fax(_JobId, _JObj, _Q, <<>>) ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(empty)">>});
send_fax(JobId, JObj, Q, ToDID) ->
    IgnoreEarlyMedia = kz_util:to_binary(kapps_config:get_is_true(?CONFIG_CAT, <<"ignore_early_media">>, 'false')),
    ToNumber = kz_util:to_binary(kz_json:get_value(<<"to_number">>, JObj)),
    ToName = kz_util:to_binary(kz_json:get_value(<<"to_name">>, JObj, ToNumber)),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    ETimeout = kz_util:to_binary(kapps_config:get_integer(?CONFIG_CAT, <<"endpoint_timeout">>, 40)),
    AccountId =  kz_doc:account_id(JObj),
    AccountRealm = kz_util:get_account_realm(AccountId),
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
                ,{<<"Outbound-Call-ID">>, CallId}
                ,{<<"Bypass-E164">>, kz_json:is_true(<<"bypass_e164">>, JObj)}
                 | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("sending fax originate request ~s with call-id ~s", [JobId, CallId]),
    kapi_offnet_resource:publish_req(Request).

-spec get_hunt_account_id(ne_binary()) -> api_binary().
get_hunt_account_id(AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Options = [{'key', <<"no_match">>}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, ?CALLFLOW_LIST, Options) of
        {'ok', [JObj]} -> maybe_hunt_account_id(kz_json:get_value([<<"doc">>, <<"flow">>], JObj), AccountId);
        _ -> 'undefined'
    end.

-spec maybe_hunt_account_id(api_object(), ne_binary()) -> api_binary().
maybe_hunt_account_id('undefined', _) -> 'undefined';
maybe_hunt_account_id(JObj, AccountId) ->
    case kz_json:get_value(<<"module">>, JObj) of
        <<"resources">> ->
            kz_json:get_value([<<"data">>, <<"hunt_account_id">>], JObj, AccountId);
        _ ->
            maybe_hunt_account_id(kz_json:get_value([<<"children">>, <<"_">>], JObj), AccountId)
    end.

-spec resource_ccvs(ne_binary()) -> kz_json:object().
resource_ccvs(JobId) ->
    kz_json:from_list([{<<"Authorizing-ID">>, JobId}
                      ,{<<"Authorizing-Type">>, <<"outbound_fax">>}
                      ]).

-spec get_did(kz_json:object()) -> api_binary().
get_did(JObj) ->
    case kz_json:is_true(<<"bypass_e164">>, JObj, 'false') of
        'true' -> kz_json:get_value(<<"to_number">>, JObj);
        'false' -> knm_converters:normalize(kz_json:get_value(<<"to_number">>, JObj))
    end.

-spec get_proxy_url(ne_binary()) -> ne_binary().
get_proxy_url(JobId) ->
    Hostname = kz_network_utils:get_hostname(),
    Port = kapps_config:get_binary(?CONFIG_CAT, <<"port">>),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId, ".tiff"]).

-spec send_status(state(), ne_binary()) -> any().
send_status(State, Status) ->
    send_status(State, Status, ?FAX_SEND, 'undefined').

-spec send_error_status(state(), ne_binary()) -> any().
send_error_status(State, Status) ->
    send_status(State, Status, ?FAX_ERROR, 'undefined').

-spec send_status(state(), ne_binary(), api_object()) -> any().
send_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_SEND, FaxInfo).

-spec send_status(state(), ne_binary(), ne_binary(), api_object()) -> any().
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

-spec send_reply_status(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), api_object()) -> 'ok'.
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

-spec send_control_status(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_control_status(CtrlQ, Q, JobId, FaxState) ->
    Payload = [{<<"Job-ID">>, JobId}
              ,{<<"Fax-State">>, FaxState}
               | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_fax:publish_targeted_status(CtrlQ, P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_start_job(kz_json:object(), kz_proplist()) -> 'ok'.
handle_start_job(JObj, _Props) ->
    'true' = kapi_fax:start_job_v(JObj),
    fax_worker_sup:start_fax_job(JObj).
