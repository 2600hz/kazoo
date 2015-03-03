%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(fax_worker).

-behaviour(gen_listener).

-export([start_link/1]).
-export([handle_tx_resp/2
         ,handle_fax_event/2
         ,handle_channel_destroy/2
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

-record(state, {queue_name :: api_binary()
                ,pool :: api_pid()
                ,job_id :: api_binary()
                ,job :: api_object()
                ,account_id :: api_binary()
                ,status :: binary()
                ,fax_status :: api_object()
                ,pages  :: integer()
                ,page = 0  ::integer()
                ,file :: api_binary()
               }).
-type state() :: #state{}.


-define(BINDINGS, [
                   {'self', []}
                  ,{'fax', [{'restrict_to', ['query_status']}]}
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
                     ,{{?MODULE, 'handle_job_status_query'}
                       ,[{<<"fax">>, <<"query_status">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DEFAULT_RETRY_PERIOD, whapps_config:get_integer(?CONFIG_CAT, <<"default_retry_period">>, 300)).
-define(DEFAULT_RETRY_COUNT, whapps_config:get_integer(?CONFIG_CAT, <<"default_retry_count">>, 3)).
-define(DEFAULT_COMPARE_FIELD, whapps_config:get_binary(?CONFIG_CAT, <<"default_compare_field">>, <<"result_cause">>)).

-define(COUNT_PAGES_CMD, <<"echo -n `tiffinfo ~s | grep 'Page Number' | grep -c 'P'`">>).
-define(CONVERT_PDF_CMD, <<"/usr/bin/gs -q -r204x98 -g1728x1078 -dNOPAUSE -dBATCH -dSAFER -sDEVICE=tiffg3 -sOutputFile=~s -- ~s &> /dev/null && echo -n \"success\"">>).


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
start_link(_) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

handle_tx_resp(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'tx_resp', wh_json:get_value(<<"Msg-ID">>, JObj), JObj}).


handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = wh_json:get_value([<<"Custom-Channel-Vars">>,<<"Authorizing-ID">>], JObj),
    Event = wh_json:get_value(<<"Application-Event">>, JObj),
    gen_server:cast(Srv, {'fax_status', Event , JobId, JObj}).

-spec handle_channel_destroy(wh_json:object(), wh_proplist()) -> any().
handle_channel_destroy(JObj, Props) ->
    Srv = props:get_value('server', Props),
    JobId = wh_json:get_value([<<"Custom-Channel-Vars">>,<<"Authorizing-ID">>], JObj),
    gen_server:cast(Srv, {'channel_destroy', JobId, JObj}).

-spec handle_job_status_query(wh_json:object(), wh_proplist()) -> any().
handle_job_status_query(JObj, Props) ->
    'true' = wapi_fax:query_status_v(JObj),
    Srv = props:get_value('server', Props),
    JobId = wh_json:get_value(<<"Job-ID">>, JObj),
    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
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
init([]) ->
    {'ok', #state{}}.

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
handle_cast({'tx_resp', JobId, JObj}, #state{job_id=JobId
                                            ,job=Job
                                            ,pool=Pid
                                            }=State) ->
    case wh_json:get_value(<<"Response-Message">>, JObj) of
        <<"SUCCESS">> ->
            lager:debug("received successful attempt to originate fax, continue processing"),
            send_status(State, <<"received successful attempt to originate fax">>),
            {'noreply', State#state{status = <<"negotiating">>}};
        _Else ->
            lager:debug("received failed attempt to tx fax, releasing job: ~s", [_Else]),
            send_error_status(State, wh_json:get_value(<<"Error-Message">>, JObj)),
            release_failed_job('tx_resp', JObj, Job),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_cast({'tx_resp', JobId2, _}, #state{job_id=JobId}=State) ->
    lager:debug("received txresp for ~s but this JobId is ~s",[JobId2, JobId]),
    {'noreply', State};
handle_cast({'channel_destroy', JobId, JObj}, #state{job_id=JobId
                                                    ,job=Job
                                                    ,pool=Pid
                                            }=State) ->
    lager:debug("received channel destroy for ~s",[JobId]),
    send_error_status(State, wh_json:get_value(<<"Hangup-Cause">>, JObj)),
    _ = release_failed_job('channel_destroy', JObj, Job),
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', reset(State)};
handle_cast({'channel_destroy', JobId2, _JObj}, #state{job_id=JobId}=State) ->
    lager:debug("received channel destroy for ~s but this JobId is ~s",[JobId2, JobId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JobId, JObj}, State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    TransferRate = wh_json:get_integer_value(<<"Fax-Transfer-Rate">>, Data, 1),
    lager:debug("fax status - negociate result - ~s : ~p",[JobId, TransferRate]),
    Status = list_to_binary(["Fax negotiated at ", wh_util:to_list(TransferRate)]),
    send_status(State, Status, Data),
    {'noreply', State#state{status=Status, fax_status=Data}};
handle_cast({'fax_status', <<"pageresult">>, JobId, JObj}
           , #state{pages=Pages}=State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    Page = wh_json:get_integer_value(<<"Fax-Transferred-Pages">>, Data, 0),
    lager:debug("fax status - page result - ~s : ~p : ~p"
                ,[JobId, Page, wh_util:current_tstamp()]),
    Status = list_to_binary(["Sent Page ", wh_util:to_list(Page), " of ", wh_util:to_list(Pages)]),
    send_status(State#state{page=Page}, Status, Data),
    {'noreply', State#state{page=Page, status=Status, fax_status=Data}};
handle_cast({'fax_status', <<"result">>, JobId, JObj}
           , #state{job_id=JobId,job=Job,pool=Pid}=State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    case wh_json:is_true([<<"Application-Data">>, <<"Fax-Success">>], JObj) of
        'true' ->
            send_status(State, <<"Fax Successfuly sent">>, ?FAX_END, Data),
            release_successful_job(JObj, Job);
        'false' ->
            send_status(State, <<"Error sending fax">>, ?FAX_ERROR, Data),
            release_failed_job('fax_result', JObj, Job)
    end,
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', reset(State)};
handle_cast({'fax_status', Event, JobId, _}, State) ->
    lager:debug("fax status ~s - ~s event not handled",[JobId, Event]),
    {'noreply', State};
handle_cast({'query_status', JobId, Queue, MsgId, _JObj}
           , #state{status=Status,job_id=JobId, account_id=AccountId, fax_status=Data}=State) ->
    lager:debug("query fax status ~s handled by this queue",[JobId]),
    send_reply_status(Queue, MsgId, JobId, Status, AccountId, Data),
    {'noreply', State};
handle_cast({'query_status', JobId, Queue, MsgId, _JObj}, State) ->
    lager:debug("query fax status ~s not handled by this queue",[JobId]),
    Status = list_to_binary(["Fax ", JobId, " not being processed by this Queue"]),
    send_reply_status(Queue, MsgId, JobId, Status, <<"*">>,'undefined'),
    {'noreply', State};
handle_cast({_, Pid, _}, #state{queue_name='undefined'}=State) when is_pid(Pid) ->
    lager:debug("worker received request with unknown queue name, rejecting", []),
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', State};
handle_cast({_, Pid, _}, #state{job_id=JobId}=State) when is_binary(JobId), is_pid(Pid) ->
    lager:debug("worker received request while still processing a job, rejecting", []),
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', State};
handle_cast({'attempt_transmission', Pid, Job}, #state{queue_name=Q}=State) ->
    JobId = wh_json:get_value(<<"id">>, Job),
    put('callid', JobId),
    case attempt_to_acquire_job(JobId, Q) of
        {'ok', JObj} ->
            lager:debug("acquired job ~s", [JobId]),
            AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
            Status = <<"preparing">>,
            NewState = State#state{job_id=JobId
                                    ,pool=Pid
                                    ,job=JObj
                                    ,account_id=AccountId
                                    ,status=Status
                                    ,page=0
                                    ,fax_status=wh_json:new()
                                   },
            send_status(NewState, <<"job acquired">>, ?FAX_START, 'undefined'),
            gen_server:cast(self(), 'prepare_job'),
            {'noreply', NewState};
        {'error', _Reason} ->
            lager:debug("failed to acquire job ~s: ~p", [JobId, _Reason]),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_cast('prepare_job', #state{job_id=JobId
                                  ,job=JObj
                                  ,pool=Pid
                                 }=State) ->
    send_status(State, <<"fetching document to send">>, ?FAX_PREPARE, 'undefined'),
    case fetch_document(JObj) of
        {'ok', "200", RespHeaders, RespContent} ->
            send_status(State, <<"preparing document to send">>, ?FAX_PREPARE, 'undefined'),
            case prepare_contents(JobId, RespHeaders, RespContent) of
                {'error', Cause} ->
                    send_error_status(State, Cause),
                    release_failed_job('bad_file', Cause, JObj),
                    gen_server:cast(Pid, {'job_complete', self()}),
                    {'noreply', reset(State)};
                {'ok', OutputFile} ->
                    gen_server:cast(self(), 'count_pages'),
                    {'noreply', State#state{file=OutputFile}}
            end;
        {'ok', Status, _, _} ->
            lager:debug("failed to fetch file for job: http response ~p", [Status]),
            _ = send_error_status(State, Status),
            release_failed_job('fetch_failed', Status, JObj),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)};
        {'error', Reason} ->
            lager:debug("failed to fetch file for job: ~p", [Reason]),
            send_error_status(State, <<"failed to fetch file for job">>),
            release_failed_job('fetch_error', Reason, JObj),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_cast('count_pages', #state{file=File
                                  ,job=JObj
                                 }=State) ->
    {NumberOfPages, FileSize} = get_sizes(File),
    Values = [{<<"pvt_pages">>, NumberOfPages}
              ,{<<"pvt_size">>, FileSize}
             ],
    gen_server:cast(self(), 'send'),
    {'noreply',State#state{job=wh_json:set_values(Values, JObj),pages=NumberOfPages}};
handle_cast('send', #state{job_id=JobId
                           ,job=JObj
                           ,queue_name=Q
                          }=State) ->
    send_status(State, <<"ready to send">>, ?FAX_SEND, 'undefined'),
    send_fax(JobId, JObj, Q),
    {'noreply', State};
handle_cast({'error', 'invalid_number', Number}, #state{job=JObj, pool=Pid}=State) ->
    send_error_status(State, <<"invalid fax number">>),
    release_failed_job('invalid_number', Number, JObj),
    gen_server:cast(Pid, {'job_complete', self()}),
     {'noreply', reset(State)};
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State#state{queue_name=QueueName}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
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
handle_info('job_timeout', #state{job=JObj}=State) ->
    release_failed_job('job_timeout', 'undefined', JObj),
    {'noreply', reset(State)};
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
-spec attempt_to_acquire_job(ne_binary(), ne_binary()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', term()}.
-spec attempt_to_acquire_job(wh_json:object()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', term()}.
attempt_to_acquire_job(Id, Q) ->
    case couch_mgr:open_doc(?WH_FAXES, Id) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            attempt_to_acquire_job(wh_json:set_value(<<"pvt_queue">>, Q, JObj))
    end.

attempt_to_acquire_job(JObj) ->
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        <<"pending">> ->
            couch_mgr:save_doc(?WH_FAXES
                               ,wh_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                                    ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
                                                    ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                   ]
                                                   ,JObj
                                                  )
                               ,[{'rev', wh_doc:revision(JObj)}]
                              );
        _Else ->
            lager:debug("job not in an available status: ~s", [_Else]),
            {'error', 'job_not_available'}
    end.

-spec release_failed_job(atom(), any(), wh_json:object()) -> 'failure'.
release_failed_job('fetch_failed', Status, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("could not retrieve file, http response ~p", [Status])),
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
    Msg = wh_util:to_binary(io_lib:format("could not connect to document URL: ~s", [Cause])),
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
    Msg = wh_json:get_value(<<"Error-Message">>, Resp),
    <<"sip:", Code/binary>> = wh_json:get_value(<<"Response-Code">>, Resp, <<"sip:500">>),
    Result = [{<<"success">>, 'false'}
              ,{<<"result_code">>, wh_util:to_integer(Code)}
              ,{<<"result_text">>, Msg}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj, Resp);
release_failed_job('invalid_number', Number, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("invalid fax number: ~s", [Number])),
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
    <<"sip:", Code/binary>> = wh_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:487">>),
    Result = props:filter_undefined(
               [{<<"time_elapsed">>, elapsed_time(JObj)}
                ,{<<"result_code">>, wh_util:to_integer(Code)}
                ,{<<"result_cause">>, wh_json:get_value(<<"Hangup-Cause">>, Resp)}
                ,{<<"success">>, 'false'}
                | fax_util:fax_properties(wh_json:get_value(<<"Application-Data">>, Resp, Resp))
               ]),
    release_job(Result, JObj, Resp);
release_failed_job('job_timeout', _Error, JObj) ->
    Result = [{<<"success">>, 'false'}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, <<"fax job timed out">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, 'false'}
             ],
    release_job(Result, JObj).

-spec release_successful_job(wh_json:object(), wh_json:object()) -> 'ok'.
release_successful_job(Resp, JObj) ->
    <<"sip:", Code/binary>> = wh_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:200">>),
    Result = props:filter_undefined([
               {<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"result_code">>, wh_util:to_integer(Code)}
              ,{<<"result_cause">>, wh_json:get_value(<<"Hangup-Cause">>, Resp)}
              ,{<<"pvt_delivered_date">>,
                case wh_json:is_true([<<"Application-Data">>, <<"Fax-Success">>], Resp) of
                    'true' -> wh_util:current_tstamp();
                    'false' -> 'undefined'
                end
               }
              | fax_util:fax_properties(wh_json:get_value(<<"Application-Data">>, Resp, Resp))
             ]),
    release_job(Result, JObj, Resp).

-spec release_job(wh_proplist(), wh_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj) ->
    release_job(Result, JObj, wh_json:new()).

-spec release_job(wh_proplist(), wh_json:object(), wh_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj, Resp) ->
    Success = props:is_true(<<"success">>, Result, 'false'),
    Updaters = [ fun(J) -> wh_json:set_value(<<"tx_result">>, wh_json:from_list(Result), J) end
                ,fun(J) -> wh_json:delete_key(<<"pvt_queue">>, J) end
                ,fun(J) -> apply_reschedule_logic(J) end
                ,fun(J) ->
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         Retries = wh_json:get_integer_value(<<"retries">>, J, 1),
                         case Retries - Attempts >= 1 of
                             _ when Success ->
                                 lager:debug("releasing job with status: completed"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"completed">>, J);
                             'true' ->
                                 lager:debug("releasing job with status: pending"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"pending">>, J);
                             'false' ->
                                 lager:debug("releasing job with status: failed"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"failed">>, J)
                         end
                 end
                ,fun(J) ->
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         wh_json:set_value(<<"attempts">>, Attempts + 1, J)
                 end
               ],
    Update = lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters),
    couch_mgr:ensure_saved(?WH_FAXES, Update),
    maybe_notify(Result, JObj, Resp, wh_json:get_value(<<"pvt_job_status">>, Update)),
    case Success of 'true' -> 'ok'; 'false' -> 'failure' end.

-spec apply_reschedule_logic(wh_json:object()) -> wh_json:object().
apply_reschedule_logic(JObj) ->
    Map = whapps_config:get(?CONFIG_CAT, <<"reschedule">>, wh_json:new()),
    case apply_reschedule_rules(wh_json:get_values(Map), set_default_update_fields(JObj)) of
        {'no_rules', JObj2} ->
            lager:debug("no rules applied in fax reschedule logic"),
            JObj2;
        {'ok', JObj2} ->
            lager:debug("rule '~s' applied in fax reschedule logic",
                        [wh_json:get_value(<<"reschedule_rule">>, JObj2)]),
            JObj2
    end.

-spec apply_reschedule_rules({wh_json:json_terms(), wh_json:keys()}, wh_json:object()) ->
                                    {'ok', wh_json:object()} |
                                    {'no_rules', wh_json:object()}.
apply_reschedule_rules({[], _}, JObj) -> {'no_rules', JObj};
apply_reschedule_rules({[Rule | Rules], [Key | Keys]}, JObj) ->
    Attempts = wh_json:get_integer_value(<<"attempts">>, JObj, 0),
    Result = wh_json:get_value(<<"tx_result">>, JObj, wh_json:new()),
    Field = wh_json:get_value(<<"compare-field">>, Rule, ?DEFAULT_COMPARE_FIELD),
    ValueList = wh_json:get_value(<<"compare-values">>, Rule, []),
    ResultValue = wh_json:get_value(Field, Result),
    Attempt = get_attempt_value(wh_json:get_value(<<"attempt">>, Rule)),
    RetryAfter = wh_json:get_integer_value(<<"retry-after">>, Rule, ?DEFAULT_RETRY_PERIOD),
    Retries = wh_json:get_integer_value(<<"retries">>, Rule, ?DEFAULT_RETRY_COUNT),
    NewRetries = wh_json:get_integer_value(<<"new-retry-count">>, Rule, Retries),
    case (Attempt =:= Attempts orelse Attempt =:= -1)
        andalso lists:member(ResultValue, ValueList)
    of
        'true' ->
            NewJObj = wh_json:set_values([{<<"retry_after">>, RetryAfter}
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
get_attempt_value(X) -> wh_util:to_integer(X).

-spec set_default_update_fields(wh_json:object()) -> wh_json:object().
set_default_update_fields(JObj) ->
    wh_json:set_values([{<<"pvt_modified">>, wh_util:current_tstamp()}
                       ,{<<"retry_after">>, ?DEFAULT_RETRY_PERIOD}
                       ], JObj).


-spec maybe_notify(wh_proplist(), wh_json:object(), wh_json:object(), ne_binary()) -> any().
maybe_notify(_Result, JObj, Resp, <<"completed">>) ->
    Message = notify_fields(JObj, Resp),
    wapi_notifications:publish_fax_outbound(Message);
maybe_notify(_Result, JObj, Resp, <<"failed">>) ->
    Message = notify_fields(JObj, Resp),
    wapi_notifications:publish_fax_outbound_error(Message);
maybe_notify(_Result, _JObj, _Resp, Status) ->
    lager:debug("notify Status ~p not handled",[Status]).

-spec notify_fields(wh_json:object(), wh_json:object()) -> wh_proplist().
notify_fields(JObj, Resp) ->
    <<"sip:", HangupCode/binary>> = wh_json:get_value(<<"Hangup-Code">>, Resp, <<"sip:0">>),
    HangupCause =  wh_json:get_value(<<"Hangup-Cause">>, Resp),
    FaxFields = [{"Fax-Hangup-Code", wh_util:to_integer(HangupCode)}
                ,{"Fax-Hangup-Cause", HangupCause}
                | fax_fields(wh_json:get_value(<<"Application-Data">>, Resp))],

    ToNumber = wh_util:to_binary(wh_json:get_value(<<"to_number">>, JObj)),
    ToName = wh_util:to_binary(wh_json:get_value(<<"to_name">>, JObj, ToNumber)),
    NotifyTmp = wh_json:get_value([<<"notifications">>,<<"email">>,<<"send_to">>], JObj, []),
    Notify = lists:filter(fun wh_util:is_not_empty/1, NotifyTmp),
    props:filter_empty(
      [{<<"Caller-ID-Name">>, wh_json:get_value(<<"from_name">>, JObj)}
      ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
      ,{<<"Callee-ID-Number">>, ToNumber}
      ,{<<"Callee-ID-Name">>, ToName }
      ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
      ,{<<"Fax-JobId">>, wh_json:get_value(<<"_id">>, JObj)}
      ,{<<"FaxBox-ID">>, wh_json:get_value(<<"faxbox_id">>, JObj)}
      ,{<<"Fax-Notifications">>
        ,wh_json:from_list([{<<"email">>, wh_json:from_list([{<<"send_to">>, Notify}])}])
       }
      ,{<<"Fax-Info">>, wh_json:from_list(FaxFields) }
      ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Resp)}
      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec fax_fields(wh_json:object()) -> wh_proplist().
fax_fields('undefined') ->
    [];
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- wh_json:to_proplist(JObj)].

-spec elapsed_time(wh_json:object()) -> non_neg_integer().
elapsed_time(JObj) ->
    wh_util:current_tstamp() - wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()).

-spec fetch_document(wh_json:object()) ->
                            {'ok', string(), wh_proplist(), ne_binary()} |
                            {'error', term()}.
fetch_document(JObj) ->
    case wh_doc:attachment_names(JObj) of
        [] -> fetch_document_from_url(JObj);
        AttachmentNames -> fetch_document_from_attachment(JObj, AttachmentNames)
    end.

-spec fetch_document_from_attachment(wh_json:object(), ne_binaries()) ->
                                            {'ok', string(), wh_proplist(), ne_binary()} |
                                            {'error', term()}.
fetch_document_from_attachment(JObj, [AttachmentName|_]) ->
    JobId = wh_json:get_value(<<"_id">>, JObj),
    Extension = filename:extension(AttachmentName),
    DefaultContentType = fax_util:extension_to_content_type(Extension),

    ContentType = case wh_doc:attachment_content_type(JObj, AttachmentName) of
                      'undefined' -> DefaultContentType;
                      CT -> CT
                  end,

    Props = [{"Content-Type", ContentType}],
    {'ok', Contents} = couch_mgr:fetch_attachment(?WH_FAXES, JobId, AttachmentName),
    {'ok', "200", Props, Contents}.

-spec fetch_document_from_url(wh_json:object()) ->
                                     {'ok', string(), wh_proplist(), ne_binary()} |
                                     {'error', term()}.
fetch_document_from_url(JObj) ->
    FetchRequest = wh_json:get_value(<<"document">>, JObj),
    Url = wh_json:get_string_value(<<"url">>, FetchRequest),
    Method = wh_util:to_atom(wh_json:get_value(<<"method">>, FetchRequest, <<"get">>), 'true'),
    Headers = props:filter_undefined(
                [{"Host", wh_json:get_string_value(<<"host">>, FetchRequest)}
                 ,{"Referer", wh_json:get_string_value(<<"referer">>, FetchRequest)}
                 ,{"User-Agent", wh_json:get_string_value(<<"user_agent">>, FetchRequest, wh_util:to_list(node()))}
                 ,{"Content-Type", wh_json:get_string_value(<<"content_type">>, FetchRequest, <<"text/plain">>)}
                ]),
    HTTPOptions = [{'response_format', 'binary'}],
    Body = wh_json:get_string_value(<<"content">>, FetchRequest, ""),
    lager:debug("making ~s request to '~s'", [Method, Url]),
    ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions).

-spec prepare_contents(ne_binary(), wh_proplist(), ne_binary()) ->
                              {'ok', ne_binary()} |
                              {'error', ne_binary()}.
prepare_contents(JobId, RespHeaders, RespContent) ->
    lager:debug("preparing fax contents", []),
    TmpDir = whapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
    case normalize_content_type(props:get_value("Content-Type", RespHeaders, <<"application/octet-stream">>)) of
        <<"image/tiff">> ->
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            R = file:write_file(OutputFile, RespContent),
            lager:debug("result of tmp file write: ~s", [R]),
            {'ok', OutputFile};
        <<"application/pdf">> ->
            InputFile = list_to_binary([TmpDir, JobId, ".pdf"]),
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            R = file:write_file(InputFile, RespContent),
            lager:debug("result of tmp file write: ~s", [R]),
            ConvertCmd = whapps_config:get_binary(<<"fax">>, <<"conversion_command">>, ?CONVERT_PDF_CMD),
            Cmd = io_lib:format(ConvertCmd, [OutputFile, InputFile]),
            lager:debug("attempting to convert pdf: ~s", [Cmd]),
            case os:cmd(Cmd) of
                "success" ->
                    {'ok', OutputFile};
                _Else ->
                    lager:debug("could not covert file: ~s", [_Else]),
                    {'error', <<"can not convert file, try uploading a tiff">>}
            end;
        Else ->
            lager:debug("unsupported file type: ~p", [Else]),
            {'error', list_to_binary(["file type '", Else, "' is unsupported"])}
    end.

-spec get_sizes(ne_binary()) -> {integer(), integer()}.
get_sizes(OutputFile) ->
    CmdCount = whapps_config:get_binary(<<"fax">>, <<"count_pages_command">>, ?COUNT_PAGES_CMD),
    Cmd = io_lib:format(CmdCount, [OutputFile]),
    Result = os:cmd(wh_util:to_list(Cmd)),
    NumberOfPages = wh_util:to_integer(Result),
    FileSize = filelib:file_size(OutputFile),
    {NumberOfPages, FileSize}.

-spec normalize_content_type(text()) -> ne_binary().
normalize_content_type(<<"image/tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/x-tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"image/x-tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"application/tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/x-tif">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"apppliction/x-tiff">>) -> <<"image/tiff">>;
normalize_content_type(<<"application/pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"application/x-pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"text/pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<"text/x-pdf">>) -> <<"application/pdf">>;
normalize_content_type(<<_/binary>> = Else) -> Else;
normalize_content_type(CT) ->
    normalize_content_type(wh_util:to_binary(CT)).

-spec send_fax(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
send_fax(JobId, JObj, Q) ->
    send_fax(JobId, JObj, Q, get_did(JObj)).

-spec send_fax(ne_binary(), wh_json:object(), ne_binary(), binary() | 'undefined') -> 'ok'.
send_fax(_JobId, _JObj, _Q, 'undefined') ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(undefined)">>});
send_fax(_JobId, _JObj, _Q, <<>>) ->
    gen_server:cast(self(), {'error', 'invalid_number', <<"(empty)">>});
send_fax(JobId, JObj, Q, ToDID) ->
    IgnoreEarlyMedia = wh_util:to_binary(whapps_config:get_is_true(?CONFIG_CAT, <<"ignore_early_media">>, 'false')),
    ToNumber = wh_util:to_binary(wh_json:get_value(<<"to_number">>, JObj)),
    ToName = wh_util:to_binary(wh_json:get_value(<<"to_name">>, JObj, ToNumber)),
    CallId = wh_util:rand_hex_binary(8),
    ETimeout = wh_util:to_binary(whapps_config:get_integer(?CONFIG_CAT, <<"endpoint_timeout">>, 10)),
    Request = props:filter_undefined(
                [{<<"Outbound-Caller-ID-Name">>, wh_json:get_value(<<"from_name">>, JObj)}
                 ,{<<"Outbound-Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
                 ,{<<"Outbound-Callee-ID-Number">>, ToNumber}
                 ,{<<"Outbound-Callee-ID-Name">>, ToName }
                 ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
                 ,{<<"To-DID">>, ToDID}
                 ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"fax_identity_number">>, JObj)}
                 ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"fax_identity_name">>, JObj)}
                 ,{<<"Fax-Timezone">>, wh_json:get_value(<<"fax_timezone">>, JObj)}
                 ,{<<"Flags">>, [<<"fax">> | wh_json:get_value(<<"flags">>, JObj, [])]}
                 ,{<<"Resource-Type">>, <<"originate">>}
                 ,{<<"Msg-ID">>, JobId}
                 ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Authorizing-ID">>, JobId}
                                                                 ,{<<"Authorizing-Type">>, <<"outbound_fax">>}
                                                                ])}
                 ,{<<"Custom-SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, JObj)}
                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
                 ,{<<"Application-Name">>, <<"fax">>}
                 ,{<<"Timeout">>,ETimeout}
                 ,{<<"Application-Data">>, get_proxy_url(JobId)}
                 ,{<<"Outbound-Call-ID">>, CallId}
                 ,{<<"Bypass-E164">>, wh_json:is_true(<<"bypass_e164">>, JObj)}
                 | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                ]),
    gen_listener:add_binding(self(), 'call', [{'callid', CallId}
                                              ,{'restrict_to', [<<"CHANNEL_FAX_STATUS">>]}
                                             ]),
    wapi_offnet_resource:publish_req(Request).

-spec get_did(wh_json:object()) -> api_binary().
get_did(JObj) ->
    case wh_json:is_true(<<"bypass_e164">>, JObj, 'false') of
        'true' -> wh_json:get_value(<<"to_number">>, JObj);
        'false' -> wnm_util:to_e164(wh_json:get_value(<<"to_number">>, JObj))
    end.

-spec get_proxy_url(ne_binary()) -> ne_binary().
get_proxy_url(JobId) ->
    Hostname = wh_network_utils:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"port">>),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId, ".tiff"]).

-spec reset(state()) -> state().
reset(State) ->
    put('callid', ?LOG_SYSTEM_ID),
    State#state{job_id='undefined'
                ,job='undefined'
                ,pool='undefined'
                ,page=0
               }.

-spec send_status(state(), ne_binary()) -> any().
send_status(State, Status) ->
    send_status(State, Status, ?FAX_SEND, 'undefined').

-spec send_error_status(state(), text()) -> any().
send_error_status(State, Status) ->
    send_status(State, Status, ?FAX_ERROR, 'undefined').

-spec send_status(state(), text(), api_object()) -> any().
send_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_SEND, FaxInfo).

-spec send_status(state(), text(), ne_binary(), api_object()) -> any().
send_status(#state{job=JObj
                   ,page=Page
                   ,job_id=JobId
                   ,account_id=AccountId
                  }
            ,Status, FaxState, FaxInfo) ->
    FaxboxId = wh_json:get_value(<<"faxbox_id">>, JObj),
    CloudJobId = wh_json:get_value(<<"cloud_job_id">>, JObj),
    CloudPrinterId = wh_json:get_value(<<"cloud_printer_id">>, JObj),
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                 ,{<<"FaxBox-ID">>, FaxboxId}
                 ,{<<"Account-ID">>, AccountId}
                 ,{<<"Cloud-Job-ID">>, CloudJobId}
                 ,{<<"Cloud-Printer-ID">>, CloudPrinterId}
                 ,{<<"Status">>, wh_util:to_binary(Status)}
                 ,{<<"Fax-State">>, FaxState}
                 ,{<<"Fax-Info">>, FaxInfo}
                 ,{<<"Direction">>, ?FAX_OUTGOING}
                 ,{<<"Page">>, Page}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_fax:publish_status(Payload).

-spec send_reply_status(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), api_object()) -> 'ok'.
send_reply_status(Q, MsgId, JobId, Status, AccountId, JObj) ->
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                 ,{<<"Status">>, Status}
                 ,{<<"Msg-ID">>, MsgId}
                 ,{<<"Account-ID">>, AccountId}
                 ,{<<"Fax-Info">>, JObj}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_fax:publish_targeted_status(Q, Payload).
