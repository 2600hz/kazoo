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
         ,handle_call_event/2
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
                ,keep_alive :: 'undefined' | reference()
                ,max_time :: 'undefined' | reference()
               }).
-type state() :: #state{}.


-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_tx_resp'}
                      ,[{<<"resource">>, <<"offnet_resp">>}]
                     }
                     ,{{?MODULE, 'handle_call_event'}
                       ,[{<<"call_detail">>, <<"cdr">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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

handle_call_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'tx_resp', wh_json:get_value(<<"Msg-ID">>, JObj), JObj}).

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
            lager:debug("received successful attempt to tx fax, releasing job"),
            release_successful_job(JObj, Job);
        _Else ->
            lager:debug("received failed attempt to tx fax, releasing job: ~s", [_Else]),
            release_failed_job('tx_resp', JObj, Job)
    end,
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', reset(State)};
handle_cast({'tx_resp', _, _}, State) ->
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
    case attempt_to_acquire_job(JobId) of
        {'ok', JObj} ->
            lager:debug("acquired job, attempting to send fax ~s", [JobId]),
            try execute_job(JObj, Q) of
                'ok' ->
                    KeepAliveRef = erlang:send_after(60000, self(), 'keep_alive'),
                    MaxTime = whapps_config:get_integer(?CONFIG_CAT, <<"job_timeout">>, 900000),
                    MaxTimeRef = erlang:send_after(MaxTime, self(), 'job_timeout'),
                    {'noreply', State#state{job_id=JobId
                                            ,pool=Pid
                                            ,job=JObj
                                            ,keep_alive=KeepAliveRef
                                            ,max_time=MaxTimeRef
                                           }};
                'failure' ->
                    gen_server:cast(Pid, {'job_complete', self()}),
                    {'noreply', reset(State)}
            catch
                Error:_R ->
                    lager:debug("~s while processing job ~s: ~p", [Error, JobId, _R]),
                    release_failed_job('exception', Error, JObj),
                    gen_server:cast(Pid, {'job_complete', self()}),
                    {'noreply', reset(State)}
            end;
        {'error', _Reason} ->
            lager:debug("failed to acquire job ~s: ~p", [JobId, _Reason]),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State#state{queue_name=QueueName}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
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
handle_info('keep_alive', #state{pool=Pid
                                 ,job_id=JobId
                                }=State) ->
    case bump_modified(JobId) of
        {'ok', _} ->
            KeepAliveRef = erlang:send_after(60000, self(), 'keep_alive'),
            {'noreply', State#state{keep_alive=KeepAliveRef}};
        {'error', _R} ->
            lager:debug("prematurely terminating job: ~p", [_R]),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_info('job_timeout', #state{job=JObj}=State) ->
    release_failed_job('job_timeout', 'undefined', JObj),
    {'noreply', reset(State)};
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
-spec attempt_to_acquire_job(ne_binary()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', term()}.
attempt_to_acquire_job(Id) ->
    case couch_mgr:open_doc(?WH_FAXES, Id) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"pending">> ->
                    couch_mgr:save_doc(?WH_FAXES, wh_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                                                      ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
                                                                      ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                                     ],JObj));
                _Else ->
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {'error', 'job_not_available'}
            end
    end.

-spec bump_modified(ne_binary()) -> {'ok', wh_json:object()} | {'error', _}.
bump_modified(JobId) ->
    case couch_mgr:open_doc(?WH_FAXES, JobId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"processing">> ->
                    lager:debug("bumped modified time for fax job ~s", [JobId]),
                    couch_mgr:save_doc(?WH_FAXES, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj));
                _Else ->
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {'error', 'invalid_job_status'}
            end
    end.

-spec release_failed_job('fetch_failed', string(), wh_json:object()) -> 'failure';
                        ('bad_file', ne_binary(), wh_json:object()) -> 'failure';
                        ('job_timeout', 'undefined', wh_json:object()) -> 'failure';
                        ('fetch_error', {atom(), _}, wh_json:object()) -> 'failure';
                        ('tx_resp', wh_json:object(), wh_json:object()) -> 'failure';
                        ('exception', _, wh_json:object()) -> 'failure';
                        ('timeout', _, wh_json:object()) -> 'failure'.
release_failed_job('fetch_failed', Status, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("could not retrieve file, http response ~s", [Status])),
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
release_failed_job('exception', _Error, JObj) ->
    Result = [{<<"success">>, 'false'}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, <<"fax job caused an exception">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, 'false'}
             ],
    release_job(Result, JObj);
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
    Result = [{<<"time_elapsed">>, elapsed_time(JObj)}
              | fax_util:fax_properties(wh_json:get_value(<<"Resource-Response">>, Resp))
             ],
    release_job(Result, JObj).

-spec release_job(wh_proplist(), wh_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj) ->
    Success = props:is_true(<<"success">>, Result, 'false'),
    Updaters = [fun(J) -> wh_json:set_value(<<"tx_result">>, wh_json:from_list(Result), J) end
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
    couch_mgr:ensure_saved(?WH_FAXES, lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)),
    case Success of 'true' -> 'ok'; 'false' -> 'failure' end.

-spec elapsed_time(wh_json:object()) -> non_neg_integer().
elapsed_time(JObj) ->
    wh_util:current_tstamp() - wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()).

-spec execute_job(wh_json:object(), ne_binary()) -> 'ok' | 'failure'.
execute_job(JObj, Q) ->
    JobId = wh_json:get_value(<<"_id">>, JObj),
    case fetch_document(JObj) of
        {'ok', "200", RespHeaders, RespContent} ->
            case prepare_contents(JobId, RespHeaders, RespContent) of
                {'error', Cause} -> release_failed_job('bad_file', Cause, JObj);
                {'ok', _} -> send_fax(JobId, JObj, Q)
            end;
        {'ok', Status, _, _} ->
            lager:debug("failed to fetch file for job: http response ~p", [Status]),
            release_failed_job('fetch_failed', Status, JObj);
        {'error', Reason} ->
            lager:debug("failed to fetch file for job: ~p", [Reason]),
            release_failed_job('fetch_error', Reason, JObj)
    end.

-spec fetch_document(wh_json:object()) ->
                            {'ok', string(), wh_proplist(), ne_binary()} |
                            {'error', term()}.
fetch_document(JObj) ->
	case wh_json:get_value(<<"_attachments">>, JObj, 'undefined') of
		'undefined' -> fetch_document_from_url(JObj);
		Attachments -> fetch_document_from_attachment(JObj)
	end.


-spec fetch_document_from_attachment(wh_json:object()) ->
                            {'ok', string(), wh_proplist(), ne_binary()} |
                            {'error', term()}.
fetch_document_from_attachment(JObj) ->
	lager:info("execute_job ~p",[JObj]),
    JobId = wh_json:get_value(<<"_id">>, JObj),
	Attachments = wh_json:get_value(<<"_attachments">>, JObj),
	lager:info("execute_job_attachments ~p",[Attachments]),
	Keys = wh_json:get_keys(Attachments),
	lager:info("execute_job_keys ~p",[Keys]),
	AttachmentName = lists:nth(1,Keys),	
	lager:info("execute_job_attachment_key ~p",[AttachmentName]),
	AttachmentDoc = wh_json:get_value(AttachmentName, Attachments),	
	lager:info("execute_job_attachment ~p",[AttachmentDoc]),
	CT = wh_json:get_value(<<"content_type">>, AttachmentDoc),
	lager:info("execute_job_attachment_ct ~p",[CT]),
	Props = [{"Content-Type",CT}],	
	{'ok', Contents} = couch_mgr:fetch_attachment(?WH_FAXES,JobId,AttachmentName),
	{'ok',"200",Props,Contents}.
	
	
	
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
            DefaultCmd = <<"/usr/bin/gs -q -r204x98 -g1728x1078 -dNOPAUSE -dBATCH -dSAFER -sDEVICE=tiffg3 -sOutputFile=~s -- ~s &> /dev/null && echo -n \"success\"">>,
            ConvertCmd = whapps_config:get_binary(<<"fax">>, <<"conversion_command">>, DefaultCmd),
            Cmd = io_lib:format(ConvertCmd, [OutputFile, InputFile]),
            lager:debug("attempting to convert pdf: ~s", [Cmd]),
            Result = case os:cmd(Cmd) of
                "success" ->
                    {'ok', OutputFile};
                _Else ->
                    lager:debug("could not covert file: ~s", [_Else]),
                    {'error', <<"can not convert file, try uploading a tiff">>}
            end,
			file:delete(InputFile),
			Result;
        Else ->
            lager:debug("unsupported file type: ~p", [Else]),
            {'error', list_to_binary(["file type '", Else, "' is unsupported"])}
    end.

-spec normalize_content_type(string() | ne_binary()) -> ne_binary().
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
normalize_content_type(<<Else/binary>>) ->
    Else;
normalize_content_type(CT) ->
    normalize_content_type(wh_util:to_binary(CT)).

-spec send_fax(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
send_fax(JobId, JObj, Q) ->
    IgnoreEarlyMedia = wh_util:to_binary(whapps_config:get_is_true(?CONFIG_CAT, <<"ignore_early_media">>, 'false')),
    CallId = wh_util:rand_hex_binary(8),
    Request = props:filter_undefined([
                {<<"Outbound-Caller-ID-Name">>, wh_json:get_value(<<"from_name">>, JObj)}
               ,{<<"Outbound-Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
               ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
               ,{<<"To-DID">>, wnm_util:to_e164(wh_json:get_value(<<"to_number">>, JObj))}
               ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"fax_identity_number">>, JObj)}
               ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"fax_identity_name">>, JObj)}
               ,{<<"Flags">>, wh_json:get_value(<<"flags">>, JObj)}
               ,{<<"Resource-Type">>, <<"originate">>}
               ,{<<"Msg-ID">>, JobId}
               ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Authorizing-ID">>, JobId}
                                                               ,{<<"Authorizing-Type">>, <<"outbound_fax">>}
                                                              ])}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, JObj)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
               ,{<<"Application-Name">>, <<"fax">>}
               ,{<<"Application-Data">>, get_proxy_url(JobId)}
               ,{<<"Outbound-Call-ID">>, CallId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ]),
    gen_listener:add_binding(self(), 'call', [{'restrict_to', ['cdr']}, {'callid', CallId}]),
    wapi_offnet_resource:publish_req(Request).

-spec get_proxy_url(ne_binary()) -> ne_binary().
get_proxy_url(JobId) ->
    Hostname = wh_network_utils:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"port">>),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId, ".tiff"]).

-spec reset(state()) -> state().
reset(#state{keep_alive=KeepAliveRef}=State) when is_reference(KeepAliveRef) ->
    erlang:cancel_timer(KeepAliveRef),
    reset(State#state{keep_alive='undefined'});
reset(#state{max_time=MaxTimeRef}=State) when is_reference(MaxTimeRef) ->
    erlang:cancel_timer(MaxTimeRef),
    reset(State#state{max_time='undefined'});
reset(State) ->
    put('callid', ?LOG_SYSTEM_ID),
    State#state{job_id='undefined'
                ,job='undefined'
                ,pool='undefined'
               }.
