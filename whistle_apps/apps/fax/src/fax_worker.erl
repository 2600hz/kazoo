%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(fax_worker).

-behaviour(gen_listener).

-export([start_link/1]).
-export([handle_tx_resp/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("fax.hrl").

-record(state, {queue_name :: 'undefined' | ne_binary()
                ,pool :: 'undefined' | pid()
                ,job_id :: 'undefined' | ne_binary()
                ,job :: 'undefined' | wh_json:json_object()
                ,keep_alive :: 'undefined' | reference() 
                ,max_time :: 'undefined' | reference()               
               }).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{?MODULE, handle_tx_resp}, [{<<"resource">>, <<"offnet_resp">>}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(ROUTE_OPTIONS, []).

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
    gen_listener:start_link(?MODULE, [{bindings, ?BINDINGS}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{route_options, ?ROUTE_OPTIONS}
                                     ], []).

handle_tx_resp(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_server:cast(Srv, {tx_resp, wh_json:get_value(<<"Msg-ID">>, JObj), JObj}).

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
    Self = self(),
    spawn(fun() ->
                  QueueName = gen_listener:queue_name(Self),
                  gen_listener:cast(Self, {queue_name, QueueName})
          end),
    {ok, #state{}}.

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
    {reply, {error, not_implemented}, State}.

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
handle_cast({tx_resp, JobId, JObj}, #state{job_id=JobId, job=Job, pool=Pid}=State) ->
    case wh_json:get_value(<<"Response-Message">>, JObj) of
        <<"SUCCESS">> -> 
            lager:debug("received successful attempt to tx fax, releasing job", []),
            release_successful_job(JObj, Job);
        _Else -> 
            lager:debug("received failed attempt to tx fax, releasing job: ~s", [_Else]),
            release_failed_job(tx_resp, JObj, Job)
    end,
    gen_server:cast(Pid, {job_complete, self()}),
    {noreply, reset(State)};
handle_cast({tx_resp, _, _}, State) ->
    {noreply, State};
handle_cast({_, Pid, _}, #state{queue_name=undefined}=State) when is_pid(Pid) ->
    lager:debug("worker received request with unknown queue name, rejecting", []),
    gen_server:cast(Pid, {job_complete, self()}),
    {noreply, State};
handle_cast({_, Pid, _}, #state{job_id=JobId}=State) when is_binary(JobId), is_pid(Pid) ->
    lager:debug("worker received request while still processing a job, rejecting", []),
    gen_server:cast(Pid, {job_complete, self()}),
    {noreply, State};
handle_cast({attempt_transmission, Pid, Job}, #state{queue_name=Q}=State) ->
    JobId = wh_json:get_value(<<"id">>, Job),
    put(callid, JobId),
    case attempt_to_acquire_job(JobId) of
        {ok, JObj} -> 
            lager:debug("acquired job, attempting to send fax ~s", [JobId]),
            try execute_job(JObj, Q) of
                ok -> 
                    KeepAliveRef = erlang:send_after(60000, self(), keep_alive),
                    MaxTime = whapps_config:get_integer(?CONFIG_CAT, <<"job_timeout">>, 900000),
                    MaxTimeRef = erlang:send_after(MaxTime, self(), job_timeout),
                    {noreply, State#state{job_id=JobId, pool=Pid, job=JObj, keep_alive=KeepAliveRef, max_time=MaxTimeRef}};
                failure -> 
                    gen_server:cast(Pid, {job_complete, self()}),
                    {noreply, reset(State)}
            catch
                Error:_R ->
                    lager:debug("~s while processing job ~s: ~p", [Error, JobId, _R]),
                    release_failed_job(exception, Error, JObj),
                    gen_server:cast(Pid, {job_complete, self()}),
                    {noreply, reset(State)}
            end;
        {error, _Reason} -> 
            lager:debug("failed to acquire job ~s: ~p", [JobId, _Reason]),
            gen_server:cast(Pid, {job_complete, self()}),
            {noreply, reset(State)}
    end;
handle_cast({queue_name, QueueName}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {noreply, State#state{queue_name=QueueName}};    
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(keep_alive, #state{pool=Pid, job_id=JobId}=State) ->
    case bump_modified(JobId) of
        {ok, _} ->
            KeepAliveRef = erlang:send_after(60000, self(), keep_alive),
            {noreply, State#state{keep_alive=KeepAliveRef}};
        {error, _R} ->
            lager:debug("prematurely terminating job: ~p", [_R]),
            gen_server:cast(Pid, {job_complete, self()}),
            {noreply, reset(State)}
    end;
handle_info(job_timeout, #state{job=JObj}=State) ->
    release_failed_job(job_timeout, undefined, JObj),
    {noreply, reset(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {reply, []}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec attempt_to_acquire_job/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                                   {'error', term()}.
attempt_to_acquire_job(Id) ->
    case couch_mgr:open_doc(?WH_FAXES, Id) of
        {error, _}=E -> E;
        {ok, JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"pending">> ->
                    couch_mgr:save_doc(?WH_FAXES, wh_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                                                      ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
                                                                      ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                                     ],JObj));
                _Else -> 
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {error, job_not_available}
            end
    end.
        
-spec bump_modified/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} | {'error', _}.
bump_modified(JobId) ->
    case couch_mgr:open_doc(?WH_FAXES, JobId) of
        {error, _}=E -> E;
        {ok, JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"processing">> ->
                    lager:debug("bumped modified time for fax job ~s", [JobId]),
                    couch_mgr:save_doc(?WH_FAXES, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj));
                _Else ->
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {error, invalid_job_status}
            end
    end.    
     
-spec release_failed_job/3 :: ('fetch_failed', string(), wh_json:json_object()) -> 'failure';
                              ('bad_file', ne_binary(), wh_json:json_object()) -> 'failure';
                              ('fetch_error', {atom(), _}, wh_json:json_object()) -> 'failure';
                              ('tx_resp', wh_json:json_object(), wh_json:json_object()) -> 'failure';
                              ('exception', _, wh_json:json_object()) -> 'failure';
                              ('timeout', _, wh_json:json_object()) -> 'failure'.
release_failed_job(fetch_failed, Status, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("could not retrieve file, http response ~s", [Status])),
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, Msg}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(bad_file, Msg, JObj) ->
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, Msg}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(fetch_error, {conn_failed, _}, JObj) ->
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, <<"could not connect to document URL">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(fetch_error, {Cause, _}, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("could not connect to document URL: ~s", [Cause])),
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, Msg}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(tx_resp, Resp, JObj) ->
    Msg = wh_json:get_value(<<"Error-Message">>, Resp),
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, Msg}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(exception, _Error, JObj) ->
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, <<"fax job caused an exception">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj);
release_failed_job(job_timeout, _Error, JObj) ->
    Result = [{<<"success">>, false}
              ,{<<"result_code">>, 0}
              ,{<<"result_text">>, <<"fax job timed out">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj).

-spec release_successful_job/2 :: (wh_json:json_object(), wh_json:json_object()) -> 'ok'.
release_successful_job(Resp, JObj) ->
    Result = [{<<"success">>, wh_json:is_true([<<"Resource-Response">>, <<"Fax-Success">>], Resp, false)}
              ,{<<"result_code">>, wh_json:get_value([<<"Resource-Response">>, <<"Fax-Result-Code">>], Resp, 0)}
              ,{<<"result_message">>, wh_json:get_value([<<"Resource-Response">>, <<"Fax-Result-Text">>], Resp, <<>>)}
              ,{<<"pages_sent">>, wh_json:get_integer_value([<<"Resource-Response">>, <<"Fax-Transferred-Pages">>], Resp, 0)}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, wh_json:get_integer_value([<<"Resource-Response">>, <<"Fax-Bad-Rows">>], Resp, 0)}
              ,{<<"fax_speed">>, wh_json:get_integer_value([<<"Resource-Response">>, <<"Fax-Transfer-Rate">>], Resp, 0)}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, wh_json:is_true([<<"Resource-Response">>, <<"Fax-ECM-Used">>], Resp, false)}              
             ],
    release_job(Result, JObj).

-spec release_job/2 :: (proplist(), wh_json:json_object()) -> 'ok' | 'failure'.
release_job(Result, JObj) ->
    Success = props:get_value(<<"success">>, Result, false),
    Updaters = [fun(J) -> wh_json:set_value(<<"tx_result">>, wh_json:from_list(Result), J) end
                ,fun(J) ->
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         Retries = wh_json:get_integer_value(<<"retries">>, J, 1),
                         case Retries - Attempts >= 1 of 
                             _ when Success -> 
                                 lager:debug("releasing job with status: completed", []),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"completed">>, J);
                             true ->
                                 lager:debug("releasing job with status: pending", []),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"pending">>, J);
                             false -> 
                                 lager:debug("releasing job with status: failed", []),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"failed">>, J)
                         end
                 end
                ,fun(J) -> 
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         wh_json:set_value(<<"attempts">>, Attempts + 1, J)
                 end
               ],                             
    couch_mgr:ensure_saved(?WH_FAXES, lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)),
    case Success of true -> ok; false -> failure end.

-spec elapsed_time/1 :: (wh_json:json_object()) -> non_neg_integer().
elapsed_time(JObj) ->
    wh_util:current_tstamp() - wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()).

-spec execute_job/2 :: (wh_json:json_object(), ne_binary()) -> 'ok' | 'failure'.
execute_job(JObj, Q) ->
    JobId = wh_json:get_value(<<"_id">>, JObj),
    case fetch_document(JObj) of
        {ok, "200", RespHeaders, RespContent} ->
            case prepare_contents(JobId, RespHeaders, RespContent) of
                {error, Cause} -> release_failed_job(bad_file, Cause, JObj);
                {ok, _} -> send_fax(JobId, JObj, Q)
            end;
        {ok, Status, _, _} ->
            lager:debug("failed to fetch file for job: http response ~p", [Status]),
            release_failed_job(fetch_failed, Status, JObj);
        {error, Reason} ->
            lager:debug("failed to fetch file for job: ~p", [Reason]),
            release_failed_job(fetch_error, Reason, JObj)
    end.

-spec fetch_document/1 :: (wh_json:json_object()) -> {'ok', string(), proplist(), ne_binary()} |
                                                     {'error', term()}.
fetch_document(JObj) ->
    FetchRequest = wh_json:get_value(<<"document">>, JObj),    
    Url = wh_json:get_string_value(<<"url">>, FetchRequest),
    Method = wh_util:to_atom(wh_json:get_value(<<"method">>, FetchRequest, <<"get">>), true),
    Headers = [{K, V}
               || {K, V} <- [{"Host", wh_json:get_string_value(<<"host">>, FetchRequest)}
                             ,{"Referer", wh_json:get_string_value(<<"referer">>, FetchRequest)}
                             ,{"User-Agent", wh_json:get_string_value(<<"user_agent">>, FetchRequest, wh_util:to_list(node()))}
                             ,{"Content-Type", wh_json:get_string_value(<<"content_type">>, FetchRequest, <<"text/plain">>)}]
                      ,V =/= undefined],
    HTTPOptions = [{response_format, binary}],
    Body = wh_json:get_string_value(<<"content">>, FetchRequest, ""),
    lager:debug("making ~s request to '~s'", [Method, Url]),
    ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions).

-spec prepare_contents/3 :: (ne_binary(), proplist(), ne_binary()) -> {'ok', ne_binary()} | {'error', ne_binary()}.
prepare_contents(JobId, RespHeaders, RespContent) ->
    lager:debug("preparing fax contents", []),   
    TmpDir = whapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>),
    case normalize_content_type(props:get_value("Content-Type", RespHeaders, <<"application/octet-stream">>)) of
        <<"image/tiff">> -> 
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            R = file:write_file(OutputFile, RespContent),
            lager:debug("result of tmp file write: ~s", [R]),
            {ok, OutputFile};
        <<"application/pdf">> -> 
            InputFile = list_to_binary([TmpDir, JobId, ".pdf"]),
            OutputFile = list_to_binary([TmpDir, JobId, ".tiff"]),
            R = file:write_file(InputFile, RespContent),
            lager:debug("result of tmp file write: ~s", [R]),
            Cmd = io_lib:format("/usr/bin/gs -q -r204x98 -g1728x1078 -dNOPAUSE -dBATCH -dSAFER -sDEVICE=tiffg3 -sOutputFile=~s -- ~s &> /dev/null && echo -n \"success\"", [OutputFile, InputFile]),
            lager:debug("attempting to convert pdf: ~s", [Cmd]),
            case os:cmd(Cmd) of
                "success" -> 
                    {ok, OutputFile};
                _Else -> 
                    lager:debug("could not covert file: ~s", [_Else]),
                    {error, <<"can not convert file, try uploading a tiff">>}
            end;
        Else -> 
            lager:debug("unsupported file type: ~p", [Else]),
            {error, list_to_binary(["file type '", Else, "' is unsupported"])}
    end.

-spec normalize_content_type/1 :: (string() | ne_binary()) -> ne_binary().
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

-spec send_fax/3 :: (ne_binary(), wh_json:json_object(), ne_binary()) -> 'ok'.
send_fax(JobId, JObj, Q) ->
    Request = [{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"from_name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
               ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
               ,{<<"To-DID">>, wnm_util:to_e164(wh_json:get_value(<<"to_number">>, JObj))}
               ,{<<"Resource-Type">>, <<"originate">>}
               ,{<<"Msg-ID">>, JobId}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Authorizing-ID">>, JobId}
                                                               ,{<<"Authorizing-Type">>, <<"outbound_fax">>}
                                                              ])}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
               ,{<<"Application-Name">>, <<"fax">>}
               ,{<<"Application-Data">>, get_proxy_url(JobId)}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_offnet_resource:publish_req(Request).

-spec get_proxy_url/1 :: (ne_binary()) -> ne_binary().
get_proxy_url(JobId) ->
    Hostname = wh_util:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"port">>),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId, ".tiff"]).
    
-spec reset/1 :: (#state{}) -> #state{}.
reset(#state{keep_alive=KeepAliveRef}=State) when is_reference(KeepAliveRef) ->
    erlang:cancel_timer(KeepAliveRef),
    reset(State#state{keep_alive=undefined});
reset(#state{max_time=MaxTimeRef}=State) when is_reference(MaxTimeRef) ->
    erlang:cancel_timer(MaxTimeRef),
    reset(State#state{max_time=undefined});
reset(State) ->
    put(callid, <<"00000000000">>),
    State#state{job_id=undefined, job=undefined, pool=undefined}.
