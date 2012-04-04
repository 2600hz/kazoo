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
-export([handle_resp/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("fax.hrl").

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{?MODULE, handle_resp}, [{<<"*">>, <<"*">>}]}]).
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

handle_resp(JObj, _Props) ->
    io:format("~p~n", [JObj]).

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
    {ok, #state{}, 0}.

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
handle_cast({attempt_transmission, Pid, Job}, State) ->
    JobId = wh_json:get_value(<<"id">>, Job),
    case attempt_to_acquire_job(JobId) of
        {ok, JObj} -> 
            lager:debug("attempting to send fax ~s", [JobId]),
            try execute_job(JObj) of
                false -> ok;
                true -> ok
            catch
                Error:_R ->
                    lager:debug("~s while processing job ~s: ~p", [Error, JobId, _R]),
                    release_failed_job(exception, Error, JObj)
            end,
            gen_server:cast(Pid, {job_complete, self()}),
            {noreply, State};
        {error, _Reason} -> 
            lager:debug("failed to acquire job ~s: ~p", [JobId, _Reason]),
            gen_server:cast(Pid, {job_complete, self()}),
            {noreply, State}
    end;
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
                    couch_mgr:save_doc(?WH_FAXES, wh_json:set_value(<<"pvt_job_status">>, <<"processing">>, JObj));
                _Else -> 
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {error, job_not_available}
            end
    end.
             
-spec release_failed_job/3 :: ('fetch_failed', string(), wh_json:json_object()) -> 'failure';
                              ('fetch_error', {atom(), term()}, wh_json:json_object()) -> 'failure';
                              ('exception', term(), wh_json:json_object()) -> 'failure'.
release_failed_job(fetch_failed, Status, JObj) ->
    Msg = wh_util:to_binary(io_lib:format("could not retrieve file, http response ~s", [Status])),
    Result = [{<<"success">>, false}
              ,{<<"error_message">>, Msg}
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
              ,{<<"error_message">>, <<"could not connect to document URL">>}
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
              ,{<<"error_message">>, Msg}
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
              ,{<<"error_message">>, <<"fax job caused an exception">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj).

-spec release_successful_job/2 :: (wh_json:json_object(), wh_json:json_object()) -> boolean().
release_successful_job(_Result, JObj) ->
    Result = [{<<"success">>, true}
              ,{<<"error_message">>, <<"">>}
              ,{<<"pages_sent">>, 0}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"fax_bad_rows">>, 0}
              ,{<<"fax_speed">>, 0}
              ,{<<"fax_receiver_id">>, <<>>}
              ,{<<"fax_error_correction">>, false}              
             ],
    release_job(Result, JObj).

-spec release_job/2 :: (proplist(), wh_json:json_object()) -> boolean().
release_job(Result, JObj) ->
    Success = props:get_value(<<"success">>, Result, false),
    Updaters = [fun(J) -> wh_json:set_value(<<"tx_result">>, Result, J) end
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
    Success.

-spec elapsed_time/1 :: (wh_json:json_object()) -> non_neg_integer().
elapsed_time(JObj) ->
    wh_util:current_tstamp() - wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()).

-spec execute_job/1 :: (wh_json:json_object()) -> boolean().
execute_job(JObj) ->
    JobId = wh_json:get_value(<<"_id">>, JObj),
    case fetch_document(JObj) of
        {ok, "200", RespHeaders, RespContent} ->
            prepare_contents(JobId, RespHeaders, RespContent),
            send_fax(JobId, JObj),
            release_successful_job([], JObj);            
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

-spec prepare_contents/3 :: (ne_binary(), proplist(), ne_binary()) -> 'ok'.
prepare_contents(JobId, RespHeaders, RespContent) ->
    lager:debug("preparing fax contents", []),
    %%exec("/usr/bin/gs -q -r204x98 -g1728x1078 -dNOPAUSE -dBATCH -dSAFER -sDEVICE=tiffg3 -sOutputFile=$dest_file -- $input_file", $output, $return_var)
    {ok, Cache} = fax_sup:cache_proc(),
    ContentType = wh_util:to_list(props:get_value("Content-Type", RespHeaders, <<"application/octet-stream">>)),
    ContentSize = wh_util:to_list(props:get_value("Content-Length", RespHeaders, size(RespContent))),    
    wh_cache:store_local(Cache, {fax_file, JobId}, {ContentType, ContentSize, RespContent}).

-spec send_fax/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
send_fax(JobId, JObj) ->
    Request = [{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"from_name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
               ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
               ,{<<"To-DID">>, wnm_util:to_e164(wh_json:get_value(<<"to_number">>, JObj))}
               ,{<<"Resource-Type">>, <<"originate">>}
               ,{<<"Application-Name">>, <<"fax">>}
               ,{<<"Application-Data">>, get_proxy_url(JobId)}
               | wh_api:default_headers(<<"test">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_offnet_resource:publish_req(Request).

-spec get_proxy_url/1 :: (ne_binary()) -> ne_binary().
get_proxy_url(JobId) ->
    Hostname = wh_util:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"port">>),
    list_to_binary(["http://", Hostname, ":", Port, "/fax/", JobId]).
    
