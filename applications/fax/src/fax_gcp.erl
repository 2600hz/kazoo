%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_gcp).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([handle_job_notify/2
        ,handle_push/2
        ,maybe_process_job/2
        ]).

-include("fax_gcp.hrl").

-define(POLLING_INTERVAL, 60000).



-define(RESPONDERS, [{{?MODULE, 'handle_job_notify'}
                     , [{<<"notification">>, <<"outbound_fax">>}]}
                    ,{{?MODULE, 'handle_job_notify'}
                     , [{<<"notification">>, <<"outbound_fax_error">>}]}
                    ,{{?MODULE, 'handle_push'}
                      ,[{<<"xmpp_event">>, <<"push">>}]
                     }
                    ]).

-define(NOTIFY_RESTRICT_TO, [outbound_fax
                            ,outbound_fax_error
                            ]).

-define(BINDINGS, [{notifications, [{restrict_to, ?NOTIFY_RESTRICT_TO}]}
                  ,{xmpp,[{restrict_to,['push']}]}
                  ,{self, []}
                  ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).


-record(state, {}).


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
start_link() ->    
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, ?BINDINGS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], []).


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

handle_event(_JObj, _State) ->
    {reply, []}.

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
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("terminating fax gcp listener : ~p", [_Reason]).

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


-spec handle_job_notify(wh_json:object(), wh_proplist()) -> any().
handle_job_notify(JObj, _Props) ->
    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"outbound_fax_error">> ->
            'true' = wapi_notifications:fax_outbound_error_v(JObj);
        <<"outbound_fax">> ->
            'true' = wapi_notifications:fax_outbound_v(JObj);
        EventName ->
            lager:debug("wrong message type ~s : crashing this.",[EventName])
    end,
        
    JobId = wh_json:get_value(<<"Fax-JobId">>, JObj),
    {ok, FaxJObj} = couch_mgr:open_doc(?WH_FAXES, JobId),
    lager:debug("Checking if JobId ~s is a cloud printer job",[JobId]),
    case wh_json:get_value(<<"cloud_job_id">>, FaxJObj) of
        'undefined' ->
            lager:debug("JobId ~s is not a cloud printer job",[JobId]);
        CloudJobId ->
            lager:debug("JobId ~s is a cloud printer job with Id ~s",[JobId,CloudJobId]),
            PrinterId = wh_json:get_value(<<"cloud_printer_id">>, FaxJObj),
            process_job_outcome(PrinterId, CloudJobId, wh_json:get_value(<<"Event-Name">>, JObj))
    end.

process_job_outcome(PrinterId, JobId, <<"outbound_fax_error">>) ->
    process_job_outcome(PrinterId, JobId, <<"ABORTED">>);    
process_job_outcome(PrinterId, JobId, <<"outbound_fax">>) ->
    process_job_outcome(PrinterId, JobId, <<"DONE">>);    
process_job_outcome(PrinterId, JobId, Status) ->
    update_job_status(PrinterId, JobId, Status).

-spec handle_push(wh_json:object(), wh_proplist()) -> any().
handle_push(JObj, _Props) ->
    'true' = wapi_xmpp:event_v(JObj),
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    AppEvent = wh_json:get_value(<<"Application-Event">>, JObj),
    AppData = wh_json:get_value(<<"Application-Data">>, JObj),
    JID = wh_json:get_value(<<"JID">>, JObj),
    handle_push_event(JID, AppName, AppEvent, AppData).
   
    
handle_push_event(_JID, <<"GCP">>, <<"Queued-Job">>, PrinterId) ->     
    URL = <<?POOL_URL,PrinterId/binary>>,
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            Headers = [?GPC_PROXY_HEADER , {"Authorization",Authorization}],
            case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
                {'ok', "200", RespHeaders, RespBody} ->
                    JObj = wh_json:decode(RespBody),
                    JObjs = wh_json:get_value(<<"jobs">>, JObj,[]),
                    spawn(?MODULE, maybe_process_job, [JObjs,Authorization]);
                {'ok', "403", RespHeaders, RespBody} ->
                    lager:debug("something wrong with oauth credentials ~p",[RespHeaders]);                    
                Other ->
                    lager:debug("unexpected response from gcp ~p",[Other])                    
            end;
        {'error', E} ->
            lager:debug("no credentials for gcp printer ~s/~p",[PrinterId, E])
    end;
handle_push_event(JID, AppName, AppEvent, AppData) ->
    lager:debug("unhandled xmpp push event ~s/~s/~s/~p",[JID, AppName, AppEvent, AppData]).
    
maybe_process_job([], Authorization) ->
    'ok';
maybe_process_job([JObj | JObjs], Authorization) ->   
    JobId = wh_json:get_value(<<"id">>, JObj),
    TicketObj = fetch_ticket(JobId, Authorization),
    TicketItem = wh_json:get_value([<<"print">>,<<"vendor_ticket_item">>], TicketObj, wh_json:new()),
    NumberObj = lists:foldl(fun(A,B) -> maybe_fax_number(A,B) end, wh_json:new(),TicketItem),
    PrinterId = wh_json:get_value(<<"printerid">>, JObj),
    FileURL = wh_json:get_value(<<"fileUrl">>, JObj),
    case wh_json:get_value(<<"Fax-Number">>, NumberObj) of
        'undefined' ->
            lager:debug("no fax number in job ticket"),
            update_job_status(PrinterId, JobId, <<"ABORT">>);
        FaxNumber ->
            maybe_save_fax_document(JObj, JobId, PrinterId, FaxNumber, FileURL )
    end,
    maybe_process_job(JObjs,Authorization).

maybe_fax_number(A,B) ->
    case wh_json:get_value(<<"id">>,A) of
        <<"fax_number">> ->
            Number = wh_json:get_value(<<"value">>,A),
            wh_json:set_value(<<"Fax-Number">>, Number, B);
        Other -> B
    end.
    

fetch_ticket(JobId, Authorization) ->
    URL = <<?TICKET_URL,JobId/binary>>,
    Headers = [?GPC_PROXY_HEADER , {"Authorization",Authorization}],
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
        {'ok', "200", RespHeaders, RespBody} ->
            wh_json:decode(RespBody);
        Response -> 
            lager:debug("unexpected result fetching ticket : ~p",[Response]),
            wh_json:new()    
    end.

-spec update_job_status(ne_binary(), ne_binary(), ne_binary() | wh_json:object()) -> any().
update_job_status(PrinterId, JobId, <<"IN_PROGRESS">>=Status) ->
    StateObj = wh_json:set_value(<<"state">>, wh_json:set_value(<<"type">>, Status, wh_json:new()), wh_json:new()),
    update_job_status(PrinterId, JobId, StateObj);    
update_job_status(PrinterId, JobId, <<"DONE">>=Status) ->
    StateObj = wh_json:set_value(<<"state">>, wh_json:set_value(<<"type">>, Status, wh_json:new()), wh_json:new()),
    update_job_status(PrinterId, JobId, StateObj);    
update_job_status(PrinterId, JobId, <<"ABORTED">>=Status) ->
    StateObj = fax_cloud_printer_util:recursive_from_list(
                 [{<<"state">>, 
                   [{<<"type">>, Status}
                   ,{<<"device_action_cause">>,
                     [{<<"error_code">>,<<"OTHER">>}]
                    }]}
                 ]),
    update_job_status(PrinterId, JobId, StateObj);    
update_job_status(PrinterId, JobId, Status) ->
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            send_update_job_status(JobId, Status, Authorization);
        {'error', E} ->
            lager:debug("error getting printer (~s) oauth credentials when updating job (~s) status : ~p",[PrinterId, JobId, E])
    end.
    
send_update_job_status(JobId, Status, Authorization) ->
    Headers = [?GPC_PROXY_HEADER,
               {"Authorization",Authorization},
               {"Content-Type","application/x-www-form-urlencoded"}],
    
    Fields = [
              {"jobid", wh_util:to_list(JobId)},
              {"semantic_state_diff", wh_util:to_list(wh_json:encode(Status))}
              ],

    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),

    case ibrowse:send_req(wh_util:to_list(?JOBCTL_URL), Headers, 'post', Body) of
        {'ok', "200", RespHeaders, RespBody} ->
            JObj = wh_json:decode(RespBody),
            lager:debug("TICKET Result ~p", [JObj]),
            JObj;
        Response -> lager:debug("POOL RESPONSE ~p",[Response])
    end.

download_file(URL, Authorization) ->
    Headers = [?GPC_PROXY_HEADER , {"Authorization",Authorization}],
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
        {'ok', "200", RespHeaders, RespBody} ->
            CT = wh_util:to_binary(props:get_value("Content-Type", RespHeaders)),
            Ext = fax_util:content_type_to_extension(CT),
            FileName = <<"/tmp/fax_printer_",(wh_util:to_binary(wh_util:current_tstamp()))/binary,".",Ext/binary>>,
            file:write_file(FileName,RespBody),
            {'ok', CT, RespBody};
        Response -> 
            lager:debug("error downloading file ~s : ~p",[URL, Response]),
            {'error', Response}
    end.


maybe_save_fax_document(Job, JobId, PrinterId, FaxNumber, FileURL ) ->
    case save_fax_document(Job, JobId, PrinterId, FaxNumber) of
        {'ok', JObj} ->
            maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL );
        {'error', 'conflict'} ->
            lager:debug("got conflict saving fax job ~s", [JobId]);
        {'error', _E} ->
            lager:debug("got error saving fax job ~s : ~p", [JobId, _E])
    end.    

maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL ) ->
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            case download_file(FileURL,Authorization) of
                {'ok', CT, FileContents} ->
                    fax_util:save_fax_attachment(JObj, FileContents, CT),
                    update_job_status(PrinterId, JobId, <<"IN_PROGRESS">>);
                {'error', Error} ->
                    lager:debug("error downloading file for JobId ~s : ~p",[JobId, Error])
            end;
        {'error', E} ->
            lager:debug("error getting printer (~s) oauth credentials for JobId (~s) : ~p",[PrinterId, JobId, E])            
    end.
    
save_fax_document(Job, JobId, PrinterId, FaxNumber ) ->     
    {'ok', FaxBoxDoc} = couch_mgr:open_doc(?WH_FAXES, PrinterId),
    AccountId = wh_json:get_value(<<"pvt_account_id">>,FaxBoxDoc),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    OwnerId = wh_json:get_value(<<"ownerId">>, Job),
    NotifyDef = wh_json:get_value(<<"email_to">>,FaxBoxDoc,[]),
    Notify = [ OwnerId | NotifyDef ],
    Props = props:filter_undefined([
             {<<"from_name">>,wh_json:get_value(<<"caller_name">>,FaxBoxDoc)}
            ,{<<"fax_identity_name">>, wh_json:get_value(<<"caller_name">>, FaxBoxDoc)}     
            ,{<<"from_number">>,wh_json:get_value(<<"caller_id">>,FaxBoxDoc)}
            ,{<<"fax_identity_number">>, wh_json:get_value(<<"caller_id">>, FaxBoxDoc)}
            ,{<<"fax_timezone">>, wh_json:get_value(<<"timezone">>, FaxBoxDoc)}
            ,{<<"to_name">>,FaxNumber}
            ,{<<"to_number">>,FaxNumber}
            ,{<<"retries">>,wh_json:get_value(<<"retries">>,FaxBoxDoc,3)}
            ,{<<"notifications">>,notifications(Notify) }
            ,{<<"faxbox_id">>, PrinterId}
            ,{<<"folder">>, <<"outbox">>}
            ,{<<"cloud_printer_id">>, PrinterId}
            ,{<<"cloud_job_id">>, JobId}
            ,{<<"cloud_job">>, Job}
             ]),
    { _ , JObjTemp} = wh_json_validator:is_valid(wh_json:from_list(Props), <<"faxes">>),
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                             ,{<<"_id">>, JobId}
                             ,{<<"pvt_job_status">>, <<"queued">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                             ,{<<"attempts">>, 0}
                             ,{<<"pvt_account_id">>, AccountId}
                             ,{<<"pvt_account_db">>, AccountDb}], JObjTemp),
    couch_mgr:save_doc(?WH_FAXES, Doc).


notifications('undefined') -> 'undefined';
notifications(Notifications) ->
    wh_json:from_list(
      [{<<"email">>, wh_json:from_list( 
          [ {<<"send_to">>, lists:usort(Notifications)} ] 
                                      ) 
       }]).





get_printer_oauth_credentials(PrinterId) ->
    case wh_cache:peek_local(?FAX_CACHE, {'gcp', PrinterId }) of
        {'ok', Auth}=OK -> OK;
        {'error', _} ->
            case couch_mgr:open_cache_doc(?WH_FAXES, PrinterId) of
                {'ok', JObj} ->
                    Cloud = wh_json:get_value(<<"pvt_cloud">>, JObj),
                    {'ok',App} = kazoo_oauth_util:get_oauth_app(wh_json:get_value(<<"oauth_app">>, Cloud)),
                    RefreshToken = #oauth_refresh_token{token = wh_json:get_value(<<"refresh_token">>, Cloud)},
                    {'ok', #oauth_token{expires=Expires}=Token} = kazoo_oauth_util:token(App, RefreshToken),
                    Auth = wh_util:to_list(kazoo_oauth_util:authorization_header(Token)),
                    wh_cache:store_local(?FAX_CACHE, {'gcp', PrinterId }, Auth, [{'expires', Expires }]),
                    {'ok', Auth};
                {'error', _}=E -> E
            end
    end.


