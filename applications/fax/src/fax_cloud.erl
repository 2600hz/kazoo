%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_cloud).

-export([handle_job_notify/2
        ,handle_push/2
        ,handle_faxbox_created/2, handle_faxbox_edited/2, handle_faxbox_deleted/2
        ,maybe_process_job/2
        ,check_registration/3
        ,get_printer_oauth_credentials/1
        ]).

-include("fax_cloud.hrl").

-define(JSON(L), wh_json:from_list(L)).
-define(DEFAULT_CLOUD_REG_SLEEP, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec handle_job_notify(wh_json:object(), wh_proplist()) -> 'ok'.
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
    {'ok', FaxJObj} = couch_mgr:open_doc(?WH_FAXES_DB, JobId),
    lager:debug("Checking if JobId ~s is a cloud printer job",[JobId]),
    case wh_json:get_value(<<"cloud_job_id">>, FaxJObj) of
        'undefined' ->
            lager:debug("JobId ~s is not a cloud printer job",[JobId]);
        CloudJobId ->
            lager:debug("JobId ~s is a cloud printer job with Id ~s",[JobId,CloudJobId]),
            PrinterId = wh_json:get_value(<<"cloud_printer_id">>, FaxJObj),
            process_job_outcome(PrinterId, CloudJobId, wh_json:get_value(<<"Event-Name">>, JObj))
    end.

-spec process_job_outcome(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
process_job_outcome(PrinterId, JobId, <<"outbound_fax_error">>) ->
    process_job_outcome(PrinterId, JobId, <<"ABORTED">>);
process_job_outcome(PrinterId, JobId, <<"outbound_fax">>) ->
    process_job_outcome(PrinterId, JobId, <<"DONE">>);
process_job_outcome(PrinterId, JobId, Status) ->
    update_job_status(PrinterId, JobId, Status).

-spec handle_push(wh_json:object(), wh_proplist()) -> 'ok'.
handle_push(JObj, _Props) ->
    'true' = wapi_xmpp:event_v(JObj),
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    AppEvent = wh_json:get_value(<<"Application-Event">>, JObj),
    AppData = wh_json:get_value(<<"Application-Data">>, JObj),
    JID = wh_json:get_value(<<"JID">>, JObj),
    handle_push_event(JID, AppName, AppEvent, AppData).

-spec handle_push_event(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
handle_push_event(_JID, <<"GCP">>, <<"Queued-Job">>, PrinterId) ->
    URL = <<?POOL_URL,PrinterId/binary>>,
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            Headers = [?GPC_PROXY_HEADER , {"Authorization",Authorization}],
            case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
                {'ok', "200", _RespHeaders, RespBody} ->
                    JObj = wh_json:decode(RespBody),
                    JObjs = wh_json:get_value(<<"jobs">>, JObj, []),
                    _P = wh_util:spawn(?MODULE, 'maybe_process_job', [JObjs, Authorization]),
                    lager:debug("maybe processing job in ~p", [_P]);
                {'ok', "403", _RespHeaders, _RespBody} ->
                    lager:debug("something wrong with oauth credentials"),
                    _ = [lager:debug("resp header: ~p", [_RespHeader]) || _RespHeader <- _RespHeaders],
                    lager:debug("body: ~s", [_RespBody]);
                _Other ->
                    lager:debug("unexpected response from gcp ~p", [_Other])
            end;
        {'error', E} ->
            lager:debug("no credentials for gcp printer ~s/~p",[PrinterId, E])
    end;
handle_push_event(JID, AppName, AppEvent, AppData) ->
    lager:debug("unhandled xmpp push event ~s/~s/~s/~p",[JID, AppName, AppEvent, AppData]).

-spec maybe_process_job(wh_json:objects(), ne_binary()) -> 'ok'.
maybe_process_job([], _Authorization) -> 'ok';
maybe_process_job([JObj | JObjs], Authorization) ->
    JobId = wh_doc:id(JObj),
    TicketObj = fetch_ticket(JobId, Authorization),
    TicketItem = wh_json:get_value([<<"print">>,<<"vendor_ticket_item">>], TicketObj, []),
    NumberObj = lists:foldl(fun(A,B) -> maybe_fax_number(A,B) end, wh_json:new(),TicketItem),
    PrinterId = wh_json:get_value(<<"printerid">>, JObj),
    FileURL = wh_json:get_value(<<"fileUrl">>, JObj),
    case wh_json:get_value(<<"Fax-Number">>, NumberObj) of
        'undefined' ->
            lager:debug("no fax number in job ticket ~s for printer ~s", [JobId, PrinterId]),
            update_job_status(PrinterId, JobId, <<"ABORTED">>);
        FaxNumber ->
            maybe_save_fax_document(JObj, JobId, PrinterId, FaxNumber, FileURL )
    end,
    maybe_process_job(JObjs,Authorization).

-spec maybe_fax_number(wh_json:object(), wh_json:object()) -> wh_json:object().
maybe_fax_number(A, B) ->
    case wh_doc:id(A) of
        <<"fax_number">> ->
            Number = fax_util:filter_numbers(wh_json:get_value(<<"value">>, A)),
            case wh_util:is_empty(Number) of
                'true' -> lager:debug("fax number is empty");
                'false' -> wh_json:set_value(<<"Fax-Number">>, Number, B)
            end;
        _Other -> B
    end.

-spec fetch_ticket(ne_binary(), ne_binary()) -> wh_json:object().
fetch_ticket(JobId, Authorization) ->
    URL = <<?TICKET_URL,JobId/binary>>,
    Headers = [?GPC_PROXY_HEADER
               ,{"Authorization",Authorization}
              ],
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
        {'ok', "200", _RespHeaders, RespBody} ->
            wh_json:decode(RespBody);
        Response ->
            lager:debug("unexpected result fetching ticket : ~p",[Response]),
            wh_json:new()
    end.

-spec update_job_status(ne_binary(), ne_binary(), ne_binary() | wh_json:object()) -> _.
update_job_status(PrinterId, JobId, <<"IN_PROGRESS">>=Status) ->
    StateObj = wh_json:set_value(<<"state">>, wh_json:set_value(<<"type">>, Status, wh_json:new()), wh_json:new()),
    update_job_status(PrinterId, JobId, StateObj);
update_job_status(PrinterId, JobId, <<"DONE">>=Status) ->
    StateObj = wh_json:set_value(<<"state">>, wh_json:set_value(<<"type">>, Status, wh_json:new()), wh_json:new()),
    update_job_status(PrinterId, JobId, StateObj);
update_job_status(PrinterId, JobId, <<"ABORTED">>=Status) ->
    StateObj = wh_json:from_list(
                 [{<<"state">>
                   ,?JSON([{<<"type">>, Status}
                           ,{<<"device_action_cause">>
                             ,?JSON([{<<"error_code">>,<<"OTHER">>}])
                            }
                          ]
                         )
                  }
                 ]),
    update_job_status(PrinterId, JobId, StateObj);
update_job_status(PrinterId, JobId, Status) ->
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            send_update_job_status(JobId, Status, Authorization);
        {'error', E} ->
            lager:debug("error getting printer (~s) oauth credentials when updating job (~s) status : ~p",[PrinterId, JobId, E])
    end.

-spec send_update_job_status(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_update_job_status(JobId, Status, Authorization) ->
    Headers = [?GPC_PROXY_HEADER
               ,{"Authorization",Authorization}
               ,{"Content-Type","application/x-www-form-urlencoded"}
              ],

    Fields = [{"jobid", JobId}
              ,{"semantic_state_diff", wh_json:encode(Status)}
             ],

    Body = props:to_querystring(Fields),

    case ibrowse:send_req(wh_util:to_list(?JOBCTL_URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespBody} ->
            JObj = wh_json:decode(RespBody),
            case wh_json:is_true(<<"success">>, JObj) of
                'true' ->
                    lager:debug("cloud jobid ~s updated successfully", [JobId]);
                'false' ->
                    lager:error("error updating cloud jobid ~s : ~p", [JobId, JObj])
            end;
        _Response ->
            lager:debug("unexpected response  sending update_job_status: ~p", [_Response])
    end.

-spec download_file(ne_binary(), ne_binary()) ->
                           {'ok', ne_binary(), ne_binary()} |
                           {'error', _}.
download_file(URL, Authorization) ->
    Headers = [?GPC_PROXY_HEADER , {"Authorization",Authorization}],
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'get') of
        {'ok', "200", RespHeaders, RespBody} ->
            CT = wh_util:to_binary(props:get_value("Content-Type", RespHeaders)),
            Ext = fax_util:content_type_to_extension(CT),
            FileName = <<"/tmp/fax_printer_"
                         ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
                         ,"."
                         ,Ext/binary
                       >>,
            case file:write_file(FileName,RespBody) of
                'ok' -> {'ok', CT, RespBody};
                {'error', _}=Error ->
                    lager:debug("error writing file ~s from ~s : ~p", [URL, FileName, Error]),
                    Error
            end;
        Response ->
            lager:debug("error downloading file ~s : ~p",[URL, Response]),
            {'error', Response}
    end.

-spec maybe_save_fax_document(wh_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
maybe_save_fax_document(Job, JobId, PrinterId, FaxNumber, FileURL ) ->
    case save_fax_document(Job, JobId, PrinterId, FaxNumber) of
        {'ok', JObj} ->
            maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL );
        {'error', 'conflict'} ->
            lager:debug("cloud job ~s already exists, skipping", [JobId]);
        {'error', _E} ->
            lager:debug("got error saving fax job ~s : ~p", [JobId, _E])
    end.

-spec maybe_save_fax_attachment(wh_json:object(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL ) ->
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            case download_file(FileURL,Authorization) of
                {'ok', CT, FileContents} ->
                    case fax_util:save_fax_attachment(JObj, FileContents, CT) of
                        {'ok', _} -> update_job_status(PrinterId, JobId, <<"IN_PROGRESS">>);
                        {'error', E} ->
                            lager:debug("error saving attachment for JobId ~s : ~p",[JobId, E])
                    end;
                {'error', Error} ->
                    lager:debug("error downloading file for JobId ~s : ~p",[JobId, Error])
            end;
        {'error', E} ->
            lager:debug("error getting printer (~s) oauth credentials for JobId (~s) : ~p",[PrinterId, JobId, E])
    end.

-spec save_fax_document(wh_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               {'error', _}.
save_fax_document(Job, JobId, PrinterId, FaxNumber ) ->
    {'ok', FaxBoxDoc} = get_faxbox_doc(PrinterId),

    AccountId = wh_doc:account_id(FaxBoxDoc),
    AccountDb = ?WH_FAXES_DB,
    ResellerId = case kzd_services:reseller_id(FaxBoxDoc) of
                     'undefined' -> wh_services:find_reseller_id(AccountId);
                     TheResellerId -> TheResellerId
                 end,
    OwnerId = wh_json:get_value(<<"ownerId">>, Job),
    FaxBoxUserEmail = wh_json:get_value(<<"owner_email">>, FaxBoxDoc),

    FaxBoxEmailNotify = wh_json:get_value([<<"notifications">>
                                           ,<<"outbound">>
                                           ,<<"email">>
                                           ,<<"send_to">>
                                          ]
                                          ,FaxBoxDoc
                                          ,[]
                                         ),

    FaxBoxNotify = wh_json:set_value([<<"notifications">>
                                      ,<<"outbound">>
                                      ,<<"email">>
                                      ,<<"send_to">>
                                     ]
                                     ,fax_util:notify_email_list(OwnerId, FaxBoxUserEmail, FaxBoxEmailNotify)
                                     ,FaxBoxDoc
                                    ),

    Notify = wh_json:get_value([<<"notifications">>,<<"outbound">>],FaxBoxNotify),
    Props = props:filter_undefined(
              [{<<"from_name">>,wh_json:get_value(<<"caller_name">>,FaxBoxDoc)}
               ,{<<"from_number">>,wh_json:get_value(<<"caller_id">>,FaxBoxDoc)}
               ,{<<"fax_identity_name">>, wh_json:get_value(<<"fax_header">>, FaxBoxDoc)}
               ,{<<"fax_identity_number">>, wh_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
               ,{<<"fax_timezone">>, kzd_fax_box:timezone(FaxBoxDoc)}
               ,{<<"to_name">>,FaxNumber}
               ,{<<"to_number">>,FaxNumber}
               ,{<<"retries">>,wh_json:get_value(<<"retries">>,FaxBoxDoc,3)}
               ,{<<"notifications">>, Notify }
               ,{<<"faxbox_id">>, wh_doc:id(FaxBoxDoc)}
               ,{<<"folder">>, <<"outbox">>}
               ,{<<"cloud_printer_id">>, PrinterId}
               ,{<<"cloud_job_id">>, JobId}
               ,{<<"cloud_job">>, Job}
               ,{<<"_id">>, JobId}
              ]),
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                              ,{<<"pvt_job_status">>, <<"queued">>}
                              ,{<<"pvt_created">>, wh_util:current_tstamp()}
                              ,{<<"attempts">>, 0}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_reseller_id">>, ResellerId}
                             ]
                             ,wh_json_schema:add_defaults(wh_json:from_list(Props), <<"faxes">>)
                            ),
    couch_mgr:save_doc(?WH_FAXES_DB, Doc).

-spec get_faxbox_doc(ne_binary()) ->
                            {'ok', wh_json:object()} |
                            {'error', _}.
get_faxbox_doc(PrinterId) ->
    case wh_cache:peek_local(?FAX_CACHE, {'faxbox', PrinterId }) of
        {'ok', _Doc}=OK -> OK;
        {'error', _} ->
            fetch_faxbox_doc(PrinterId)
    end.

-spec fetch_faxbox_doc(ne_binary()) ->
                              {'ok', wh_json:object()} |
                              {'error', _}.
fetch_faxbox_doc(PrinterId) ->
    ViewOptions = [{'key', PrinterId}, 'include_docs'],
    case couch_mgr:get_results(?WH_FAXES_DB, <<"faxbox/cloud">>, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', [JObj]} ->
            Doc = wh_json:get_value(<<"doc">>, JObj),
            FaxBoxDoc = maybe_get_faxbox_owner_email(Doc),
            CacheProps = [{'origin', [{'db', ?WH_FAXES_DB, wh_doc:id(Doc)}] }],
            wh_cache:store_local(?FAX_CACHE, {'faxbox', PrinterId }, FaxBoxDoc, CacheProps),
            {'ok', FaxBoxDoc}
    end.

-spec maybe_get_faxbox_owner_email(wh_json:object()) -> wh_json:object().
maybe_get_faxbox_owner_email(FaxBoxDoc) ->
    case wh_json:get_value(<<"owner_id">>, FaxBoxDoc) of
        'undefined' -> FaxBoxDoc;
        OwnerId -> get_faxbox_owner_email(FaxBoxDoc, OwnerId)
    end.

-spec get_faxbox_owner_email(wh_json:object(), ne_binary()) ->
                                    wh_json:object().
get_faxbox_owner_email(FaxBoxDoc, OwnerId) ->
    AccountId = wh_doc:account_id(FaxBoxDoc),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', OwnerDoc} ->
            case wh_json:get_value(<<"email">>, OwnerDoc) of
                'undefined' -> FaxBoxDoc;
                OwnerEmail -> wh_json:set_value(<<"owner_email">>, OwnerEmail, FaxBoxDoc)
            end;
        _ ->
            FaxBoxDoc
    end.

-spec get_printer_oauth_credentials(ne_binary()) ->
                                           {'ok', ne_binary()} |
                                           {'error', _}.
get_printer_oauth_credentials(PrinterId) ->
    case wh_cache:peek_local(?FAX_CACHE, {'gcp', PrinterId }) of
        {'ok', _Auth}=OK -> OK;
        {'error', _} ->
            fetch_printer_oauth_credentials(PrinterId)
    end.

-spec fetch_printer_oauth_credentials(ne_binary()) ->
                                             {'ok', ne_binary()} |
                                             {'error', _}.
fetch_printer_oauth_credentials(PrinterId) ->
    case get_faxbox_doc(PrinterId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            {'ok',App} = kazoo_oauth_util:get_oauth_app(wh_json:get_value(<<"pvt_cloud_oauth_app">>, JObj)),
            RefreshToken = #oauth_refresh_token{token = wh_json:get_value(<<"pvt_cloud_refresh_token">>, JObj)},
            {'ok', #oauth_token{expires=Expires}=Token} = kazoo_oauth_util:token(App, RefreshToken),
            Auth = wh_util:to_list(kazoo_oauth_util:authorization_header(Token)),
            wh_cache:store_local(?FAX_CACHE, {'gcp', PrinterId}, Auth, [{'expires', Expires}]),
            {'ok', Auth}
    end.

-spec handle_faxbox_edited(wh_json:object(), wh_proplist()) -> pid().
handle_faxbox_edited(JObj, Props) ->
    handle_faxbox_created(JObj, Props).

-spec handle_faxbox_created(wh_json:object(), wh_proplist()) -> pid().
handle_faxbox_created(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    ID = wh_json:get_value(<<"ID">>, JObj),
    {'ok', Doc } = couch_mgr:open_doc(?WH_FAXES_DB, ID),
    State = wh_json:get_value(<<"pvt_cloud_state">>, Doc),
    ResellerId = kzd_services:reseller_id(Doc),
    AppId = whapps_account_config:get(ResellerId, ?CONFIG_CAT, <<"cloud_oauth_app">>),
    wh_util:spawn(?MODULE, 'check_registration', [AppId, State, Doc]).

-spec check_registration(ne_binary(), ne_binary(), wh_json:object() ) -> 'ok'.
check_registration('undefined', _, _JObj) -> 'ok';
check_registration(_, 'undefined', _JObj) -> 'ok';
check_registration(_, <<"expired">>, _JObj) -> 'ok';
check_registration(AppId, <<"registered">>, JObj) ->
    PoolingUrlPart = wh_json:get_value(<<"pvt_cloud_polling_url">>, JObj),
    PoolingUrl = wh_util:to_list(<<PoolingUrlPart/binary, AppId/binary>>),
    PrinterId = wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    case ibrowse:send_req(PoolingUrl, [?GPC_PROXY_HEADER], 'get') of
        {'ok', "200", _RespHeaders, RespXML} ->
            JObjPool = wh_json:decode(RespXML),
            Result = wh_json:get_value(<<"success">>, JObjPool, 'false'),
            process_registration_result(Result, AppId, JObj,JObjPool );
        _A ->
            lager:debug("unexpected result checking registration of printer ~s: ~p", [PrinterId, _A])
    end;
check_registration(_, _, _JObj) -> 'ok'.

-spec process_registration_result(boolean(), ne_binary(), wh_json:object(), wh_json:object() ) -> _.
process_registration_result('true', AppId, JObj, Result) ->
    _AccountId = wh_doc:account_id(JObj),
    _PrinterId = wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    FaxBoxId = wh_doc:id(JObj),
    Scope = wh_json:get_value(<<"pvt_cloud_oauth_scope">>, JObj),
    {'ok', App } = kazoo_oauth_util:get_oauth_app(AppId),
    AuthorizationCode = wh_json:get_value(<<"authorization_code">>, Result),
    JID = wh_json:get_value(<<"xmpp_jid">>, Result),
    UserEmail = wh_json:get_value(<<"user_email">>, Result),
    {'ok', Token} = kazoo_oauth_util:refresh_token(App, Scope, AuthorizationCode, [?GPC_PROXY_HEADER], <<"oob">>),

    RefreshToken = wh_json:get_value(<<"refresh_token">>, Token),
    update_printer(
      wh_json:set_values(
        [{<<"pvt_cloud_authorization_code">>, AuthorizationCode}
         ,{<<"pvt_cloud_refresh_token">>, RefreshToken}
         ,{<<"pvt_cloud_user_email">>, UserEmail}
         ,{<<"pvt_cloud_xmpp_jid">>, JID}
         ,{<<"pvt_cloud_state">>, <<"claimed">>}
         ,{<<"pvt_cloud_oauth_app">>, AppId}
        ]
        ,JObj
       )),

    timer:sleep(15 * ?MILLISECONDS_IN_SECOND),
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"start">>}
                 ,{<<"Application-Name">>, <<"fax">>}
                 ,{<<"Application-Event">>, <<"claimed">>}
                 ,{<<"Application-Data">>, FaxBoxId}
                 ,{<<"JID">>, JID}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_xmpp:publish_event(Payload);
process_registration_result('false', AppId, JObj, _Result) ->
    PrinterId = wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    TokenDuration = wh_json:get_integer_value(<<"pvt_cloud_token_duration">>, JObj),
    UnixTS = wh_json:get_integer_value(<<"pvt_cloud_created_time">>, JObj),
    CreatedTime = wh_util:unix_timestamp_to_gregorian_seconds(UnixTS),
    InviteUrl = wh_json:get_value(<<"pvt_cloud_connector_claim_url">>, JObj),
    Elapsed = wh_util:elapsed_s(CreatedTime),

    case Elapsed > TokenDuration of
        'true' ->
            lager:debug("Token expired before printer ~s was claimed at ~s",[PrinterId,InviteUrl]),
            Keys = [ K || <<"pvt_cloud", _/binary>> = K <- wh_json:get_keys(JObj)],
            update_printer(
              wh_json:set_values(
                [{<<"pvt_cloud_state">>, <<"expired">>}]
                ,wh_json:delete_keys(Keys, JObj)
               )
             );
        'false' ->
            SleepTime = whapps_config:get_integer(?CONFIG_CAT, <<"cloud_registration_pool_interval">>, ?DEFAULT_CLOUD_REG_SLEEP),
            lager:debug("Printer ~s not claimed at ~s. sleeping for ~B seconds, Elapsed/Duration (~p/~p)."
                        ,[PrinterId,InviteUrl, SleepTime div 1000 , Elapsed, TokenDuration]
                       ),
            timer:sleep(SleepTime),
            check_registration(AppId, <<"registered">>, JObj)
    end.

-spec update_printer(wh_json:object()) -> 'ok'.
update_printer(JObj) ->
    AccountDb = wh_doc:account_db(JObj),
    {'ok', _} = couch_mgr:ensure_saved(AccountDb, JObj),
    {'ok', _} = couch_mgr:ensure_saved(?WH_FAXES_DB, JObj),
    'ok'.

-spec handle_faxbox_deleted(wh_json:object(), wh_proplist()) -> _.
handle_faxbox_deleted(JObj, _Props) ->
    lager:debug("faxbox_deleted ~p",[JObj]),
    'true' = wapi_conf:doc_update_v(JObj),
    ID = wh_json:get_value(<<"ID">>, JObj),
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"stop">>}
                 ,{<<"Application-Name">>, <<"fax">>}
                 ,{<<"Application-Event">>, <<"deleted">>}
                 ,{<<"Application-Data">>, ID}
                 ,{<<"JID">>, ID}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_xmpp:publish_event(Payload).
