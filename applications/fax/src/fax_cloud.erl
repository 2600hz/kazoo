%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_cloud).

-export([handle_job_notify/2
        ,handle_push/2
        ,handle_faxbox_created/2, handle_faxbox_edited/2, handle_faxbox_deleted/2
        ,maybe_process_job/2
        ,check_registration/3
        ,get_printer_oauth_credentials/1
        ]).

-include("fax_cloud.hrl").

-define(JSON(L), kz_json:from_list(L)).
-define(DEFAULT_CLOUD_REG_SLEEP, 5000).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_job_notify(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_job_notify(JObj, _Props) ->
    _ = case kz_json:get_value(<<"Event-Name">>, JObj) of
            <<"outbound_fax_error">> ->
                'true' = kapi_notifications:fax_outbound_error_v(JObj);
            <<"outbound_fax">> ->
                'true' = kapi_notifications:fax_outbound_v(JObj);
            EventName ->
                lager:debug("wrong message type ~s : crashing this.",[EventName])
        end,

    JobId = kz_json:get_value(<<"Fax-JobId">>, JObj),
    AccountDb = kz_json:get_value(<<"Account-DB">>, JObj),
    lager:debug("checking if JobId ~s in db ~s is a cloud printer job",[JobId, AccountDb]),
    {FetchRes, MaybeFaxJObj} = kz_datamgr:open_doc(AccountDb, {<<"fax">>, JobId}),
    case FetchRes =:= 'ok'
        andalso kz_json:get_value(<<"cloud_job_id">>, MaybeFaxJObj)
    of
        'false' ->
            lager:debug("could not fetch cloud printer JobId ~p : ~p",[JobId, MaybeFaxJObj]);
        'undefined' ->
            lager:debug("jobId ~s is not a cloud printer job",[JobId]);
        CloudJobId ->
            lager:debug("jobId ~s is a cloud printer job with Id ~s",[JobId,CloudJobId]),
            PrinterId = kz_json:get_value(<<"cloud_printer_id">>, MaybeFaxJObj),
            process_job_outcome(PrinterId, CloudJobId, kz_json:get_value(<<"Event-Name">>, JObj))
    end.

-spec process_job_outcome(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
process_job_outcome(PrinterId, JobId, <<"outbound_fax_error">>) ->
    process_job_outcome(PrinterId, JobId, <<"ABORTED">>);
process_job_outcome(PrinterId, JobId, <<"outbound_fax">>) ->
    process_job_outcome(PrinterId, JobId, <<"DONE">>);
process_job_outcome(PrinterId, JobId, Status) ->
    update_job_status(PrinterId, JobId, Status).

-spec handle_push(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_push(JObj, _Props) ->
    'true' = kapi_xmpp:event_v(JObj),
    AppName = kz_json:get_value(<<"Application-Name">>, JObj),
    AppEvent = kz_json:get_value(<<"Application-Event">>, JObj),
    AppData = kz_json:get_value(<<"Application-Data">>, JObj),
    JID = kz_json:get_value(<<"JID">>, JObj),
    handle_push_event(JID, AppName, AppEvent, AppData).

-spec handle_push_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_push_event(_JID, <<"GCP">>, <<"Queued-Job">>, PrinterId) ->
    URL = <<?POOL_URL,PrinterId/binary>>,
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            Headers = [?GPC_PROXY_HEADER
                      ,{"Authorization", Authorization}
                      ],
            case kz_http:get(kz_term:to_list(URL), Headers) of
                {'ok', 200, _RespHeaders, RespBody} ->
                    JObj = kz_json:decode(RespBody),
                    JObjs = kz_json:get_value(<<"jobs">>, JObj, []),
                    _P = kz_process:spawn(fun maybe_process_job/2, [JObjs, Authorization]),
                    lager:debug("maybe processing job in ~p", [_P]);
                {'ok', 403, _RespHeaders, _RespBody} ->
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

-spec maybe_process_job(kz_json:objects(), string()) -> 'ok'.
maybe_process_job([], _Authorization) -> 'ok';
maybe_process_job([JObj | JObjs], Authorization) ->
    JobId = kz_doc:id(JObj),
    TicketObj = fetch_ticket(JobId, Authorization),
    TicketItem = kz_json:get_value([<<"print">>,<<"vendor_ticket_item">>], TicketObj, []),
    NumberObj = lists:foldl(fun(A,B) -> maybe_fax_number(A,B) end, kz_json:new(),TicketItem),
    PrinterId = kz_json:get_value(<<"printerid">>, JObj),
    FileURL = kz_json:get_value(<<"fileUrl">>, JObj),
    case kz_json:get_value(<<"Fax-Number">>, NumberObj) of
        'undefined' ->
            lager:debug("no fax number in job ticket ~s for printer ~s", [JobId, PrinterId]),
            update_job_status(PrinterId, JobId, <<"ABORTED">>);
        FaxNumber ->
            maybe_save_fax_document(JObj, JobId, PrinterId, FaxNumber, FileURL )
    end,
    maybe_process_job(JObjs, Authorization).

-spec maybe_fax_number(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_fax_number(A, B) ->
    case kz_doc:id(A) of
        <<"fax_number">> ->
            Number = fax_util:filter_numbers(kz_json:get_value(<<"value">>, A)),
            case kz_term:is_empty(Number) of
                'true' -> lager:debug("fax number is empty");
                'false' -> kz_json:set_value(<<"Fax-Number">>, Number, B)
            end;
        _Other -> B
    end.

-spec fetch_ticket(kz_term:ne_binary(), string()) -> kz_json:object().
fetch_ticket(JobId, Authorization) ->
    URL = <<?TICKET_URL, JobId/binary>>,
    Headers = [?GPC_PROXY_HEADER
              ,{"Authorization",Authorization}
              ],
    case kz_http:get(kz_term:to_list(URL), Headers) of
        {'ok', 200, _RespHeaders, RespBody} ->
            kz_json:decode(RespBody);
        Response ->
            lager:debug("unexpected result fetching ticket : ~p",[Response]),
            kz_json:new()
    end.

-spec update_job_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) -> any().
update_job_status(PrinterId, JobId, <<"IN_PROGRESS">>=Status) ->
    StateObj = kz_json:set_value(<<"state">>, kz_json:set_value(<<"type">>, Status, kz_json:new()), kz_json:new()),
    update_job_status(PrinterId, JobId, StateObj);
update_job_status(PrinterId, JobId, <<"DONE">>=Status) ->
    StateObj = kz_json:set_value(<<"state">>, kz_json:set_value(<<"type">>, Status, kz_json:new()), kz_json:new()),
    update_job_status(PrinterId, JobId, StateObj);
update_job_status(PrinterId, JobId, <<"ABORTED">>=Status) ->
    StateObj = kz_json:from_list(
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

-spec send_update_job_status(kz_term:ne_binary(), kz_term:ne_binary(), string()) -> 'ok'.
send_update_job_status(JobId, Status, Authorization) ->
    Headers = [?GPC_PROXY_HEADER
              ,{"Authorization", Authorization}
              ,{"Content-Type", "application/x-www-form-urlencoded"}
              ],

    Fields = [{"jobid", JobId}
             ,{"semantic_state_diff", kz_json:encode(Status)}
             ],

    Body = kz_http_util:props_to_querystring(Fields),

    case kz_http:post(kz_term:to_list(?JOBCTL_URL), Headers, Body) of
        {'ok', 200, _RespHeaders, RespBody} ->
            JObj = kz_json:decode(RespBody),
            case kz_json:is_true(<<"success">>, JObj) of
                'true' ->
                    lager:debug("cloud jobid ~s updated successfully", [JobId]);
                'false' ->
                    lager:error("error updating cloud jobid ~s : ~p", [JobId, JObj])
            end;
        _Response ->
            lager:debug("unexpected response  sending update_job_status: ~p", [_Response])
    end.

-spec download_file(kz_term:ne_binary(), string()) ->
                           {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                           {'error', any()}.
download_file(URL, Authorization) ->
    Headers = [?GPC_PROXY_HEADER
              ,{"Authorization", Authorization}
              ],
    case kz_http:get(kz_term:to_list(URL), Headers) of
        {'ok', 200, RespHeaders, RespBody} ->
            CT = kz_term:to_binary(props:get_value("content-type", RespHeaders)),
            Ext = kz_mime:to_extension(CT),
            FileName = list_to_binary(["/tmp/fax_printer_"
                                      ,kz_term:to_binary(kz_time:now_s())
                                      ,"."
                                      ,Ext
                                      ]),

            case file:write_file(FileName, RespBody) of
                'ok' -> {'ok', CT, RespBody};
                {'error', _}=Error ->
                    lager:debug("error writing file ~s from ~s : ~p", [URL, FileName, Error]),
                    Error
            end;
        Response ->
            lager:debug("error downloading file ~s : ~p",[URL, Response]),
            {'error', Response}
    end.

-spec maybe_save_fax_document(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_save_fax_document(Job, JobId, PrinterId, FaxNumber, FileURL ) ->
    case save_fax_document(Job, JobId, PrinterId, FaxNumber) of
        {'ok', JObj} ->
            maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL );
        {'error', 'conflict'} ->
            lager:debug("cloud job ~s already exists, skipping", [JobId]);
        {'error', _E} ->
            lager:debug("got error saving fax job ~s : ~p", [JobId, _E])
    end.

-spec maybe_save_fax_attachment(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_save_fax_attachment(JObj, JobId, PrinterId, FileURL ) ->
    case get_printer_oauth_credentials(PrinterId) of
        {'ok', Authorization} ->
            case download_file(FileURL, Authorization) of
                {'ok', CT, FileContents} ->
                    case kz_fax_attachment:save_outbound(?KZ_FAXES_DB, JObj, FileContents, CT) of
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

-spec save_fax_document(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
save_fax_document(Job, JobId, PrinterId, FaxNumber ) ->
    {'ok', FaxBoxDoc} = get_faxbox_doc(PrinterId),

    AccountId = kz_doc:account_id(FaxBoxDoc),
    AccountDb = ?KZ_FAXES_DB,
    ResellerId = case kzd_services:reseller_id(FaxBoxDoc) of
                     'undefined' -> kz_services_reseller:get_id(AccountId);
                     TheResellerId -> TheResellerId
                 end,
    OwnerId = kz_json:get_value(<<"ownerId">>, Job),
    FaxBoxUserEmail = kz_json:get_value(<<"owner_email">>, FaxBoxDoc),

    FaxBoxEmailNotify = kz_json:get_value([<<"notifications">>
                                          ,<<"outbound">>
                                          ,<<"email">>
                                          ,<<"send_to">>
                                          ]
                                         ,FaxBoxDoc
                                         ,[]
                                         ),

    FaxBoxNotify = kz_json:set_value([<<"notifications">>
                                     ,<<"outbound">>
                                     ,<<"email">>
                                     ,<<"send_to">>
                                     ]
                                    ,fax_util:notify_email_list(OwnerId, FaxBoxUserEmail, FaxBoxEmailNotify)
                                    ,FaxBoxDoc
                                    ),

    Notify = kz_json:get_value([<<"notifications">>,<<"outbound">>],FaxBoxNotify),
    Props = props:filter_undefined(
              [{<<"from_name">>,kz_json:get_value(<<"caller_name">>,FaxBoxDoc)}
              ,{<<"from_number">>,kz_json:get_value(<<"caller_id">>,FaxBoxDoc)}
              ,{<<"fax_identity_name">>, kz_json:get_value(<<"fax_header">>, FaxBoxDoc)}
              ,{<<"fax_identity_number">>, kz_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
              ,{<<"fax_timezone">>, kzd_fax_box:timezone(FaxBoxDoc)}
              ,{<<"to_name">>,FaxNumber}
              ,{<<"to_number">>,FaxNumber}
              ,{<<"retries">>,kz_json:get_value(<<"retries">>,FaxBoxDoc,3)}
              ,{<<"notifications">>, Notify }
              ,{<<"faxbox_id">>, kz_doc:id(FaxBoxDoc)}
              ,{<<"folder">>, <<"outbox">>}
              ,{<<"cloud_printer_id">>, PrinterId}
              ,{<<"cloud_job_id">>, JobId}
              ,{<<"cloud_job">>, Job}
              ,{<<"_id">>, JobId}
              ]),
    Doc = kz_json:set_values([{<<"pvt_type">>, <<"fax">>}
                             ,{<<"pvt_job_status">>, <<"queued">>}
                             ,{<<"pvt_created">>, kz_time:now_s()}
                             ,{<<"attempts">>, 0}
                             ,{<<"pvt_account_id">>, AccountId}
                             ,{<<"pvt_account_db">>, AccountDb}
                             ,{<<"pvt_reseller_id">>, ResellerId}
                             ]
                            ,kz_json_schema:add_defaults(kz_json:from_list(Props), <<"faxes">>)
                            ),
    kz_datamgr:save_doc(?KZ_FAXES_DB, Doc).

-spec get_faxbox_doc(kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
get_faxbox_doc(PrinterId) ->
    case kz_cache:peek_local(?CACHE_NAME, {'faxbox', PrinterId }) of
        {'ok', _Doc}=OK -> OK;
        {'error', _} ->
            fetch_faxbox_doc(PrinterId)
    end.

-spec fetch_faxbox_doc(kz_term:ne_binary()) ->
                              {'ok', kz_json:object()} |
                              {'error', any()}.
fetch_faxbox_doc(PrinterId) ->
    ViewOptions = [{'key', PrinterId}, 'include_docs'],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxbox/cloud">>, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', [JObj]} ->
            Doc = kz_json:get_value(<<"doc">>, JObj),
            FaxBoxDoc = maybe_get_faxbox_owner_email(Doc),
            CacheProps = [{'origin', [{'db', ?KZ_FAXES_DB, kz_doc:id(Doc)}] }],
            kz_cache:store_local(?CACHE_NAME, {'faxbox', PrinterId }, FaxBoxDoc, CacheProps),
            {'ok', FaxBoxDoc}
    end.

-spec maybe_get_faxbox_owner_email(kz_json:object()) -> kz_json:object().
maybe_get_faxbox_owner_email(FaxBoxDoc) ->
    case kz_json:get_value(<<"owner_id">>, FaxBoxDoc) of
        'undefined' -> FaxBoxDoc;
        OwnerId -> get_faxbox_owner_email(FaxBoxDoc, OwnerId)
    end.

-spec get_faxbox_owner_email(kz_json:object(), kz_term:ne_binary()) ->
                                    kz_json:object().
get_faxbox_owner_email(FaxBoxDoc, OwnerId) ->
    AccountId = kz_doc:account_id(FaxBoxDoc),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', OwnerDoc} ->
            case kz_json:get_value(<<"email">>, OwnerDoc) of
                'undefined' -> FaxBoxDoc;
                OwnerEmail -> kz_json:set_value(<<"owner_email">>, OwnerEmail, FaxBoxDoc)
            end;
        _ ->
            FaxBoxDoc
    end.

-spec get_printer_oauth_credentials(kz_term:ne_binary()) ->
                                           {'ok', string()} |
                                           {'error', any()}.
get_printer_oauth_credentials(PrinterId) ->
    case kz_cache:peek_local(?CACHE_NAME, {'gcp', PrinterId}) of
        {'ok', _Auth}=OK -> OK;
        {'error', _} ->
            fetch_printer_oauth_credentials(PrinterId)
    end.

-spec fetch_printer_oauth_credentials(kz_term:ne_binary()) ->
                                             {'ok', string()} |
                                             {'error', any()}.
fetch_printer_oauth_credentials(PrinterId) ->
    case get_faxbox_doc(PrinterId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            {'ok',App} = kazoo_oauth_util:get_oauth_app(kz_json:get_value(<<"pvt_cloud_oauth_app">>, JObj)),
            RefreshToken = #oauth_refresh_token{token = kz_json:get_value(<<"pvt_cloud_refresh_token">>, JObj)},
            {'ok', #oauth_token{expires=Expires}=Token} = kazoo_oauth_util:token(App, RefreshToken),
            Auth = kz_term:to_list(kazoo_oauth_util:authorization_header(Token)),
            kz_cache:store_local(?CACHE_NAME, {'gcp', PrinterId}, Auth, [{'expires', Expires}]),
            {'ok', Auth}
    end.

-spec handle_faxbox_edited(kz_json:object(), kz_term:proplist()) -> pid().
handle_faxbox_edited(JObj, Props) ->
    handle_faxbox_created(JObj, Props).

-spec handle_faxbox_created(kz_json:object(), kz_term:proplist()) -> pid().
handle_faxbox_created(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    ID = kz_json:get_value(<<"ID">>, JObj),
    {'ok', Doc } = kz_datamgr:open_doc(?KZ_FAXES_DB, ID),
    State = kz_json:get_value(<<"pvt_cloud_state">>, Doc),
    ResellerId = kzd_services:reseller_id(Doc),
    AppId = kapps_account_config:get(ResellerId, ?CONFIG_CAT, <<"cloud_oauth_app">>),
    kz_process:spawn(fun check_registration/3, [AppId, State, Doc]).

-spec check_registration(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() ) -> 'ok'.
check_registration('undefined', _, _JObj) -> 'ok';
check_registration(_, 'undefined', _JObj) -> 'ok';
check_registration(_, <<"expired">>, _JObj) -> 'ok';
check_registration(AppId, <<"registered">>, JObj) ->
    PoolingUrlPart = kz_json:get_value(<<"pvt_cloud_polling_url">>, JObj),
    PoolingUrl = kz_term:to_list(<<PoolingUrlPart/binary, AppId/binary>>),
    PrinterId = kz_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    case kz_http:get(PoolingUrl, [?GPC_PROXY_HEADER]) of
        {'ok', 200, _RespHeaders, RespXML} ->
            JObjPool = kz_json:decode(RespXML),
            Result = kz_json:get_value(<<"success">>, JObjPool, 'false'),
            process_registration_result(Result, AppId, JObj, JObjPool);
        _A ->
            lager:debug("unexpected result checking registration of printer ~s: ~p", [PrinterId, _A])
    end;
check_registration(_, _, _JObj) -> 'ok'.

-spec process_registration_result(boolean(), kz_term:ne_binary(), kz_json:object(), kz_json:object() ) -> any().
process_registration_result('true', AppId, JObj, Result) ->
    _AccountId = kz_doc:account_id(JObj),
    _PrinterId = kz_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    FaxBoxId = kz_doc:id(JObj),
    Scope = kz_json:get_value(<<"pvt_cloud_oauth_scope">>, JObj),
    {'ok', App } = kazoo_oauth_util:get_oauth_app(AppId),
    AuthorizationCode = kz_json:get_value(<<"authorization_code">>, Result),
    JID = kz_json:get_value(<<"xmpp_jid">>, Result),
    UserEmail = kz_json:get_value(<<"user_email">>, Result),
    {'ok', Token} = kazoo_oauth_util:refresh_token(App, Scope, AuthorizationCode, [?GPC_PROXY_HEADER], <<"oob">>),

    RefreshToken = kz_json:get_value(<<"refresh_token">>, Token),
    update_printer(
      kz_json:set_values([{<<"pvt_cloud_authorization_code">>, AuthorizationCode}
                         ,{<<"pvt_cloud_refresh_token">>, RefreshToken}
                         ,{<<"pvt_cloud_user_email">>, UserEmail}
                         ,{<<"pvt_cloud_xmpp_jid">>, JID}
                         ,{<<"pvt_cloud_state">>, <<"claimed">>}
                         ,{<<"pvt_cloud_oauth_app">>, AppId}
                         ]
                        ,JObj
                        )
     ),

    timer:sleep(15 * ?MILLISECONDS_IN_SECOND),
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"start">>}
                ,{<<"Application-Name">>, <<"fax">>}
                ,{<<"Application-Event">>, <<"claimed">>}
                ,{<<"Application-Data">>, FaxBoxId}
                ,{<<"JID">>, JID}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_xmpp:publish_event(Payload);
process_registration_result('false', AppId, JObj, _Result) ->
    PrinterId = kz_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    TokenDuration = kz_json:get_integer_value(<<"pvt_cloud_token_duration">>, JObj),
    UnixTS = kz_json:get_integer_value(<<"pvt_cloud_created_time">>, JObj),
    CreatedTime = kz_time:unix_timestamp_to_gregorian_seconds(UnixTS),
    InviteUrl = kz_json:get_value(<<"pvt_cloud_connector_claim_url">>, JObj),
    Elapsed = kz_time:elapsed_s(CreatedTime),

    case Elapsed > TokenDuration of
        'true' ->
            lager:debug("token expired before printer ~s was claimed at ~s",[PrinterId,InviteUrl]),
            Keys = [ K || <<"pvt_cloud", _/binary>> = K <- kz_json:get_keys(JObj)],
            update_printer(
              kz_json:set_values([{<<"pvt_cloud_state">>, <<"expired">>}]
                                ,kz_json:delete_keys(Keys, JObj)
                                )
             );
        'false' ->
            SleepTime = kapps_config:get_integer(?CONFIG_CAT, <<"cloud_registration_pool_interval">>, ?DEFAULT_CLOUD_REG_SLEEP),
            lager:debug("printer ~s not claimed at ~s. sleeping for ~B seconds, Elapsed/Duration (~p/~p)."
                       ,[PrinterId,InviteUrl, SleepTime div 1000 , Elapsed, TokenDuration]
                       ),
            timer:sleep(SleepTime),
            check_registration(AppId, <<"registered">>, JObj)
    end.

-spec update_printer(kz_json:object()) -> 'ok'.
update_printer(JObj) ->
    AccountDb = kz_doc:account_db(JObj),
    {'ok', _} = kz_datamgr:ensure_saved(AccountDb, JObj),
    {'ok', _} = kz_datamgr:ensure_saved(?KZ_FAXES_DB, JObj),
    'ok'.

-spec handle_faxbox_deleted(kz_json:object(), kz_term:proplist()) -> any().
handle_faxbox_deleted(JObj, _Props) ->
    lager:debug("faxbox_deleted ~p",[JObj]),
    'true' = kapi_conf:doc_update_v(JObj),
    ID = kz_json:get_value(<<"ID">>, JObj),
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"stop">>}
                ,{<<"Application-Name">>, <<"fax">>}
                ,{<<"Application-Event">>, <<"deleted">>}
                ,{<<"Application-Data">>, ID}
                ,{<<"JID">>, ID}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_xmpp:publish_event(Payload).
