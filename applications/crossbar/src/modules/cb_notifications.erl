%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_notifications).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,authorize/1, authorize/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/2
        ,content_types_accepted/2
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2, post/3
        ,delete/2

        ,flush/0

        ,acceptable_content_types/0
        ]).

-ifdef(TEST).
-export([merge_available/2]).
-endif.

-include("crossbar.hrl").

-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>, '*'}
                                 ,{<<"text">>, <<"plain">>, '*'}
                                 ]).
-define(CB_LIST, <<"notifications/crossbar_listing">>).
-define(PREVIEW, <<"preview">>).
-define(SMTP_LOG, <<"smtplog">>).
-define(CUSTOMER_UPDATE, <<"customer_update">>).
-define(MESSAGE, <<"message">>).
-define(CB_LIST_SMTP_LOG, <<"notifications/smtp_log">>).
-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).

-define(MACROS, <<"macros">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".notifications">>).
-define(NOTIFICATION_TIMEOUT
       ,kapps_config:get_integer(?MOD_CONFIG_CAT, <<"notification_timeout_ms">>, 5 * ?MILLISECONDS_IN_SECOND)
       ).
-define(INHERIT_DEFAULT_VALUES
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"inherit_default_values">>, 'false')
       ).

-define(PVT_TYPE_SMTPLOG, <<"notify_smtp_log">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.notifications">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize.notifications">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.notifications">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.notifications">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.notifications">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.notifications">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.notifications">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.notifications">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.notifications">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?SMTP_LOG) ->
    [?HTTP_GET];
allowed_methods(_NotificationId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_NotificationId, ?PREVIEW) ->
    [?HTTP_POST];
allowed_methods(?SMTP_LOG, _SMTPLogId) ->
    [?HTTP_GET];
allowed_methods(?CUSTOMER_UPDATE, ?MESSAGE) ->
    [?HTTP_POST].

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _Id) ->
    authorize(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), http_method(), req_nouns()) -> boolean().
authorize(_Context, ?HTTP_GET, [{<<"notifications">>, _}]) ->
    'true';
authorize(Context, _, [{<<"notifications">>, _}]) ->
    lager:debug("allowing system notifications mutation request"),
    cb_context:is_superduper_admin(Context);
authorize(_Context, _, [{<<"notifications">>, _}, {<<"accounts">>, [?NE_BINARY=_AccountId]}]) ->
    'true';
authorize(_Context, _, _Nouns) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /notifications => []
%%    /notifications/foo => [<<"foo">>]
%%    /notifications/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?SMTP_LOG) -> 'true';
resource_exists(_Id) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_Id, ?PREVIEW) -> 'true';
resource_exists(?SMTP_LOG, _Id) -> 'true';
resource_exists(?CUSTOMER_UPDATE, ?MESSAGE) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------

-spec acceptable_content_types() -> cowboy_content_types().
acceptable_content_types() ->
    ?NOTIFICATION_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token()) ->
          cb_context:context().
content_types_provided(Context, ?SMTP_LOG) ->
    Context;
content_types_provided(Context, Id) ->
    DbId = kz_notification:db_id(Id),
    ReqVerb = cb_context:req_verb(Context),
    content_types_provided_for_notifications(maybe_update_db(Context), DbId, ReqVerb).

-spec content_types_provided_for_notifications(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
content_types_provided_for_notifications(Context, Id, ?HTTP_GET) ->
    Context1 = read(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_set_content_types(Context1);
        _Status -> Context1
    end;
content_types_provided_for_notifications(Context, Id, ?HTTP_DELETE) ->
    Context1 = read(Context, Id, 'account'),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_set_content_types(Context1);
        _Status -> Context1
    end;
content_types_provided_for_notifications(Context, _Id, _Verb) ->
    Context.

-spec maybe_set_content_types(cb_context:context()) -> cb_context:context().
maybe_set_content_types(Context) ->
    case kz_doc:attachments(cb_context:doc(Context)) of
        'undefined' -> Context;
        Attachments -> set_content_types(Context, Attachments)
    end.

-spec set_content_types(cb_context:context(), kz_json:object()) -> cb_context:context().
set_content_types(Context, Attachments) ->
    ContentTypes = content_types_from_attachments(Attachments),
    lager:debug("setting content types for attachments: ~p", [ContentTypes]),
    cb_context:set_content_types_provided(Context, [{'to_json', ?JSON_CONTENT_TYPES}
                                                   ,{'to_binary', ContentTypes}
                                                   ]).

-spec content_types_from_attachments(kz_json:object()) -> [cowboy_content_type()].
content_types_from_attachments(Attachments) ->
    kz_json:foldl(fun content_type_from_attachment/3, [], Attachments).

-spec content_type_from_attachment(kz_json:path(), kz_json:object(), kz_term:proplist()) ->
          [cowboy_content_type()].
content_type_from_attachment(_Name, Attachment, Acc) ->
    case kz_json:get_value(<<"content_type">>, Attachment) of
        'undefined' -> Acc;
        ContentType ->
            [Lhs, Rhs] = binary:split(ContentType, <<"/">>),
            [{Lhs,Rhs, '*'} | Acc]
    end.

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, ?SMTP_LOG) ->
    Context;
content_types_accepted(Context, _Id) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
          cb_context:context().
content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?NOTIFICATION_MIME_TYPES}
          ,{'from_json', ?JSON_CONTENT_TYPES}
          ],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /notifications might load a list of skel objects
%% /notifications/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    ReqVerb = cb_context:req_verb(Context),
    validate_notifications(maybe_update_db(Context), ReqVerb).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?SMTP_LOG) ->
    crossbar_view:load_modb(Context, ?CB_LIST_SMTP_LOG, [{mapper, crossbar_view:map_value_fun()}]);
validate(Context, Id) ->
    ReqVerb = cb_context:req_verb(Context),
    DbId = kz_notification:db_id(Id),
    validate_notification(maybe_update_db(Context), DbId, ReqVerb).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, ?PREVIEW) ->
    DbId = kz_notification:db_id(Id),
    update_notification(maybe_update_db(Context), DbId);
validate(Context, ?SMTP_LOG, Id) ->
    load_smtp_log_doc(Id, Context);
validate(Context, ?CUSTOMER_UPDATE, ?MESSAGE) ->
    may_be_validate_recipient_id(Context).

-spec validate_notifications(cb_context:context(), http_method()) -> cb_context:context().
validate_notifications(Context, ?HTTP_GET) ->
    summary(Context);
validate_notifications(Context, ?HTTP_PUT) ->
    validate_action(Context, ?HTTP_PUT, cb_context:req_value(Context, <<"action">>), cb_context:account_id(Context));
validate_notifications(Context, ?HTTP_DELETE) ->
    validate_action(Context, ?HTTP_DELETE, cb_context:req_value(Context, <<"action">>), cb_context:account_id(Context)).

-spec validate_action(cb_context:context(), http_method(), kz_term:api_binary(), kz_term:api_binary()) -> cb_context:context().
validate_action(Context, ?HTTP_PUT, 'undefined', _AccountId) ->
    create(Context);
validate_action(Context, _Method, Action, 'undefined') ->
    Resp = [{<<"message">>, <<"Can not perform mutation action on top-level notification templates">>}],
    cb_context:add_validation_error(Action, <<"forbidden">>, kz_json:from_list(Resp), Context);
validate_action(Context, ?HTTP_PUT, <<"force_system">>, AccountId) ->
    force_system_templates(Context, AccountId);
validate_action(Context, ?HTTP_DELETE, <<"remove_customizations">>, AccountId) ->
    remove_account_customizations(Context, AccountId);
validate_action(Context, _Method, Action, _AccountId) ->
    Resp = [{<<"cause">>, Action}
           ,{<<"message">>, <<"Unknown mutation action">>}
           ],
    cb_context:add_validation_error(Action, <<"forbidden">>, kz_json:from_list(Resp), Context).

-spec validate_notification(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
validate_notification(Context, Id, ?HTTP_GET) ->
    maybe_read(Context, Id);
validate_notification(Context, Id, ?HTTP_POST) ->
    maybe_update(Context, Id);
validate_notification(Context, Id, ?HTTP_DELETE) ->
    validate_delete(Context, Id, cb_context:account_id(Context)).

-spec validate_delete(cb_context:context(), path_token(), kz_term:api_binary()) -> cb_context:context().
validate_delete(Context, Id, 'undefined') ->
    disallow_delete(Context, kz_notification:resp_id(Id));
validate_delete(Context, Id, _AccountId) ->
    lager:debug("trying to remove notification from account ~s", [_AccountId]),
    read(Context, Id, 'account').

-spec disallow_delete(cb_context:context(), path_token()) -> cb_context:context().
disallow_delete(Context, Id) ->
    lager:debug("deleting the system config template is disallowed"),
    Resp =
        [{<<"target">>, Id}
        ,{<<"message">>, <<"Top-level notification template cannot be deleted">>}
        ],
    cb_context:add_validation_error(Id, <<"forbidden">>, kz_json:from_list(Resp), Context).

-spec may_be_validate_recipient_id(cb_context:context()) -> cb_context:context().
may_be_validate_recipient_id(Context) ->
    case cb_context:req_value(Context, <<"recipient_id">>) of
        'undefined' ->
            cb_context:set_resp_status(Context, 'success');
        <<RecipientId:32/binary>> ->
            validate_recipient_id(RecipientId, Context);
        _ ->
            Context
    end.

-spec validate_recipient_id(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_recipient_id(RecipientId, Context) ->
    SenderId = sender_account_id(Context),
    ViewOpts = [{'startkey', [SenderId]}
               ,{'endkey', [SenderId, kz_json:new()]}
               ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts) of
        {'ok', Accounts} ->
            AccountIds = [kz_json:get_value(<<"id">>, Acc) || Acc <- Accounts],
            case lists:member(RecipientId, AccountIds) of
                'true' -> cb_context:set_resp_status(Context, 'success');
                'false' -> Context
            end;
        {'error', _Reason} = E ->
            lager:info("failed to load children. error: ~p", [E]),
            Context
    end.

-spec sender_account_id(cb_context:context()) -> kz_term:ne_binary().
sender_account_id(Context) ->
    sender_account_id(Context, cb_context:account_id(Context)).

-spec sender_account_id(cb_context:context(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
sender_account_id(Context, 'undefined') ->
    cb_context:auth_account_id(Context);
sender_account_id(_Context, AccountId) ->
    AccountId.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    case cb_context:req_value(Context, <<"action">>) of
        'undefined' ->
            Context1 = crossbar_doc:save(set_system_macros(Context)),
            case cb_context:resp_status(Context1) of
                'success' -> leak_doc_id(Context1);
                _Status -> Context1
            end;
        _ -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    case cb_context:req_files(Context) of
        [] ->
            lager:debug("handling POST of template meta for ~s", [Id]),
            do_post(Context);
        [{_FileName, FileJObj}] ->
            lager:debug("POST is for an attachment on ~s(~s)", [Id, kz_notification:db_id(Id)]),
            update_template(Context, Id, FileJObj)
    end.

-spec do_post(cb_context:context()) -> cb_context:context().
do_post(Context) ->
    Context1 = crossbar_doc:save(set_system_macros(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_set_teletype_as_default(Context1),
            leak_doc_id(Context1);
        _Status -> Context1
    end.

-spec set_system_macros(cb_context:context()) -> cb_context:context().
set_system_macros(Context) ->
    Id = kz_doc:id(cb_context:doc(Context)),
    SysContext = read_system(Context, Id),
    case cb_context:resp_status(SysContext) of
        'success' ->
            SysDoc = cb_context:doc(SysContext),
            Macros = kz_json:get_value(?MACROS, SysDoc, kz_json:new()),
            JObj = cb_context:doc(Context),
            cb_context:set_doc(Context, kz_json:set_value(?MACROS, Macros, JObj));
        _Status ->
            SysContext2 = read_system(Context, <<"notification.customer_update">>),
            case cb_context:resp_status(SysContext2) of
                'success' ->
                    lager:debug("template ~s is not exist in `system_config` database. Setting macros from 'customer_update' template", [Id]),
                    SysDoc2 = cb_context:doc(SysContext2),
                    Macros2 = kz_json:get_value(?MACROS, SysDoc2, kz_json:new()),
                    JObj2 = cb_context:doc(Context),
                    cb_context:set_doc(Context, kz_json:set_value(?MACROS, Macros2, JObj2));
                _ ->
                    lager:debug("fail to update macros from system_config"),
                    Context
            end
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?CUSTOMER_UPDATE, ?MESSAGE) ->
    case kz_amqp_worker:call(build_customer_update_payload(Context)
                            ,fun kapi_notifications:publish_customer_update/1
                            ,fun kapi_notifications:notify_update_v/1
                            )
    of
        {'ok', _Resp} ->
            lager:debug("published customer_update notification"),
            Context;
        {'error', _E} ->
            lager:debug("failed to publish_customer update notification: ~p", [_E]),
            crossbar_util:response('error', <<"Failed to send message">>, Context)
    end;

post(Context, <<"customer_update_", _/binary>>, ?PREVIEW) ->
    post(Context, ?CUSTOMER_UPDATE, ?PREVIEW);

post(Context, Id, ?PREVIEW) ->
    Notification = cb_context:doc(Context),
    Preview = build_preview_payload(Context, Notification),
    {API, _} = lists:foldl(fun preview_fold/2
                          ,{Preview, Notification}
                          ,headers(Id)
                          ),
    case kz_amqp_worker:call(maybe_add_extra_data(Id, API)
                            ,publish_fun(Id)
                            ,fun kapi_notifications:notify_update_v/1
                            ,?NOTIFICATION_TIMEOUT
                            )
    of
        {'ok', Resp} ->
            lager:debug("sent API command to preview ~s", [Id]),
            handle_preview_response(Context, Resp);
        {'error', _E} ->
            lager:debug("failed to publish preview for ~s: ~p", [Id, _E]),
            crossbar_util:response('error', <<"Failed to process notification preview">>, Context)
    end.

-spec build_customer_update_payload(cb_context:context()) -> kz_term:proplist().
build_customer_update_payload(Context) ->
    SenderId = sender_account_id(Context),
    props:filter_empty(
      [{<<"Account-ID">>, SenderId}
      ,{<<"Recipient-ID">>, cb_context:req_value(Context, <<"recipient_id">>)}
      ,{<<"User-Type">>, cb_context:req_value(Context, <<"user_type">>)}
      ,{<<"Subject">>, cb_context:req_value(Context, <<"subject">>)}
      ,{<<"From">>, cb_context:req_value(Context, <<"from">>)}
      ,{<<"Reply-To">>, cb_context:req_value(Context, <<"reply_to">>)}
      ,{<<"To">>, cb_context:req_value(Context, <<"to">>)}
      ,{<<"CC">>, cb_context:req_value(Context, <<"cc">>)}
      ,{<<"BCC">>, cb_context:req_value(Context, <<"bcc">>)}
      ,{<<"HTML">>, cb_context:req_value(Context, <<"html">>)}
      ,{<<"Text">>, cb_context:req_value(Context, <<"plain">>)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec build_preview_payload(cb_context:context(), kz_json:object()) -> kz_term:proplist().
build_preview_payload(Context, Notification) ->
    props:filter_empty(
      [{<<"Template-ID">>, kz_json:get_value(<<"id">>, Notification)}
      ,{<<"To">>, kz_json:get_value(<<"to">>, Notification)}
      ,{<<"From">>, kz_json:get_value(<<"from">>, Notification)}
      ,{<<"Cc">>, kz_json:get_value(<<"cc">>, Notification)}
      ,{<<"Bcc">>, kz_json:get_value(<<"bcc">>, Notification)}
      ,{<<"Reply-To">>, kz_json:get_value(<<"reply_to">>, Notification)}
      ,{<<"Subject">>, kz_json:get_value(<<"subject">>, Notification)}
      ,{<<"HTML">>, kz_json:get_value(<<"html">>, Notification)}
      ,{<<"Text">>, kz_json:get_value(<<"plain">>, Notification)}
      ,{<<"Account-ID">>, cb_context:account_id(Context)}
      ,{<<"Account-DB">>, cb_context:db_name(Context)}
      ,{<<"Msg-ID">>, cb_context:req_id(Context)}
      ,{<<"Call-ID">>, cb_context:req_id(Context)}
      ,{<<"Preview">>, 'true'}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec handle_preview_response(cb_context:context(), kz_json:object()) -> cb_context:context().
handle_preview_response(Context, Resp) ->
    case kz_json:get_value(<<"Status">>, Resp) of
        <<"failed">> ->
            lager:debug("failed notification preview: ~p", [Resp]),
            CleansedResp = kz_json:normalize(kz_api:remove_defaults(Resp)),
            crossbar_util:response_invalid_data(CleansedResp, Context);
        _Status ->
            lager:debug("notification preview status :~s", [_Status]),
            crossbar_util:response_202(<<"Notification processing">>, Context)
    end.

-spec headers(kz_term:ne_binary()) -> kz_term:ne_binaries().
headers(<<"voicemail_to_email">>) ->
    kapi_notifications:headers(<<"voicemail_new">>);
headers(<<"port_request_admin">>) ->
    kapi_notifications:headers(<<"port_request">>);
headers(<<"fax_inbound_error_to_email_filtered">>) ->
    kapi_notifications:headers(<<"fax_inbound_error_to_email">>);
headers(<<"fax_outbound_smtp_error_to_email">>) ->
    kapi_notifications:headers(<<"fax_outbound_smtp_error">>);
headers(<<"transaction_failed">>) ->
    kapi_notifications:headers(<<"transaction">>);
headers(Id) ->
    kapi_notifications:headers(Id).

-spec maybe_add_extra_data(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_extra_data(<<"bill_reminder">>, API) ->
    Now = kz_time:now_s(),
    props:set_values([{<<"Items">>, []}
                     ,{<<"Timestamp">>, Now}
                     ,{<<"Due-Date">>, Now}
                     ], API);
maybe_add_extra_data(<<"fax_inbound_error_to_email">>, API) ->
    props:set_value(<<"Fax-Result-Code">>, <<"500">>, API);
maybe_add_extra_data(<<"fax_inbound_error_to_email_filtered">>, API) ->
    props:set_value(<<"Fax-Result-Code">>, <<"49">>, API);
maybe_add_extra_data(<<"fax_outbound_smtp_error_to_email">>, API) ->
    props:set_value(<<"Errors">>, [<<"Not Deliverable">>], API);
maybe_add_extra_data(<<"service_added">>, API) ->
    props:set_value(<<"Items">>, [], API);
maybe_add_extra_data(<<"transaction">>, API) ->
    props:set_value(<<"Success">>, 'true', API);
maybe_add_extra_data(<<"transaction_failed">>, API) ->
    props:set_value(<<"Success">>, 'false', API);
maybe_add_extra_data(<<"port_", _/binary>>, API) ->
    props:set_value(<<"Reason">>, kz_json:new(), API);
maybe_add_extra_data(<<"ported">>, API) ->
    props:set_value(<<"Reason">>, kz_json:new(), API);
maybe_add_extra_data(_Id, API) -> API.

-spec publish_fun(kz_term:ne_binary()) -> fun((kz_term:api_terms()) -> 'ok').
publish_fun(<<"account_zone_change">>) ->
    fun kapi_notifications:publish_account_zone_change/1;
publish_fun(<<"bill_reminder">>) ->
    fun kapi_notifications:publish_bill_reminder/1;
publish_fun(<<"cnam_request">>) ->
    fun kapi_notifications:publish_cnam_request/1;
publish_fun(<<"customer_update">>) ->
    fun kapi_notifications:publish_customer_update/1;
publish_fun(<<"denied_emergency_bridge">>) ->
    fun kapi_notifications:publish_denied_emergency_bridge/1;
publish_fun(<<"deregister">>) ->
    fun kapi_notifications:publish_deregister/1;
publish_fun(<<"fax_inbound_error_to_email">>) ->
    fun kapi_notifications:publish_fax_inbound_error/1;
publish_fun(<<"fax_inbound_error_to_email_filtered">>) ->
    fun kapi_notifications:publish_fax_inbound_error/1;
publish_fun(<<"fax_inbound_to_email">>) ->
    fun kapi_notifications:publish_fax_inbound/1;
publish_fun(<<"fax_outbound_error_to_email">>) ->
    fun kapi_notifications:publish_fax_outbound_error/1;
publish_fun(<<"fax_outbound_smtp_error_to_email">>) ->
    fun kapi_notifications:publish_fax_outbound_smtp_error/1;
publish_fun(<<"fax_outbound_to_email">>) ->
    fun kapi_notifications:publish_fax_outbound/1;
publish_fun(<<"first_occurrence">>) ->
    fun kapi_notifications:publish_first_occurrence/1;
publish_fun(<<"low_balance">>) ->
    fun kapi_notifications:publish_low_balance/1;
publish_fun(<<"missed_call">>) ->
    fun kapi_notifications:publish_missed_call/1;
publish_fun(<<"new_account">>) ->
    fun kapi_notifications:publish_new_account/1;
publish_fun(<<"new_user">>) ->
    fun kapi_notifications:publish_new_user/1;
publish_fun(<<"password_recovery">>) ->
    fun kapi_notifications:publish_password_recovery/1;
publish_fun(<<"port_cancel">>) ->
    fun kapi_notifications:publish_port_cancel/1;
publish_fun(<<"port_comment">>) ->
    fun kapi_notifications:publish_port_comment/1;
publish_fun(<<"port_pending">>) ->
    fun kapi_notifications:publish_port_pending/1;
publish_fun(<<"port_request">>) ->
    fun kapi_notifications:publish_port_request/1;
publish_fun(<<"port_rejected">>) ->
    fun kapi_notifications:publish_port_rejected/1;
publish_fun(<<"port_request_admin">>) ->
    fun kapi_notifications:publish_port_request/1;
publish_fun(<<"port_scheduled">>) ->
    fun kapi_notifications:publish_port_scheduled/1;
publish_fun(<<"port_unconfirmed">>) ->
    fun kapi_notifications:publish_port_unconfirmed/1;
publish_fun(<<"ported">>) ->
    fun kapi_notifications:publish_ported/1;
publish_fun(<<"service_added">>) ->
    fun kapi_notifications:publish_service_added/1;
publish_fun(<<"system_alert">>) ->
    fun kapi_notifications:publish_system_alert/1;
publish_fun(<<"topup">>) ->
    fun kapi_notifications:publish_topup/1;
publish_fun(<<"transaction">>) ->
    fun kapi_notifications:publish_transaction/1;
publish_fun(<<"transaction_failed">>) ->
    fun kapi_notifications:publish_transaction/1;
publish_fun(<<"voicemail_full">>) ->
    fun kapi_notifications:publish_voicemail_full/1;
publish_fun(<<"voicemail_to_email">>) ->
    fun kapi_notifications:publish_voicemail_new/1;
publish_fun(<<"webhook_disabled">>) ->
    fun kapi_notifications:publish_webhook_disabled/1;
publish_fun(_Id) ->
    lager:debug("no kapi_notifications:publish_~s/1 defined", [_Id]),
    fun(_Any) -> 'ok' end.

-spec preview_fold(kz_term:ne_binary(), {kz_term:proplist(), kz_json:object()}) ->
          {kz_term:proplist(), kz_json:object()}.
preview_fold(Header, {Props, ReqData}) ->
    case kz_json:get_first_defined([Header, kz_json:normalize_key(Header)], ReqData) of
        'undefined' ->
            {props:insert_value(Header, Header, Props), ReqData};
        V ->
            {props:set_value(Header, V, Props), ReqData}
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    ContentTypes = media_values(cb_context:req_header(Context, <<"content-type">>)),
    maybe_delete(Context, Id, ContentTypes).

-spec maybe_delete(cb_context:context(), path_token(), media_values()) ->
          cb_context:context().
maybe_delete(Context, Id, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)]) ->
    delete_doc(Context, Id);
maybe_delete(Context, Id, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)]) ->
    delete_doc(Context, Id);
maybe_delete(Context, Id, [?MEDIA_VALUE(Type, SubType, _, _, _)]) ->
    maybe_delete_template(Context, Id, <<Type/binary, "/", SubType/binary>>).

-spec delete_doc(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
delete_doc(Context, Id) ->
    Context1 = crossbar_doc:delete(Context, ?HARD_DELETE),
    case cb_context:resp_status(Context1) of
        'success' ->
            kz_datamgr:flush_cache_doc(cb_context:db_name(Context), Id),
            leak_doc_id(Context1);
        _Status -> Context1
    end.

-spec maybe_delete_template(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          cb_context:context().
maybe_delete_template(Context, Id, ContentType) ->
    maybe_delete_template(Context, Id, ContentType, cb_context:doc(Context)).

-spec maybe_delete_template(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
maybe_delete_template(Context, Id, ContentType, TemplateJObj) ->
    AttachmentName = attachment_name_by_media_type(ContentType),
    case kz_doc:attachment(TemplateJObj, AttachmentName) of
        'undefined' ->
            lager:debug("failed to find attachment ~s", [AttachmentName]),
            JObj = kz_json:from_list([{<<"cause">>, ContentType}]),
            cb_context:add_system_error('bad_identifier', JObj, Context);
        _Attachment ->
            lager:debug("attempting to delete attachment ~s", [AttachmentName]),
            crossbar_doc:delete_attachment(kz_notification:db_id(Id), AttachmentName, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec accept_values(cb_context:context()) -> media_values().
accept_values(Context) ->
    AcceptValue = cb_context:req_header(Context, <<"accept">>),
    Tunneled = cb_context:req_value(Context, <<"accept">>),
    media_values(AcceptValue, Tunneled).

-spec media_values(kz_term:api_binary()) -> media_values().
media_values(Media) ->
    media_values(Media, 'undefined').

-spec media_values(kz_term:api_binary(), kz_term:api_binary()) -> media_values().
media_values('undefined', 'undefined') ->
    lager:debug("no accept headers, assuming JSON"),
    [?MEDIA_VALUE(<<"application">>, <<"json">>)];
media_values(AcceptValue, 'undefined') ->
    case cb_modules_util:parse_media_type(AcceptValue) of
        {'error', 'badarg'} -> media_values('undefined', 'undefined');
        AcceptValues -> lists:reverse(lists:keysort(2, AcceptValues))
    end;
media_values(AcceptValue, Tunneled) ->
    case cb_modules_util:parse_media_type(Tunneled) of
        {'error', 'badarg'} -> media_values(AcceptValue, 'undefined');
        TunneledValues ->
            lager:debug("using tunneled accept value ~s", [Tunneled]),
            lists:reverse(lists:keysort(2, TunneledValues))
    end.

-spec acceptable_content_types(cb_context:context()) -> kz_term:proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec maybe_read(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_read(Context, Id) ->
    Acceptable = acceptable_content_types(Context),
    maybe_read(Context, Id, Acceptable, accept_values(Context)).

-spec maybe_read(cb_context:context(), kz_term:ne_binary(), kz_term:proplist(), media_values()) -> cb_context:context().
maybe_read(Context, Id, _Acceptable, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)|_Accepts]) ->
    read(Context, Id);
maybe_read(Context, Id, _Acceptable, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)|_Accepts]) ->
    read(Context, Id);
maybe_read(Context, Id, _Acceptable, [?MEDIA_VALUE(<<"*">>, <<"*">>, _, _, _)|_Accepts]) ->
    lager:debug("catch-all accept header, using json"),
    read(Context, Id);
maybe_read(Context, Id, Acceptable, [?MEDIA_VALUE(Type, SubType, _, _, _)|Accepts]) ->
    case is_acceptable_accept(Acceptable, Type, SubType) of
        'false' ->
            lager:debug("unknown accept header: ~s/~s", [Type, SubType]),
            maybe_read(Context, Id, Acceptable, Accepts);
        'true' ->
            lager:debug("accept header: ~s/~s", [Type, SubType]),
            maybe_read_template(read(Context, Id), Id, <<Type/binary, "/", SubType/binary>>)
    end;
maybe_read(Context, Id, _Acceptable, []) ->
    lager:debug("no accept headers, using json"),
    read(Context, Id).

-spec is_acceptable_accept(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:member({Type,SubType}, Acceptable).

-type load_from() :: 'system' | 'account' | 'system_migrate'.

-spec read(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
read(Context, Id) ->
    read(Context, Id, 'system').

-spec read(cb_context:context(), kz_term:ne_binary(), load_from()) -> cb_context:context().
read(Context, Id, LoadFrom) ->
    Context1 =
        case cb_context:db_name(Context) of
            ?KZ_CONFIG_DB when LoadFrom =:= 'system'; LoadFrom =:= 'system_migrate' ->
                lager:debug("loading ~s from system config", [Id]),
                read_system(Context, Id);
            _AccountDb ->
                lager:debug("reading ~s from account ~s first", [Id, _AccountDb]),
                read_account(Context, Id, LoadFrom)
        end,
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("successful read of ~s", [Id]),
            read_success(Context1);
        _Status -> Context1
    end.

-spec read_system(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
read_system(Context, Id) ->
    Context1 = crossbar_doc:load(Id
                                ,cb_context:set_db_name(Context, ?KZ_CONFIG_DB)
                                ,?TYPE_CHECK_OPTION(kz_notification:pvt_type())
                                ),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:store(Context1, 'attachments_db', ?KZ_CONFIG_DB);
        _Status -> Context1
    end.

-spec read_account(cb_context:context(), kz_term:ne_binary(), load_from()) -> cb_context:context().
read_account(Context, Id, LoadFrom) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kz_notification:pvt_type())),
    case {cb_context:resp_error_code(Context1)
         ,cb_context:resp_status(Context1)
         }
    of
        {404, 'error'} when LoadFrom =:= 'system'; LoadFrom =:= 'system_migrate' ->
            maybe_read_from_parent(Context, Id, LoadFrom, cb_context:reseller_id(Context));
        {_Code, 'success'} ->
            case system_config_notification_doc(Id) of
                {'ok', _JObj} -> prepare_account_responce(Context1, Id, 'set_account_overridden');
                {'error', 'not_found'} -> prepare_account_responce(Context1, Id, 'set_account_defined');
                {'error', _E} ->
                    lager:debug("error fetching ~s from system config: ~p", [Id, _E]),
                    crossbar_util:response_db_fatal(Context1)
            end;
        {_Code, _Status} ->
            lager:debug("failed to load ~s: ~p", [Id, _Code]),
            Context1
    end.

-spec prepare_account_responce(cb_context:context(), kz_term:ne_binary(), atom()) -> cb_context:context().
prepare_account_responce(Context, Id, 'set_account_overridden') ->
    lager:debug("loaded system notification ~s from account database ~s", [Id, cb_context:db_name(Context)]),
    Context1 = maybe_merge_ancestor_attachments(Context, Id),
    NewRespData = note_account_override(cb_context:resp_data(Context1)),
    cb_context:set_resp_data(Context1, NewRespData);
prepare_account_responce(Context, Id, 'set_account_defined') ->
    lager:debug("loaded account defined notification ~s from database ~s", [Id, cb_context:db_name(Context)]),
    Context1 = maybe_merge_ancestor_attachments(Context, Id),
    NewRespData = note_account_defined(cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context1, NewRespData).

-spec maybe_read_from_parent(cb_context:context(), kz_term:ne_binary(), load_from(), kz_term:api_binary()) -> cb_context:context().
maybe_read_from_parent(Context, Id, LoadFrom, 'undefined') ->
    lager:debug("~s not found in account and reseller is undefined, reading from system_config", [Id]),
    read_system_for_account(Context, Id, LoadFrom);
maybe_read_from_parent(Context, Id, LoadFrom, ResellerId) ->
    AccountId = kzs_util:format_account_id(cb_context:db_name(Context)),
    case AccountId =/= ResellerId
        andalso get_parent_account_id(AccountId) of
        'false' ->
            lager:debug("~s not found in account and reached to reseller, reading from system_config", [Id]),
            read_system_for_account(Context, Id, LoadFrom);
        'undefined' ->
            lager:debug("~s not found in account and parent is undefined, reading from system_config", [Id]),
            read_system_for_account(Context, Id, LoadFrom);
        ParentId ->
            ParentDb = kzs_util:format_account_db(ParentId),
            lager:debug("account doesn't have ~s, reading from parent account ~s", [Id, ParentDb]),
            read_account(cb_context:set_db_name(Context, ParentDb), Id, LoadFrom)
    end.

-spec read_system_for_account(cb_context:context(), path_token(), load_from()) ->
          cb_context:context().
read_system_for_account(Context, Id, LoadFrom) ->
    Context1 = read_system(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' when LoadFrom =:= 'system' ->
            lager:debug("read template ~s from system_config", [Id]),
            revert_context_to_account(Context, Context1);
        'success' when LoadFrom =:= 'system_migrate' ->
            lager:debug("read template ~s from system_config account, now migrating it", [Id]),
            migrate_template_to_account(revert_context_to_account(Context, Context1), Id);
        _Status ->
            lager:debug("failed to read system_config db for ~s", [Id]),
            revert_context_to_account(Context, Context1)
    end.

-spec get_parent_account_id(kz_term:ne_binary()) -> kz_term:api_binary().
get_parent_account_id(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} -> kzd_accounts:parent_account_id(JObj);
        {'error', _E} ->
            lager:error("failed to find parent account for ~s", [AccountId]),
            'undefined'
    end.

-spec revert_context_to_account(cb_context:context(), cb_context:context()) -> cb_context:context().
revert_context_to_account(AccountContext, SystemContext) ->
    AccountDb = cb_context:db_name(AccountContext),
    AccountId = cb_context:account_id(AccountContext),
    SystemContext1 = cb_context:setters(
                       SystemContext
                      ,[{fun cb_context:set_db_name/2, AccountDb}
                       ,{fun cb_context:set_account_id/2, AccountId}
                       ]
                      ),
    SystemDoc = kz_doc:delete_revision(cb_context:doc(SystemContext)),
    SystemDoc1 = kz_doc:setters(SystemDoc, [{fun kz_doc:set_account_db/2, AccountDb}
                                           ,{fun kz_doc:set_account_id/2, AccountId}
                                           ]),
    cb_context:set_doc(SystemContext1, SystemDoc1).

-spec migrate_template_to_account(cb_context:context(), path_token()) -> cb_context:context().
migrate_template_to_account(Context, Id) ->
    lager:debug("saving template ~s from system config to account ~s", [Id, cb_context:account_id(Context)]),

    Template = cb_context:doc(Context),
    %% Remove attachments when saving the template doc to the account
    Context1 = cb_context:set_doc(Context, kz_doc:delete_attachments(Template)),

    Context2 = crossbar_doc:save(Context1),
    case cb_context:resp_status(Context2) of
        'success' ->
            lager:debug("saved template ~s to account ~s", [Id, cb_context:db_name(Context2)]),
            Context3 = migrate_template_attachments(Context2, Id, kz_doc:attachments(Template)),
            maybe_set_teletype_as_default(Context3),
            Context3;
        _Status -> Context2
    end.

-spec maybe_hard_delete(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
maybe_hard_delete(Context, Id) ->
    case kz_datamgr:del_doc(cb_context:db_name(Context), Id) of
        {'ok', _} ->
            kz_datamgr:flush_cache_doc(cb_context:db_name(Context), Id),
            lager:debug("hard-deleted old version of ~s from ~s", [Id, cb_context:db_name(Context)]);
        {'error', 'not_found'} ->
            kz_datamgr:flush_cache_doc(cb_context:db_name(Context), Id),
            lager:debug("~s wasn't found in ~s", [Id, cb_context:db_name(Context)]);
        {'error', _E} ->
            lager:debug("error deleting ~s from ~s: ~p", [Id, cb_context:db_name(Context), _E])
    end.

-spec maybe_merge_ancestor_attachments(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
maybe_merge_ancestor_attachments(Context, Id) ->
    Doc = cb_context:doc(Context),
    AttachmentsDb = kz_doc:account_db(Doc),
    case kz_doc:attachments(Doc) of
        'undefined' -> merge_ancestor_attachments(Context, Id);
        _ -> cb_context:store(Context, 'attachments_db', AttachmentsDb)
    end.

-spec merge_ancestor_attachments(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
merge_ancestor_attachments(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = cb_context:reseller_id(Context),
    merge_ancestor_attachments(Context, Id, AccountId, ResellerId).

%% Last attempt was reseller, now try ?KZ_CONFIG_DB

-spec merge_ancestor_attachments(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          cb_context:context().
merge_ancestor_attachments(Context, Id, AccountId, AccountId) ->
    lager:debug("trying attachments in ~s", [?KZ_CONFIG_DB]),
    case kz_doc:attachments(cb_context:doc(read_system(Context, Id))) of
        'undefined' ->
            lager:debug("the ~s ~s template is missing", [?KZ_CONFIG_DB, Id]),
            Context;
        Attachments ->
            lager:debug("found attachments in ~s", [?KZ_CONFIG_DB]),
            Doc = kz_json:set_value(<<"_attachments">>, Attachments, cb_context:doc(Context)),
            cb_context:setters(Context
                              ,[{fun cb_context:store/3, 'db_doc', Doc}
                               ,{fun cb_context:set_doc/2, Doc}
                               ,{fun cb_context:store/3, 'attachments_db', ?KZ_CONFIG_DB}])
    end;
%% Not yet at reseller, try parent account
merge_ancestor_attachments(Context, Id, AccountId, ResellerId) ->
    case get_parent_account_id(AccountId) of
        'undefined' -> Context;
        ParentAccountId ->
            lager:debug("trying attachments in account ~s", [ParentAccountId]),
            try_parent_attachments(Context, Id, ParentAccountId, ResellerId)
    end.

-spec try_parent_attachments(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          cb_context:context().
try_parent_attachments(Context, Id, ParentAccountId, ResellerId) ->
    ParentNotificationContext = crossbar_doc:load(Id
                                                 ,masquerade(Context, ParentAccountId)
                                                 ,?TYPE_CHECK_OPTION(kz_notification:pvt_type())),
    ParentDoc = cb_context:doc(ParentNotificationContext),
    case kz_doc:attachments(ParentDoc) of
        'undefined' -> merge_ancestor_attachments(Context, Id, ParentAccountId, ResellerId);
        Attachments ->
            lager:debug("found attachments in account ~s", [ParentAccountId]),
            Doc = kz_json:set_value(<<"_attachments">>, Attachments, cb_context:doc(Context)),
            AttachmentsDb = kzs_util:format_account_db(ParentAccountId),
            cb_context:setters(Context
                              ,[{fun cb_context:store/3, 'db_doc', Doc}
                               ,{fun cb_context:set_doc/2, Doc}
                               ,{fun cb_context:store/3, 'attachments_db', AttachmentsDb}])
    end.

-spec masquerade(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
masquerade(Context, AccountId) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    cb_context:setters(Context
                      ,[{fun cb_context:set_account_id/2, AccountId}
                       ,{fun cb_context:set_db_name/2, AccountDb}
                       ]).

-spec maybe_set_teletype_as_default(cb_context:context()) -> 'ok'.
maybe_set_teletype_as_default(Context) ->
    AccountId = cb_context:account_id(Context),
    case kzd_accounts:fetch(AccountId) of
        {'error', _E} -> lager:debug("failed to note preference: ~p", [_E]);
        {'ok', AccountDoc} ->
            maybe_set_teletype_as_default(Context, AccountDoc)
    end.

-spec maybe_set_teletype_as_default(cb_context:context(), kzd_accounts:doc()) -> 'ok'.
maybe_set_teletype_as_default(Context, AccountDoc) ->
    case kzd_accounts:notification_preference(AccountDoc) of
        'undefined' -> set_teletype_as_default(Context, AccountDoc);
        <<"teletype">> -> lager:debug("account already prefers teletype");
        _Pref -> set_teletype_as_default(Context, AccountDoc)
    end.

-spec set_teletype_as_default(cb_context:context(), kzd_accounts:doc()) -> 'ok'.
set_teletype_as_default(Context, AccountDoc) ->
    Updates = [{kzd_accounts:path_notification_preference(), <<"teletype">>}
               | crossbar_doc:pvt_updates(AccountDoc, Context)
              ],

    case kzd_accounts:update(cb_context:account_id(Context), Updates) of
        {'ok', _UpdatedAccountJObj} ->
            lager:debug("updated pref for account");
        {'error', _E} ->
            lager:debug("failed to note preference: ~p", [_E])
    end.

-spec migrate_template_attachments(cb_context:context(), kz_term:ne_binary(), kz_term:api_object()) -> cb_context:context().
migrate_template_attachments(Context, _Id, 'undefined') ->
    lager:debug("no attachments to migrate for ~s", [_Id]),
    Context;
migrate_template_attachments(Context, Id, Attachments) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    kz_json:foldl(fun(AName, AMeta, C) ->
                          lager:debug("migrate attachment ~s", [AName]),
                          migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, C)
                  end, Context, Attachments).

-spec migrate_template_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), cb_context:context()) ->
          cb_context:context().
migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, Context) ->
    case kz_datamgr:fetch_attachment(MasterAccountDb, Id, AName) of
        {'ok', Bin} ->
            ContentType = kz_json:get_value(<<"content_type">>, AMeta),
            lager:debug("saving attachment for ~s(~s): ~s", [Id, AName, ContentType]),
            Opts = [{'content_type', kz_term:to_list(ContentType)}],
            AttName = attachment_name_by_content_type(ContentType),
            crossbar_doc:save_attachment(Id, AttName, Bin, Context, Opts);
        {'error', _E} ->
            lager:debug("failed to load attachment ~s for ~s: ~p", [AName, Id, _E]),
            Context
    end.

-spec note_account_override(kz_json:object()) -> kz_json:object().
note_account_override(JObj) ->
    kz_json:set_value(<<"account_overridden">>, 'true', JObj).

-spec note_account_defined(kz_json:object()) -> kz_json:object().
note_account_defined(JObj) ->
    kz_json:set_value(<<"account_defined">>, 'true', JObj).

-spec read_success(cb_context:context()) -> cb_context:context().
read_success(Context) ->
    cb_context:setters(Context, [fun leak_attachments/1
                                ,fun leak_doc_id/1
                                ]).

-spec maybe_read_template(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
maybe_read_template(Context, _Id, <<"application/json">>) -> Context;
maybe_read_template(Context, _Id, <<"application/x-json">>) -> Context;
maybe_read_template(Context, Id, Accept) ->
    case cb_context:resp_status(Context) of
        'success' -> read_template(Context, Id, Accept);
        _Status -> Context
    end.

-spec read_template(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
read_template(Context, Id, Accept) ->
    Doc = cb_context:fetch(Context, 'db_doc'),
    AttachmentName = attachment_name_by_media_type(Accept),
    case {kz_doc:attachment(Doc, AttachmentName)
         ,cb_context:fetch(Context, 'attachments_db')
         }
    of
        {'undefined', _} ->
            lager:debug("failed to find attachment ~s in ~s", [AttachmentName, Id]),
            crossbar_util:response_faulty_request(Context);
        {_Attachment, 'undefined'} ->
            lager:debug("failed to load attachment, attachments_db for ~s in ~s is not set", [AttachmentName, Id]),
            crossbar_util:response_faulty_request(Context);
        {_Attachment, AttachmentsDb} ->
            lager:debug("found attachment ~s in document ~s at ~s", [AttachmentName, Id, AttachmentsDb]),
            LoadedContext = read_account_attachment(Context, AttachmentsDb, Id, AttachmentName),
            cb_context:add_resp_headers(LoadedContext
                                       ,#{<<"content-disposition">> => attachment_filename(Id, Accept)
                                         ,<<"content-type">> => kz_doc:attachment_content_type(Doc, AttachmentName)
                                         }
                                       )
    end.

-spec attachment_filename(kz_term:ne_binary(), kz_term:ne_binary()) -> iolist().
attachment_filename(Id, Accept) ->
    [<<"attachment; filename=">>
    ,kz_notification:resp_id(Id)
    ,$., kz_mime:to_extension(Accept)
    ].

-spec read_account_attachment(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
read_account_attachment(Context, AttachmentsDb, DocId, Name) ->
    Context1 = crossbar_doc:load_attachment(DocId
                                           ,Name
                                           ,?TYPE_CHECK_OPTION(kz_notification:pvt_type())
                                           ,cb_context:set_db_name(Context, AttachmentsDb)
                                           ),
    case {cb_context:resp_error_code(Context1)
         ,cb_context:resp_status(Context1)
         }
    of
        {404, 'error'} ->
            lager:debug("template ~s attachment ~s not found in ~s", [DocId, Name, AttachmentsDb]),
            Context1;
        {_Code, 'success'} ->
            lager:debug("loaded ~s from account database", [DocId]),
            Context1;
        {_Code, _Status} ->
            lager:debug("failed to load ~s: ~p", [DocId, _Code]),
            Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_update(Context, Id) ->
    case cb_context:req_files(Context) of
        [] ->
            lager:debug("updating template meta for ~s", [Id]),
            update_notification(Context, Id);
        [{_FileName, FileJObj}] ->
            lager:debug("recv template upload of ~s: ~p", [_FileName, FileJObj]),
            read(Context, Id, 'system_migrate')
    end.

-spec update_notification(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
update_notification(Context, Id) ->
    Context1 = read(Context, Id),
    IsPreview = is_preview(cb_context:req_nouns(Context)),
    case {cb_context:resp_error_code(Context1)
         ,cb_context:resp_status(Context1)
         }
    of
        {_, 'success'} ->
            Context2 = maybe_inherit_defaults(Context, cb_context:doc(Context1)),
            OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
            cb_context:validate_request_data(<<"notifications">>, Context2, OnSuccess);
        {404, 'error'} when IsPreview ->
            OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
            cb_context:validate_request_data(<<"notifications">>, Context1, OnSuccess);
        {_Code, _Status} -> Context1
    end.

-spec maybe_inherit_defaults(cb_context:context(), kz_term:api_object()) ->
          cb_context:context().
maybe_inherit_defaults(Context, Doc) ->
    case ?INHERIT_DEFAULT_VALUES of
        'true' -> inherit_defaults(Context, Doc);
        'false' -> Context
    end.

-spec inherit_defaults(cb_context:context(), kz_term:api_object()) -> cb_context:context().
inherit_defaults(Context, 'undefined') -> Context;
inherit_defaults(Context, InheritedDefaultsDoc) ->
    PublicDefaults = kz_doc:public_fields(InheritedDefaultsDoc),
    ReqData = kz_json:merge(PublicDefaults, cb_context:req_data(Context)),
    cb_context:set_req_data(Context, ReqData).

-spec update_template(cb_context:context(), path_token(), kz_json:object()) ->
          cb_context:context().
update_template(Context, Id, FileJObj) ->
    DbId = kz_notification:db_id(Id),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    lager:debug("file content type for ~s: ~s", [DbId, CT]),

    Opts = [{'content_type', kz_term:to_list(CT)}],

    case kz_template:compile(Contents, template_module_name(Id, Context, CT)) of
        {'ok', _} ->
            AttachmentName = attachment_name_by_content_type(CT),
            crossbar_doc:save_attachment(DbId
                                        ,AttachmentName
                                        ,Contents
                                        ,Context
                                        ,Opts
                                        );
        {'error', Error} ->
            lager:warning("failed to compile uploaded ~s template: ~p", [CT, Error]),
            crossbar_util:response('error', <<"Failed to compile notification template">>, Context)
    end.

-spec attachment_name_by_content_type(kz_term:ne_binary()) -> kz_term:ne_binary().
attachment_name_by_content_type(CT) ->
    kz_binary:clean(<<"template.", CT/binary>>).

-spec attachment_name_by_media_type(kz_term:ne_binary()) -> kz_term:ne_binary().
attachment_name_by_media_type(CT) ->
    <<"template.", CT/binary>>.

-spec template_module_name(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) -> atom().
template_module_name(Id, Context, CT) ->
    AccountId = cb_context:db_name(Context),
    [_C, Type] = binary:split(CT, <<"/">>),
    ModuleName = list_to_binary([AccountId, "_", Id, "_", Type]),
    kz_term:to_atom(ModuleName, 'true').

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:db_name(Context) of
        'undefined' -> summary_available(Context);
        _AccountDb -> summary_account(Context)
    end.

-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    case fetch_available() of
        {'ok', Available} -> crossbar_doc:handle_json_success(Available, Context);
        {'error', 'not_found'} -> fetch_summary_available(Context)
    end.

-spec fetch_summary_available(cb_context:context()) -> cb_context:context().
fetch_summary_available(Context) ->
    ViewOptions = [{'databases', [?KZ_CONFIG_DB]}
                  ,{'mapper', select_normalize_fun(Context)}
                  ,{'should_paginate', 'false'}
                  ,{'unchunkable', 'true'}
                  ],
    Context1 = crossbar_view:load(Context, ?CB_LIST, ViewOptions),
    case cb_context:resp_status(Context1) of
        'success' ->
            cache_available(Context1),
            Context1;
        _ ->
            Context1
    end.

-spec cache_available(cb_context:context()) -> 'ok'.
cache_available(Context) ->
    kz_cache:store_local(?CACHE_NAME
                        ,{?MODULE, 'available'}
                        ,cb_context:doc(Context)
                        ,[{'origin', [{'db', cb_context:db_name(Context), kz_notification:pvt_type()}]}]
                        ).

-spec flush() -> non_neg_integer().
flush() ->
    kz_cache:filter_erase_local(?CACHE_NAME, fun is_cache_key/2).

-spec is_cache_key(any(), any()) -> boolean().
is_cache_key({?MODULE, 'available'}, _) -> 'true';
is_cache_key(_K, _V) -> 'false'.

-spec fetch_available() -> {'ok', kz_json:objects()} |
          {'error', 'not_found'}.
fetch_available() ->
    kz_cache:fetch_local(?CACHE_NAME, {?MODULE, 'available'}).

-spec summary_account(cb_context:context()) -> cb_context:context().
summary_account(Context) ->
    Context1 =
        crossbar_doc:load_view(?CB_LIST
                              ,[]
                              ,Context
                              ,select_normalize_fun(Context)
                              ),
    lager:debug("loaded account's summary"),
    summary_account(Context1, cb_context:doc(Context1)).

-spec summary_account(cb_context:context(), kz_json:objects()) -> cb_context:context().
summary_account(Context, AccountAvailable) ->
    Context1 = summary_available(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Available = filter_available(Context1),
            lager:debug("loaded system available"),
            JObj = merge_available(AccountAvailable, Available),
            crossbar_doc:handle_json_success(JObj, Context);
        _ ->
            Context1
    end.

-spec filter_available(cb_context:context()) -> kz_json:objects().
filter_available(Context) ->
    [A || A <- cb_context:doc(Context),
          crossbar_filter:by_doc(kz_json:from_list([{<<"doc">>, A}]), Context, 'true')
    ].

-spec merge_available(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
merge_available([], Available) ->
    lager:debug("account has not overridden any, using system notifications"),
    Available;
merge_available(AccountAvailable, Available) ->
    lists:foldl(fun merge_fold/2, Available, AccountAvailable).

-spec merge_fold(kz_json:object(), kz_json:objects()) -> kz_json:objects().
merge_fold(Overridden, Acc) ->
    Id = kz_doc:id(Overridden),
    case lists:partition(fun(JObj) -> kz_doc:id(JObj) =:= Id end, Acc) of
        {[Master], Filtered} ->
            Values = [{<<"friendly_name">>, kz_json:get_value(<<"friendly_name">>, Master)}
                     ,{<<"macros">>, kz_json:get_value(<<"macros">>, Master)}
                     ],
            JObj = kz_json:set_values(Values, Overridden),
            lager:debug("noting ~s is overridden in account", [Id]),
            [note_account_override(JObj) | Filtered];
        {[], Filtered} ->
            lager:debug("notification ~s exists on the account, but doesn't exist on the system", [Id]),
            [note_account_defined(Overridden) | Filtered]
    end.

-type normalize_fun() :: fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-spec select_normalize_fun(cb_context:context()) -> normalize_fun().
select_normalize_fun(Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> fun normalize_available_admin/2;
        'false' -> fun(JObj, Acc) -> normalize_available_non_admin(JObj, Acc, Context) end
    end.

-spec normalize_available_admin(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_available_admin(JObj, Acc) ->
    Value = kz_json:get_value(<<"value">>, JObj),
    case kz_notification:category(Value) of
        <<"skel">> -> Acc;
        _Category -> [Value | Acc]
    end.

-spec normalize_available_non_admin(kz_json:object(), kz_json:objects(), cb_context:context()) ->
          kz_json:objects().
normalize_available_non_admin(JObj, Acc, Context) ->
    Value = kz_json:get_value(<<"value">>, JObj),
    case kz_notification:category(Value) of
        <<"system">> -> Acc;
        <<"skel">> -> Acc;
        <<"port">> -> normalize_available_port(Value, Acc, Context);
        _Category -> [Value | Acc]
    end.

-spec normalize_available_port(kz_json:object(), kz_json:objects(), cb_context:context()) ->
          kz_json:objects().
normalize_available_port(Value, Acc, Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),

    case kz_services_reseller:is_reseller(AuthAccountId)
        andalso kzd_port_requests:find_port_authority(AccountId)
    of
        'false' -> Acc;

        'undefined' -> [Value | Acc];
        AuthAccountId -> [Value | Acc];

        _OtherAccountId ->
            lager:debug("another account ~s manages port requests for ~s, not ~s"
                       ,[_OtherAccountId, AccountId, AuthAccountId]
                       ),
            Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec system_config_notification_doc(kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
system_config_notification_doc(DocId) ->
    kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, DocId).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    ReqTemplate = clean_req_doc(cb_context:doc(Context)),

    DocId = kz_notification:db_id(ReqTemplate),

    case system_config_notification_doc(DocId) of
        {'ok', _JObj} ->
            Doc = kz_notification:set_base_properties(ReqTemplate, DocId),
            cb_context:set_doc(Context, Doc);
        {'error', 'not_found'} ->
            handle_missing_system_config_notification(Context, DocId, ReqTemplate);
        {'error', _E} ->
            lager:debug("error fetching ~s from system config: ~p", [DocId, _E]),
            crossbar_util:response_db_fatal(Context)
    end;
on_successful_validation(Id, Context) ->
    ReqTemplate = clean_req_doc(cb_context:doc(Context)),

    CleanedContext = cb_context:set_doc(Context, ReqTemplate),

    Context1 = crossbar_doc:load_merge(Id, CleanedContext, ?TYPE_CHECK_OPTION(kz_notification:pvt_type())),

    case {cb_context:resp_status(Context1)
         ,cb_context:resp_error_code(Context1)
         }
    of
        {'error', 404} ->
            lager:debug("load/merge of ~s failed with a 404", [Id]),
            handle_missing_account_notification(CleanedContext, Id, is_preview(cb_context:req_nouns(CleanedContext)));
        {'success', _} -> Context1;
        {_Status, _Code} ->
            lager:debug("load/merge of ~s failed with ~p / ~p", [Id, _Status, _Code]),
            Context1
    end.

-spec is_preview(req_nouns()) -> boolean().
is_preview([{<<"notifications">>, [_Id, ?PREVIEW]}|_]) -> 'true';
is_preview(_) -> 'false'.

-spec handle_missing_account_notification(cb_context:context(), kz_term:ne_binary(), boolean()) ->
          cb_context:context().
handle_missing_account_notification(Context, Id, 'true') ->
    lager:debug("preview request, ignoring if notification ~s is missing", [Id]),
    Context;
handle_missing_account_notification(Context, Id, 'false') ->
    _ = maybe_hard_delete(Context, Id),
    Context1 = read_system_for_account(Context, Id, 'system_migrate'),
    case cb_context:resp_status(Context1) of
        'success' -> on_successful_validation(Id, Context);
        _Status -> Context1
    end.

-spec handle_missing_system_config_notification(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
handle_missing_system_config_notification(Context, DocId, ReqTemplate) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("creating system config notification for ~s", [DocId]),
            create_new_notification(Context, ?KZ_CONFIG_DB, ?KZ_CONFIG_DB ,DocId, ReqTemplate);
        AccountId ->
            lager:debug("doc ~s does not exist in the system config, creating account ~s defined notification"
                       ,[DocId, AccountId]
                       ),
            AccountDb = cb_context:db_name(Context),
            create_new_notification(Context, AccountDb, AccountId ,DocId, ReqTemplate)
    end.

-spec create_new_notification(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
create_new_notification(Context, AccountDb, AccountId,  DocId, ReqTemplate) ->
    Doc = kz_notification:set_base_properties(ReqTemplate, DocId),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, Doc}
                       ,{fun cb_context:set_db_name/2, AccountDb}
                       ,{fun cb_context:set_account_id/2, AccountId}
                       ]).

-spec clean_req_doc(kz_json:object()) -> kz_json:object().
clean_req_doc(Doc) ->
    kz_json:delete_keys([?MACROS
                        ,<<"templates">>
                        ], Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec leak_doc_id(cb_context:context()) -> cb_context:context().
leak_doc_id(Context) ->
    RespData = cb_context:resp_data(Context),
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"id">>
                                              ,kz_notification:resp_id(RespData)
                                              ,RespData
                                              )
                            ).

-spec leak_attachments(cb_context:context()) -> cb_context:context().
leak_attachments(Context) ->
    Attachments = kz_doc:attachments(cb_context:fetch(Context, 'db_doc'), kz_json:new()),
    Templates = kz_json:foldl(fun leak_attachments_fold/3, kz_json:new(), Attachments),
    NewRespData = kz_json:set_value(<<"templates">>, Templates, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, NewRespData).

-spec leak_attachments_fold(kz_json:path(), kz_json:json_term(), kz_json:object()) -> kz_json:object().
leak_attachments_fold(_Attachment, Props, Acc) ->
    kz_json:set_value(kz_json:get_value(<<"content_type">>, Props)
                     ,kz_json:from_list([{<<"length">>, kz_json:get_integer_value(<<"length">>, Props)}])
                     ,Acc
                     ).

-spec load_smtp_log_doc(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_smtp_log_doc(?MATCH_MODB_PREFIX(YYYY,MM,_) = Id, Context) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    C1 = crossbar_doc:load(Id
                          ,cb_context:set_db_name(Context, kzs_util:format_account_id(cb_context:account_id(Context), Year, Month))
                          ,?TYPE_CHECK_OPTION(?PVT_TYPE_SMTPLOG)
                          ),
    case cb_context:resp_status(C1) of
        'success' ->
            Doc = cb_context:doc(C1),
            TemplateId = kz_json:get_ne_binary_value(<<"template_id">>, Doc),
            JObj = maybe_remove_private_data(Doc, TemplateId, IsSuperAdmin),
            Setters = [{fun cb_context:set_doc/2, JObj}
                      ,{fun cb_context:set_resp_data/2, JObj}
                      ],
            cb_context:setters(C1, Setters);
        _ ->
            C1
    end.

-spec maybe_remove_private_data(kz_json:object(), kz_term:ne_binary(), boolean()) -> kz_json:object().
maybe_remove_private_data(JObj, <<"port_comment">>, 'false') ->
    CommentPath = [<<"macros">>, <<"port_request">>, <<"comment">>],
    case kzd_comment:is_private_legacy(kz_json:get_json_value(CommentPath, JObj, kz_json:new())) of
        'true' ->
            DeletePaths = [CommentPath
                          ,<<"rendered_templates">>
                          ],
            kz_json:delete_keys(DeletePaths, JObj);
        'false' ->
            JObj
    end;
maybe_remove_private_data(JObj, _, _) ->
    JObj.

-spec maybe_update_db(cb_context:context()) -> cb_context:context().
maybe_update_db(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> cb_context:set_db_name(Context, ?KZ_CONFIG_DB);
        _AccountId -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc Remove Template Customization from an account
%% @end
%%------------------------------------------------------------------------------
-spec remove_account_customizations(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
remove_account_customizations(Context, AccountId) ->
    ToRemove = list_templates_from_db(kzs_util:format_account_db(AccountId)),
    Result = remove_customization(AccountId, ToRemove),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Result}
              ],
    cb_context:setters(Context, Setters).

-spec remove_customization(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
remove_customization(_, []) ->
    kz_json:from_list([{<<"message">>, <<"no template customization(s) found">>}]);
remove_customization(AccountId, Ids) ->
    lager:debug("removing ~b template customization(s) from ~s~n", [length(Ids), AccountId]),
    case kz_datamgr:del_docs(kzs_util:format_account_db(AccountId), Ids) of
        {'ok', JObjs} ->
            Result = [{kz_notification:resp_id(kz_doc:id(J)), kz_term:to_binary(kz_json:get_value(<<"error">>, J, <<"deleted">>))}
                      || J <- JObjs
                     ],
            kz_json:from_list(Result);
        {'error', _Reason} ->
            Msg = io_lib:format("failed to remove customization: ~p", [_Reason]),
            lager:debug(Msg),
            kz_json:from_list([{<<"message">>, Msg}])
    end.

%%------------------------------------------------------------------------------
%% @doc Forcing System's Templates to an account by first removing
%% account's customization and then copy the templates from
%% system_config to account's db.
%% @end
%%------------------------------------------------------------------------------
-spec force_system_templates(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
force_system_templates(Context, AccountId) ->
    ToRemove = list_templates_from_db(kzs_util:format_account_db(AccountId)),
    _ = remove_customization(AccountId, ToRemove),
    ToCopy = list_templates_from_db(?KZ_CONFIG_DB),
    Result = force_system_default(AccountId, ToCopy),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Result}
              ],
    cb_context:setters(Context, Setters).

-spec force_system_default(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
force_system_default(_, []) ->
    kz_json:from_list([{<<"message">>, <<"no system template found">>}]);
force_system_default(AccountId, Ids) ->
    lager:debug("forcing ~b system default template(s) for account ~s~n", [length(Ids), AccountId]),
    AccountDb = kzs_util:format_account_db(AccountId),
    kz_json:from_list([copy_from_system_to_account(AccountDb, Id) || Id <- Ids]).

-spec copy_from_system_to_account(kz_term:ne_binary(), kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
copy_from_system_to_account(AccountDb, Id) ->
    case kz_datamgr:copy_doc(?KZ_CONFIG_DB, Id, AccountDb, Id, []) of
        {'ok', _} -> {kz_notification:resp_id(Id), <<"replaced">>};
        {'error', Reason} -> {kz_notification:resp_id(Id), kz_term:to_binary(Reason)}
    end.

-spec list_templates_from_db(kz_term:ne_binary()) -> kz_term:ne_binaries().
list_templates_from_db(Db) ->
    ViewOpts = [{'startkey', <<"notification.">>}
               ,{'endkey', <<"notification.zzz">>}
               ],
    case kz_datamgr:all_docs(Db, ViewOpts) of
        {'ok', Results} ->
            [Id
             || Result <- Results,
                Id <- [kz_doc:id(Result)],
                'notification.skel' =/=  Id
            ];
        {'error', _E} ->
            lager:debug("failed to query existing notifications: ~p~n", [_E]),
            []
    end.
