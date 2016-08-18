%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_notifications).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
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

-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                                 ,{<<"text">>, <<"plain">>}
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

-define(PVT_TYPE_SMTPLOG, <<"notify_smtp_log">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.notifications">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.notifications">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.notifications">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.notifications">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.notifications">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.notifications">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.notifications">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.notifications">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?SMTP_LOG) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_, ?PREVIEW) ->
    [?HTTP_POST];
allowed_methods(?SMTP_LOG, _Id) ->
    [?HTTP_GET];
allowed_methods(?CUSTOMER_UPDATE, ?MESSAGE) ->
    [?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /notifications => []
%%    /notifications/foo => [<<"foo">>]
%%    /notifications/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.

resource_exists(?SMTP_LOG) -> 'true';
resource_exists(_Id) -> 'true'.

resource_exists(_Id, ?PREVIEW) -> 'true';
resource_exists(?SMTP_LOG, _Id) -> 'true';
resource_exists(?CUSTOMER_UPDATE, ?MESSAGE) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------

-spec acceptable_content_types() -> kz_proplist().
acceptable_content_types() ->
    ?NOTIFICATION_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided_for_notifications(cb_context:context(), path_token(), http_method()) ->
                                                      cb_context:context().
content_types_provided(Context, ?SMTP_LOG) ->
    Context;
content_types_provided(Context, Id) ->
    DbId = kz_notification:db_id(Id),
    ReqVerb = cb_context:req_verb(Context),
    content_types_provided_for_notifications(maybe_update_db(Context), DbId, ReqVerb).

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

-spec content_types_from_attachments(kz_json:object()) -> kz_proplist().
content_types_from_attachments(Attachments) ->
    kz_json:foldl(fun content_type_from_attachment/3, [], Attachments).

-spec content_type_from_attachment(kz_json:key(), kz_json:object(), kz_proplist()) ->
                                          kz_proplist().
content_type_from_attachment(_Name, Attachment, Acc) ->
    case kz_json:get_value(<<"content_type">>, Attachment) of
        'undefined' -> Acc;
        ContentType ->
            [Lhs, Rhs] = binary:split(ContentType, <<"/">>),
            [{Lhs,Rhs} | Acc]
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /notifications mights load a list of skel objects
%% /notifications/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().

validate(Context) ->
    ReqVerb = cb_context:req_verb(Context),
    validate_notifications(maybe_update_db(Context), ReqVerb).

validate(Context, ?SMTP_LOG) ->
    load_smtp_log(Context);
validate(Context, Id) ->
    ReqVerb = cb_context:req_verb(Context),
    DbId = kz_notification:db_id(Id),
    validate_notification(maybe_update_db(Context), DbId, ReqVerb).

validate(Context, Id, ?PREVIEW) ->
    DbId = kz_notification:db_id(Id),
    update_notification(maybe_update_db(Context), DbId);
validate(Context, ?SMTP_LOG, Id) ->
    load_smtp_log_doc(Id, Context);
validate(Context, ?CUSTOMER_UPDATE, ?MESSAGE) ->
    may_be_validate_recipient_id(Context).

-spec validate_notifications(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_notification(cb_context:context(), path_token(), http_method()) ->
                                   cb_context:context().
validate_notifications(Context, ?HTTP_GET) ->
    summary(Context);
validate_notifications(Context, ?HTTP_PUT) ->
    create(Context).

validate_notification(Context, Id, ?HTTP_GET) ->
    maybe_read(Context, Id);
validate_notification(Context, Id, ?HTTP_POST) ->
    maybe_update(Context, Id);
validate_notification(Context, Id, ?HTTP_DELETE) ->
    validate_delete_notification(Context, Id).

-spec validate_delete_notification(cb_context:context(), path_token()) ->
                                          cb_context:context().
-spec validate_delete_notification(cb_context:context(), path_token(), ne_binary()) ->
                                          cb_context:context().
validate_delete_notification(Context, Id) ->
    validate_delete_notification(Context, Id, cb_context:account_id(Context)).

validate_delete_notification(Context, Id, 'undefined') ->
    disallow_delete(Context, kz_notification:resp_id(Id));
validate_delete_notification(Context, Id, _AccountId) ->
    lager:debug("trying to remove notification from account ~s", [_AccountId]),
    read(Context, Id, 'account').

-spec disallow_delete(cb_context:context(), path_token()) -> cb_context:context().
disallow_delete(Context, Id) ->
    lager:debug("deleting the system config template is disallowed"),
    Resp =
        [{<<"target">>, Id}
        ,{<<"message">>, <<"Top-level notification template cannot be deleted">>}
        ],
    cb_context:add_validation_error(Id, <<"disallow">>, kz_json:from_list(Resp), Context).

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

-spec validate_recipient_id(ne_binary(), cb_context:context()) -> cb_context:context().
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

-spec sender_account_id(cb_context:context()) -> ne_binary().
sender_account_id(Context) ->
    sender_account_id(Context, cb_context:account_id(Context)).

-spec sender_account_id(cb_context:context(), ne_binary()|'undefined') -> ne_binary().
sender_account_id(Context, 'undefined') ->
    cb_context:auth_account_id(Context);
sender_account_id(_Context, AccountId) ->
    AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' -> leak_doc_id(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    case cb_context:req_files(Context) of
        [] ->
            lager:debug("handling POST of template meta for ~s", [Id]),
            do_post(Context);
        [{_FileName, FileJObj}] ->
            lager:debug("POST is for an attachment on ~s(~s)", [Id, kz_notification:db_id(Id)]),
            update_template(Context, kz_notification:db_id(Id), FileJObj)
    end.

-spec do_post(cb_context:context()) -> cb_context:context().
do_post(Context) ->
    Context1 = crossbar_doc:save(set_system_macros(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_note_notification_preference(Context1),
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
            lager:warning("fail to update macros from system_config"),
            Context
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?CUSTOMER_UPDATE, ?MESSAGE) ->
    case kz_amqp_worker:call(build_customer_update_payload(Context)
                            ,fun kapi_notifications:publish_customer_update/1
                            ,fun kapi_notifications:customer_update_v/1
                            )
    of
        {'ok', _Resp} ->
            lager:debug("published customer_update notification"),
            Context;
        {'error', _E} ->
            lager:debug("failed to publish_customer update notification: ~p", [_E]),
            crossbar_util:response('error', <<"Failed to send message">>, Context)
    end;

post(Context, Id, ?PREVIEW) ->
    Notification = cb_context:doc(Context),
    Preview = build_preview_payload(Context, Notification),
    {API, _} = lists:foldl(fun preview_fold/2
                          ,{Preview, Notification}
                          ,headers(Id)
                          ),
    case kz_amqp_worker:call(API
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

-spec build_customer_update_payload(cb_context:context()) -> kz_proplist().
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

-spec build_preview_payload(cb_context:context(), kz_json:object()) -> kz_proplist().
build_preview_payload(Context, Notification) ->
    props:filter_empty(
      [{<<"To">>, kz_json:get_value(<<"to">>, Notification)}
      ,{<<"From">>, kz_json:get_value(<<"from">>, Notification)}
      ,{<<"Cc">>, kz_json:get_value(<<"cc">>, Notification)}
      ,{<<"Bcc">>, kz_json:get_value(<<"bcc">>, Notification)}
      ,{<<"Reply-To">>, kz_json:get_value(<<"reply_to">>, Notification)}
      ,{<<"Subject">>, kz_json:get_value(<<"subject">>, Notification)}
      ,{<<"HTML">>, kz_json:get_value(<<"html">>, Notification)}
      ,{<<"Text">>, kz_json:get_value(<<"plain">>, Notification)}
      ,{<<"Account-ID">>, cb_context:account_id(Context)}
      ,{<<"Account-DB">>, cb_context:account_db(Context)}
      ,{<<"Msg-ID">>, cb_context:req_id(Context)}
      ,{<<"Call-ID">>, cb_context:req_id(Context)}
      ,{<<"Preview">>, 'true'}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec handle_preview_response(cb_context:context(), kz_json:object()) -> cb_context:context().
handle_preview_response(Context, Resp) ->
    case kz_json:get_value(<<"Status">>, Resp) of
        <<"failed">> ->
            lager:debug("failed notificaiton preview: ~p", [Resp]),
            CleansedResp = kz_json:normalize(kz_api:remove_defaults(Resp)),
            crossbar_util:response_invalid_data(CleansedResp, Context);
        _Status ->
            lager:debug("notification preview status :~s", [_Status]),
            crossbar_util:response_202(<<"Notification processing">>, Context)
    end.

-spec headers(ne_binary()) -> ne_binaries().
headers(<<"voicemail_to_email">>) ->
    kapi_notifications:headers(<<"voicemail">>);
headers(<<"port_request_admin">>) ->
    kapi_notifications:headers(<<"port_request">>);
headers(Id) ->
    kapi_notifications:headers(Id).

-spec publish_fun(ne_binary()) -> fun((api_terms()) -> 'ok').
publish_fun(<<"voicemail_to_email">>) ->
    fun kapi_notifications:publish_voicemail/1;
publish_fun(<<"voicemail_full">>) ->
    fun kapi_notifications:publish_voicemail_full/1;
publish_fun(<<"fax_inbound_to_email">>) ->
    fun kapi_notifications:publish_fax_inbound/1;
publish_fun(<<"fax_inbound_error_to_email">>) ->
    fun kapi_notifications:publish_fax_inbound_error/1;
publish_fun(<<"fax_outbound_to_email">>) ->
    fun kapi_notifications:publish_fax_outbound/1;
publish_fun(<<"fax_outbound_error_to_email">>) ->
    fun kapi_notifications:publish_fax_outbound_error/1;
publish_fun(<<"low_balance">>) ->
    fun kapi_notifications:publish_low_balance/1;
publish_fun(<<"new_account">>) ->
    fun kapi_notifications:publish_new_account/1;
publish_fun(<<"new_user">>) ->
    fun kapi_notifications:publish_new_user/1;
publish_fun(<<"deregister">>) ->
    fun kapi_notifications:publish_deregister/1;
publish_fun(<<"transaction">>) ->
    fun kapi_notifications:publish_transaction/1;
publish_fun(<<"password_recovery">>) ->
    fun kapi_notifications:publish_password_recovery/1;
publish_fun(<<"system_alert">>) ->
    fun kapi_notifications:publish_system_alert/1;
publish_fun(<<"cnam_request">>) ->
    fun kapi_notifications:publish_cnam_request/1;
publish_fun(<<"topup">>) ->
    fun kapi_notifications:publish_topup/1;
publish_fun(<<"port_request">>) ->
    fun kapi_notifications:publish_port_request/1;
publish_fun(<<"port_request_admin">>) ->
    fun kapi_notifications:publish_port_request/1;
publish_fun(<<"port_scheduled">>) ->
    fun kapi_notifications:publish_port_scheduled/1;
publish_fun(<<"port_rejected">>) ->
    fun kapi_notifications:publish_port_rejected/1;
publish_fun(<<"port_cancel">>) ->
    fun kapi_notifications:publish_port_cancel/1;
publish_fun(<<"ported">>) ->
    fun kapi_notifications:publish_ported/1;
publish_fun(<<"webhook_disabled">>) ->
    fun kapi_notifications:publish_webhook_disabled/1;
publish_fun(<<"denied_emergency_bridge">>) ->
    fun kapi_notifications:publish_denied_emergency_bridge/1;
publish_fun(<<"customer_update">>) ->
    fun kapi_notifications:publish_customer_update/1;
publish_fun(_Id) ->
    lager:debug("no kapi_notifications:publish_~s/1 defined", [_Id]),
    fun(_Any) -> 'ok' end.

-spec preview_fold(ne_binary(), {kz_proplist(), kz_json:object()}) ->
                          {kz_proplist(), kz_json:object()}.
preview_fold(Header, {Props, ReqData}) ->
    case kz_json:get_first_defined([Header, kz_json:normalize_key(Header)], ReqData) of
        'undefined' ->
            {props:insert_value(Header, Header, Props), ReqData};
        V ->
            {props:set_value(Header, V, Props), ReqData}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
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

-spec delete_doc(cb_context:context(), ne_binary()) -> cb_context:context().
delete_doc(Context, Id) ->
    Context1 = crossbar_doc:delete(Context, 'permanent'),
    case cb_context:resp_status(Context1) of
        'success' ->
            kz_datamgr:flush_cache_doc(cb_context:account_db(Context), Id),
            leak_doc_id(Context1);
        _Status -> Context1
    end.

-spec maybe_delete_template(cb_context:context(), ne_binary(), ne_binary()) ->
                                   cb_context:context().
-spec maybe_delete_template(cb_context:context(), ne_binary(), ne_binary(), kz_json:object()) ->
                                   cb_context:context().
maybe_delete_template(Context, Id, ContentType) ->
    maybe_delete_template(Context, Id, ContentType, cb_context:doc(Context)).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec accept_values(cb_context:context()) -> media_values().
accept_values(Context) ->
    AcceptValue = cb_context:req_header(Context, <<"accept">>),
    Tunneled = cb_context:req_value(Context, <<"accept">>),
    media_values(AcceptValue, Tunneled).

-spec media_values(api_binary()) -> media_values().
-spec media_values(api_binary(), api_binary()) -> media_values().
media_values(Media) ->
    media_values(Media, 'undefined').

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

-spec acceptable_content_types(cb_context:context()) -> kz_proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec maybe_read(cb_context:context(), ne_binary()) -> cb_context:context().
-spec maybe_read(cb_context:context(), ne_binary(), kz_proplist(), media_values()) -> cb_context:context().
maybe_read(Context, Id) ->
    Acceptable = acceptable_content_types(Context),
    maybe_read(Context, Id, Acceptable, accept_values(Context)).

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

-spec is_acceptable_accept(kz_proplist(), ne_binary(), ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:member({Type,SubType}, Acceptable).

-type load_from() :: 'system' | 'account' | 'system_migrate'.

-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
-spec read(cb_context:context(), ne_binary(), load_from()) -> cb_context:context().
read(Context, Id) ->
    read(Context, Id, 'system').

read(Context, Id, LoadFrom) ->
    Context1 =
        case cb_context:account_db(Context) of
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

-spec read_system(cb_context:context(), ne_binary()) -> cb_context:context().
read_system(Context, Id) ->
    crossbar_doc:load(Id
                     ,cb_context:set_account_db(Context, ?KZ_CONFIG_DB)
                     ,?TYPE_CHECK_OPTION(kz_notification:pvt_type())).

-spec read_account(cb_context:context(), ne_binary(), load_from()) -> cb_context:context().
read_account(Context, Id, LoadFrom) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kz_notification:pvt_type())),
    case {cb_context:resp_error_code(Context1)
         ,cb_context:resp_status(Context1)
         }
    of
        {404, 'error'} when LoadFrom =:= 'system'; LoadFrom =:= 'system_migrate' ->
            lager:debug("~s not found in account, reading from master", [Id]),
            read_system_for_account(Context, Id, LoadFrom);
        {_Code, 'success'} ->
            lager:debug("loaded ~s from account database", [Id]),
            NewRespData = note_account_override(cb_context:resp_data(Context1)),
            cb_context:set_resp_data(Context1, NewRespData);
        {_Code, _Status} ->
            lager:debug("failed to load ~s: ~p", [Id, _Code]),
            Context1
    end.

-spec read_system_for_account(cb_context:context(), path_token(), load_from()) ->
                                     cb_context:context().
read_system_for_account(Context, Id, LoadFrom) ->
    Context1 = read_system(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' when LoadFrom =:= 'system' ->
            lager:debug("read template ~s from master account", [Id]),
            revert_context_to_account(Context, Context1);
        'success' when LoadFrom =:= 'system_migrate' ->
            lager:debug("read template ~s from master account, now migrating it", [Id]),
            migrate_template_to_account(revert_context_to_account(Context, Context1), Id);
        _Status ->
            lager:debug("failed to read master db for ~s", [Id]),
            Context1
    end.

-spec revert_context_to_account(cb_context:context(), cb_context:context()) -> cb_context:context().
revert_context_to_account(AccountContext, SystemContext) ->
    cb_context:setters(SystemContext
                      ,[{fun cb_context:set_account_db/2, cb_context:account_db(AccountContext)}
                       ,{fun cb_context:set_account_id/2, cb_context:account_id(AccountContext)}
                       ]).

-spec migrate_template_to_account(cb_context:context(), path_token()) -> cb_context:context().
migrate_template_to_account(Context, Id) ->
    lager:debug("saving template ~s from system config to account ~s", [Id, cb_context:account_id(Context)]),

    Template = cb_context:fetch(Context, 'db_doc'),

    Context1 =
        crossbar_doc:ensure_saved(
          cb_context:set_doc(Context
                            ,kz_notification:set_base_properties(
                               kz_doc:public_fields(Template)
                                                                ,Id
                              )
                            )
         ),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("saved template ~s to account ~s", [Id, cb_context:account_db(Context1)]),
            Context2 = migrate_template_attachments(Context1, Id, kz_doc:attachments(Template)),
            maybe_note_notification_preference(Context2),
            Context2;
        _Status -> Context1
    end.

-spec maybe_hard_delete(cb_context:context(), ne_binary()) -> 'ok'.
maybe_hard_delete(Context, Id) ->
    case kz_datamgr:del_doc(cb_context:account_db(Context), Id) of
        {'ok', _} ->
            kz_datamgr:flush_cache_doc(cb_context:account_db(Context), Id),
            lager:debug("hard-deleted old version of ~s from ~s", [Id, cb_context:account_db(Context)]);
        {'error', 'not_found'} ->
            kz_datamgr:flush_cache_doc(cb_context:account_db(Context), Id),
            lager:debug("~s wasn't found in ~s", [Id, cb_context:account_db(Context)]);
        {'error', _E} ->
            lager:debug("error deleting ~s from ~s: ~p", [Id, cb_context:account_db(Context), _E])
    end.

-spec maybe_note_notification_preference(cb_context:context()) -> 'ok'.
-spec maybe_note_notification_preference(ne_binary(), kz_json:object()) -> 'ok'.
maybe_note_notification_preference(Context) ->
    AccountDb = cb_context:account_db(Context),
    case kz_account:fetch(AccountDb) of
        {'error', _E} -> lager:debug("failed to note preference: ~p", [_E]);
        {'ok', AccountJObj} ->
            maybe_note_notification_preference(AccountDb, AccountJObj)
    end.

maybe_note_notification_preference(AccountDb, AccountJObj) ->
    case kz_account:notification_preference(AccountJObj) of
        'undefined' -> note_notification_preference(AccountDb, AccountJObj);
        <<"teletype">> -> lager:debug("account already prefers teletype");
        _Pref -> note_notification_preference(AccountDb, AccountJObj)
    end.

-spec note_notification_preference(ne_binary(), kz_json:object()) -> 'ok'.
note_notification_preference(AccountDb, AccountJObj) ->
    case kz_datamgr:save_doc(AccountDb
                            ,kz_account:set_notification_preference(AccountJObj
                                                                   ,<<"teletype">>
                                                                   )
                            )
    of
        {'ok', AccountJObj} ->
            _ = cb_accounts:replicate_account_definition(AccountJObj),
            lager:debug("updated pref for account");
        {'error', _E} ->
            lager:debug("failed to note preference: ~p", [_E])
    end.

-spec migrate_template_attachments(cb_context:context(), ne_binary(), api_object()) ->
                                          cb_context:context().
migrate_template_attachments(Context, _Id, 'undefined') ->
    lager:debug("no attachments to migrate for ~s", [_Id]),
    Context;
migrate_template_attachments(Context, Id, Attachments) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    kz_json:foldl(fun(AName, AMeta, C) ->
                          lager:debug("migrate attachment ~s", [AName]),
                          migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, C)
                  end, Context, Attachments).

-spec migrate_template_attachment(ne_binary(), ne_binary(), ne_binary(), kz_json:object(), cb_context:context()) ->
                                         cb_context:context().
migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, Context) ->
    case kz_datamgr:fetch_attachment(MasterAccountDb, Id, AName) of
        {'ok', Bin} ->
            ContentType = kz_json:get_value(<<"content_type">>, AMeta),
            lager:debug("saving attachment for ~s(~s): ~s", [Id, AName, ContentType]),
            Opts = [{'headers'
                    ,[{'content_type', kz_util:to_list(ContentType)}]
                    }
                    | ?TYPE_CHECK_OPTION(kz_notification:pvt_type())
                   ],
            crossbar_doc:save_attachment(Id
                                        ,attachment_name_by_content_type(ContentType)
                                        ,Bin
                                        ,Context
                                        ,Opts
                                        );
        {'error', _E} ->
            lager:debug("failed to load attachment ~s for ~s: ~p", [AName, Id, _E]),
            Context
    end.

-spec note_account_override(kz_json:object()) -> kz_json:object().
note_account_override(JObj) ->
    kz_json:set_value(<<"account_overridden">>, 'true', JObj).

-spec read_success(cb_context:context()) -> cb_context:context().
read_success(Context) ->
    cb_context:setters(Context, [fun leak_attachments/1
                                ,fun leak_doc_id/1
                                ]).

-spec maybe_read_template(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
maybe_read_template(Context, _Id, <<"application/json">>) -> Context;
maybe_read_template(Context, _Id, <<"application/x-json">>) -> Context;
maybe_read_template(Context, Id, Accept) ->
    case cb_context:resp_status(Context) of
        'success' -> read_template(Context, Id, Accept);
        _Status -> Context
    end.

-spec read_template(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_template(Context, Id, Accept) ->
    Doc = cb_context:fetch(Context, 'db_doc'),
    AttachmentName = attachment_name_by_media_type(Accept),
    case kz_doc:attachment(Doc, AttachmentName) of
        'undefined' ->
            lager:debug("failed to find attachment ~s in ~s", [AttachmentName, Id]),
            crossbar_util:response_faulty_request(Context);
        _Meta ->
            lager:debug("found attachment ~s in ~s", [AttachmentName, Id]),
            cb_context:add_resp_headers(
              read_account_attachment(Context, Id, AttachmentName)
                                       ,[{<<"Content-Disposition">>, attachment_filename(Id, Accept)}
                                        ,{<<"Content-Type">>, kz_doc:attachment_content_type(Doc, AttachmentName)}
                                        ,{<<"Content-Length">>, kz_doc:attachment_length(Doc, AttachmentName)}
                                        ])
    end.

-spec attachment_filename(ne_binary(), ne_binary()) -> iolist().
attachment_filename(Id, Accept) ->
    [<<"attachment; filename=">>
    ,kz_notification:resp_id(Id)
    ,$., kz_mime:to_extension(Accept)
    ].

-spec read_system_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_system_attachment(Context, DocId, Name) ->
    crossbar_doc:load_attachment(DocId
                                ,Name
                                ,?TYPE_CHECK_OPTION(kz_notification:pvt_type())
                                ,cb_context:set_account_db(Context, ?KZ_CONFIG_DB)
                                ).

-spec read_account_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_account_attachment(Context, DocId, Name) ->
    Context1 = crossbar_doc:load_attachment(DocId, Name, ?TYPE_CHECK_OPTION(kz_notification:pvt_type()), Context),
    case {cb_context:resp_error_code(Context1)
         ,cb_context:resp_status(Context1)
         }
    of
        {404, 'error'} ->
            lager:debug("~s not found in account, reading from master", [DocId]),
            read_system_attachment(Context, DocId, Name);
        {_Code, 'success'} ->
            lager:debug("loaded ~s from account database", [DocId]),
            Context1;
        {_Code, _Status} ->
            lager:debug("failed to load ~s: ~p", [DocId, _Code]),
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec maybe_update(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_update(Context, Id) ->
    case cb_context:req_files(Context) of
        [] ->
            lager:debug("updating template meta for ~s", [Id]),
            update_notification(Context, Id);
        [{_FileName, FileJObj}] ->
            lager:debug("recv template upload of ~s: ~p", [_FileName, FileJObj]),
            read(Context, Id, 'system_migrate')
    end.

-spec update_notification(cb_context:context(), ne_binary()) -> cb_context:context().
update_notification(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

-spec update_template(cb_context:context(), path_token(), kz_json:object()) ->
                             cb_context:context().
update_template(Context, Id, FileJObj) ->
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    lager:debug("file content type for ~s: ~s", [Id, CT]),

    Opts = [{'content_type', kz_util:to_list(CT)} | ?TYPE_CHECK_OPTION(kz_notification:pvt_type())], % Temporary until couchbeam update

    AttachmentName = attachment_name_by_content_type(CT),

    crossbar_doc:save_attachment(
      Id
                                ,AttachmentName
                                ,Contents
                                ,Context
                                ,Opts
     ).

-spec attachment_name_by_content_type(ne_binary()) -> ne_binary().
attachment_name_by_content_type(CT) ->
    <<"template.", (kz_http_util:urlencode(CT))/binary>>.

-spec attachment_name_by_media_type(ne_binary()) -> ne_binary().
attachment_name_by_media_type(CT) ->
    <<"template.", CT/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:account_db(Context) of
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
    Context1 =
        crossbar_doc:load_view(?CB_LIST
                              ,[]
                              ,cb_context:set_account_db(Context, ?KZ_CONFIG_DB)
                              ,select_normalize_fun(Context)
                              ),
    cache_available(Context1),
    Context1.

-spec cache_available(cb_context:context()) -> 'ok'.
cache_available(Context) ->
    kz_cache:store_local(?CACHE_NAME
                        ,{?MODULE, 'available'}
                        ,cb_context:doc(Context)
                        ,[{'origin', [{'db', cb_context:account_db(Context), kz_notification:pvt_type()}]}]
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
-spec summary_account(cb_context:context(), kz_json:objects()) -> cb_context:context().
summary_account(Context) ->
    Context1 =
        crossbar_doc:load_view(?CB_LIST
                              ,[]
                              ,Context
                              ,select_normalize_fun(Context)
                              ),
    lager:debug("loaded account's summary"),
    summary_account(Context1, cb_context:doc(Context1)).

summary_account(Context, AccountAvailable) ->
    Available = filter_available(summary_available(Context)),
    lager:debug("loaded system available"),
    JObj = merge_available(AccountAvailable, Available),
    crossbar_doc:handle_json_success(JObj, Context).

-spec filter_available(cb_context:context()) -> kz_json:objects().
filter_available(Context) ->
    [A || A <- cb_context:doc(Context),
          crossbar_doc:filtered_doc_by_qs(kz_json:from_list([{<<"doc">>, A}])
                                         ,'true'
                                         ,Context
                                         )
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
    lager:debug("noting ~s is overridden in account", [Id]),
    [note_account_override(Overridden)
     | [JObj || JObj <- Acc,
                kz_doc:id(JObj) =/= Id
       ]
    ].

-type normalize_fun() :: fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-spec select_normalize_fun(cb_context:context()) -> normalize_fun().
select_normalize_fun(Context) ->
    Account = cb_context:auth_account_id(Context),
    case kz_util:is_system_admin(Account) of
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

    case kz_services:is_reseller(AuthAccountId)
        andalso cb_port_requests:authority(AccountId)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec system_config_notification_doc(ne_binary()) ->
                                            {'ok', kz_json:object()} |
                                            {'error', any()}.
system_config_notification_doc(DocId) ->
    kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, DocId).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
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
            handle_missing_account_notification(CleanedContext, Id, cb_context:req_nouns(CleanedContext));
        {'success', _} -> Context1;
        {_Status, _Code} ->
            lager:debug("load/merge of ~s failed with ~p / ~p", [Id, _Status, _Code]),
            Context1
    end.

-spec handle_missing_account_notification(cb_context:context(), ne_binary(), kz_proplist()) ->
                                                 cb_context:context().
handle_missing_account_notification(Context, Id, [{<<"notifications">>, [_Id, ?PREVIEW]}|_]) ->
    lager:debug("preview request, ignoring if notification ~s is missing", [Id]),
    Context;
handle_missing_account_notification(Context, Id, _ReqNouns) ->
    _ = maybe_hard_delete(Context, Id),
    _Context = read_system_for_account(Context, Id, 'system_migrate'),
    on_successful_validation(Id, Context).

-spec handle_missing_system_config_notification(cb_context:context(), ne_binary(), kz_json:object()) ->
                                                       cb_context:context().
handle_missing_system_config_notification(Context, DocId, ReqTemplate) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("creating system config notification for ~s", [DocId]),
            create_new_notification(Context, DocId, ReqTemplate);
        _AccountId ->
            lager:debug("doc ~s does not exist in the system config, not letting ~s create it"
                       ,[DocId, _AccountId]
                       ),
            crossbar_util:response_bad_identifier(kz_notification:resp_id(DocId), Context)
    end.

-spec create_new_notification(cb_context:context(), ne_binary(), kz_json:object()) ->
                                     cb_context:context().
create_new_notification(Context, DocId, ReqTemplate) ->
    lager:debug("this will create a new template in the system config"),
    Doc = kz_notification:set_base_properties(ReqTemplate, DocId),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, Doc}
                       ,{fun cb_context:set_account_db/2, ?KZ_CONFIG_DB}
                       ,{fun cb_context:set_account_id/2, ?KZ_CONFIG_DB}
                       ]).

-spec clean_req_doc(kz_json:object()) -> kz_json:object().
clean_req_doc(Doc) ->
    kz_json:delete_keys([?MACROS
                        ,<<"templates">>
                        ], Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

-spec leak_attachments_fold(kz_json:key(), kz_json:json_term(), kz_json:object()) -> kz_json:object().
leak_attachments_fold(_Attachment, Props, Acc) ->
    kz_json:set_value(kz_json:get_value(<<"content_type">>, Props)
                     ,kz_json:from_list([{<<"length">>, kz_json:get_integer_value(<<"length">>, Props)}])
                     ,Acc
                     ).

-spec load_smtp_log_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_smtp_log_doc(?MATCH_MODB_PREFIX(YYYY,MM,_) = Id, Context) ->
    Year  = kz_util:to_integer(YYYY),
    Month = kz_util:to_integer(MM),
    crossbar_doc:load(Id
                     ,cb_context:set_account_modb(Context, Year, Month)
                     ,?TYPE_CHECK_OPTION(?PVT_TYPE_SMTPLOG)).

-spec load_smtp_log(cb_context:context()) -> cb_context:context().
load_smtp_log(Context) ->
    case cb_modules_util:range_modb_view_options(Context) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(?CB_LIST_SMTP_LOG
                                  ,ViewOptions
                                  ,Context
                                  ,fun normalize_view_results/2
                                  );
        Ctx -> Ctx
    end.

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec maybe_update_db(cb_context:context()) -> cb_context:context().
maybe_update_db(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> cb_context:set_account_db(Context, ?KZ_CONFIG_DB);
        _AccountId -> Context
    end.
