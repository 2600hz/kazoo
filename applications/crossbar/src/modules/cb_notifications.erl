%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_notifications).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/2
         ,content_types_accepted/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2, post/3
         ,delete/2

         ,flush/0
        ]).

-ifdef(TEST).
-export([merge_available/2]).
-endif.

-include("../crossbar.hrl").

-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                                  ,{<<"text">>, <<"plain">>}
                                 ]).
-define(CB_LIST, <<"notifications/crossbar_listing">>).
-define(PREVIEW, <<"preview">>).
-define(SMTP_LOG, <<"logs">>).
-define(CB_LIST_SMTP_LOG, <<"notifications/smtp_log">>).

-define(MACROS, <<"macros">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".notifications">>).
-define(NOTIFICATION_TIMEOUT
        ,whapps_config:get_integer(?MOD_CONFIG_CAT, <<"notification_timeout_ms">>, 5 * ?MILLISECONDS_IN_SECOND)
       ).

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
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
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
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), ne_binary(), req_nouns()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:auth_account_id(Context), cb_context:req_nouns(Context)).

authorize(_Context, AuthAccountId, [{<<"notifications">>, _Id}
                                    ,{<<"accounts">>, [AccountId]}
                                   ]) ->
    lager:debug("maybe authz for ~s to modify ~s in ~s", [AuthAccountId, _Id, AccountId]),
    wh_services:is_reseller(AuthAccountId)
        andalso wh_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true');
authorize(Context, AuthAccountId, [{<<"notifications">>, []}]) ->
    lager:debug("checking authz on system request to /"),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    cb_context:req_verb(Context) =:= ?HTTP_GET
        orelse AuthAccountId =:= MasterAccountId;
authorize(Context, AuthAccountId, [{<<"notifications">>, _Id}]) ->
    lager:debug("maybe authz for system notification ~s", [_Id]),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    cb_context:req_verb(Context) =:= ?HTTP_GET
        orelse AuthAccountId =:= MasterAccountId;
authorize(_Context, _AuthAccountId, _Nouns) ->
    'false'.

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
resource_exists(_Id) -> 'true'.
resource_exists(_Id, ?PREVIEW) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_provided(Context, ?SMTP_LOG) ->
    Context;
content_types_provided(Context, Id) ->
    content_types_provided(Context, kz_notification:db_id(Id), cb_context:req_verb(Context)).

content_types_provided(Context, Id, ?HTTP_GET) ->
    Context1 = read(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_set_content_types(Context1);
        _Status -> Context1
    end;
content_types_provided(Context, Id, ?HTTP_DELETE) ->
    Context1 = read(Context, Id, 'account'),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_set_content_types(Context1);
        _Status -> Context1
    end;
content_types_provided(Context, _Id, _Verb) ->
    Context.

-spec maybe_set_content_types(cb_context:context()) -> cb_context:context().
maybe_set_content_types(Context) ->
    case wh_doc:attachments(cb_context:doc(Context)) of
        'undefined' -> Context;
        Attachments -> set_content_types(Context, Attachments)
    end.

-spec set_content_types(cb_context:context(), wh_json:object()) -> cb_context:context().
set_content_types(Context, Attachments) ->
    ContentTypes = content_types_from_attachments(Attachments),
    lager:debug("setting content types for attachments: ~p", [ContentTypes]),
    cb_context:set_content_types_provided(Context, [{'to_json', ?JSON_CONTENT_TYPES}
                                                    ,{'to_binary', ContentTypes}
                                                   ]).

-spec content_types_from_attachments(wh_json:object()) -> wh_proplist().
content_types_from_attachments(Attachments) ->
    wh_json:foldl(fun content_type_from_attachment/3, [], Attachments).

-spec content_type_from_attachment(wh_json:key(), wh_json:object(), wh_proplist()) ->
                                          wh_proplist().
content_type_from_attachment(_Name, Attachment, Acc) ->
    case wh_json:get_value(<<"content_type">>, Attachment) of
        'undefined' -> Acc;
        ContentType ->
            [list_to_tuple(
               binary:split(ContentType, <<"/">>)
              )
             | Acc
            ]
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
    validate_notifications(Context, cb_context:req_verb(Context)).

validate(Context, ?SMTP_LOG) ->
    load_smtp_log(Context);
validate(Context, Id) ->
    validate_notification(Context, kz_notification:db_id(Id), cb_context:req_verb(Context)).

validate(Context, Id, ?PREVIEW) ->
    update_notification(Context, kz_notification:db_id(Id)).

-spec validate_notifications(cb_context:context(), http_method()) ->
                                    cb_context:context().
validate_notifications(Context, ?HTTP_GET) ->
    summary(Context);
validate_notifications(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_notification(cb_context:context(), path_token(), http_method()) ->
                                   cb_context:context().
validate_notification(Context, Id, ?HTTP_GET) ->
    maybe_read(Context, Id);
validate_notification(Context, Id, ?HTTP_POST) ->
    maybe_update(Context, Id);
validate_notification(Context, Id, ?HTTP_DELETE) ->
    validate_delete_notification(Context, Id).

-spec validate_delete_notification(cb_context:context(), path_token()) ->
                                          cb_context:context().
-spec validate_delete_notification(cb_context:context(), path_token(), ne_binary(), ne_binary()) ->
                                          cb_context:context().
validate_delete_notification(Context, Id) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    validate_delete_notification(Context, Id, MasterAccountId, cb_context:account_id(Context)).

validate_delete_notification(Context, Id, MasterAccountId, MasterAccountId) ->
    disallow_delete(Context, kz_notification:resp_id(Id));
validate_delete_notification(Context, Id, _MasterAccountId, 'undefined') ->
    disallow_delete(Context, kz_notification:resp_id(Id));
validate_delete_notification(Context, Id, _MasterAccuontId, _AccountId) ->
    lager:debug("trying to remove notification from account ~s", [_AccountId]),
    read(Context, Id, 'account').

-spec disallow_delete(cb_context:context(), path_token()) -> cb_context:context().
disallow_delete(Context, Id) ->
    lager:debug("deleting the master template is disallowed"),
    cb_context:add_validation_error(Id
                                    ,<<"disallow">>
                                    ,wh_json:from_list(
                                       [{<<"message">>, <<"Top-level notification template cannot be deleted">>}
                                        ,{<<"target">>, Id}
                                       ]
                                      )
                                    ,Context
                                   ).

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
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
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
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_note_notification_preference(Context1),
            leak_doc_id(Context1);
        _Status -> Context1
    end.

post(Context, Id, ?PREVIEW) ->
    Notification = cb_context:doc(Context),
    Preview = build_preview_payload(Context, Notification),
    {API, _} = lists:foldl(fun preview_fold/2
                           ,{Preview, Notification}
                           ,headers(Id)
                          ),
    case wh_amqp_worker:call(API
                             ,publish_fun(Id)
                             ,fun wapi_notifications:notify_update_v/1
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

-spec build_preview_payload(cb_context:context(), wh_json:object()) -> wh_proplist().
build_preview_payload(Context, Notification) ->
    props:filter_empty(
      [{<<"To">>, wh_json:get_value(<<"to">>, Notification)}
       ,{<<"From">>, wh_json:get_value(<<"from">>, Notification)}
       ,{<<"Cc">>, wh_json:get_value(<<"cc">>, Notification)}
       ,{<<"Bcc">>, wh_json:get_value(<<"bcc">>, Notification)}
       ,{<<"Reply-To">>, wh_json:get_value(<<"reply_to">>, Notification)}
       ,{<<"Subject">>, wh_json:get_value(<<"subject">>, Notification)}
       ,{<<"HTML">>, wh_json:get_value(<<"html">>, Notification)}
       ,{<<"Text">>, wh_json:get_value(<<"plain">>, Notification)}
       ,{<<"Account-ID">>, cb_context:account_id(Context)}
       ,{<<"Account-DB">>, cb_context:account_db(Context)}
       ,{<<"Msg-ID">>, cb_context:req_id(Context)}
       ,{<<"Call-ID">>, cb_context:req_id(Context)}
       ,{<<"Preview">>, 'true'}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec handle_preview_response(cb_context:context(), wh_json:object()) -> cb_context:context().
handle_preview_response(Context, Resp) ->
    case wh_json:get_value(<<"Status">>, Resp) of
        <<"failed">> ->
            lager:debug("failed notificaiton preview: ~p", [Resp]),
            crossbar_util:response_invalid_data(
              wh_json:normalize(wh_api:remove_defaults(Resp))
              ,Context
             );
        _Status ->
            lager:debug("notification preview status :~s", [_Status]),
            crossbar_util:response_202(<<"Notification processing">>, Context)
    end.

-spec headers(ne_binary()) -> wh_proplist().
headers(<<"voicemail_to_email">>) ->
    wapi_notifications:headers(<<"voicemail">>);
headers(Id) ->
    wapi_notifications:headers(Id).

-spec publish_fun(ne_binary()) -> fun((api_terms()) -> 'ok').
publish_fun(<<"voicemail_to_email">>) ->
    fun wapi_notifications:publish_voicemail/1;
publish_fun(<<"voicemail_full">>) ->
    fun wapi_notifications:publish_voicemail_full/1;
publish_fun(<<"fax_inbound_to_email">>) ->
    fun wapi_notifications:publish_fax_inbound/1;
publish_fun(<<"fax_inbound_error_to_email">>) ->
    fun wapi_notifications:publish_fax_inbound_error/1;
publish_fun(<<"fax_outbound_to_email">>) ->
    fun wapi_notifications:publish_fax_outbound/1;
publish_fun(<<"fax_outbound_error_to_email">>) ->
    fun wapi_notifications:publish_fax_outbound_error/1;
publish_fun(<<"low_balance">>) ->
    fun wapi_notifications:publish_low_balance/1;
publish_fun(<<"new_account">>) ->
    fun wapi_notifications:publish_new_account/1;
publish_fun(<<"new_user">>) ->
    fun wapi_notifications:publish_new_user/1;
publish_fun(<<"deregister">>) ->
    fun wapi_notifications:publish_deregister/1;
publish_fun(<<"transaction">>) ->
    fun wapi_notifications:publish_transaction/1;
publish_fun(<<"password_recovery">>) ->
    fun wapi_notifications:publish_pwd_recovery/1;
publish_fun(<<"system_alert">>) ->
    fun wapi_notifications:publish_system_alert/1;
publish_fun(<<"cnam_request">>) ->
    fun wapi_notifications:publish_cnam_request/1;
publish_fun(<<"topup">>) ->
    fun wapi_notifications:publish_topup/1;
publish_fun(<<"port_request">>) ->
    fun wapi_notifications:publish_port_request/1;
publish_fun(<<"port_scheduled">>) ->
    fun wapi_notifications:publish_port_scheduled/1;
publish_fun(<<"port_cancel">>) ->
    fun wapi_notifications:publish_port_cancel/1;
publish_fun(<<"ported">>) ->
    fun wapi_notifications:publish_ported/1;
publish_fun(<<"webhook_disabled">>) ->
    fun wapi_notifications:publish_webhook_disabled/1;
publish_fun(_Id) ->
    lager:debug("no wapi_notification:publish_~s/1 defined", [_Id]),
    fun(_Any) -> 'ok' end.

-spec preview_fold(ne_binary(), {wh_proplist(), wh_json:object()}) ->
                          {wh_proplist(), wh_json:object()}.
preview_fold(Header, {Props, ReqData}) ->
    case wh_json:get_first_defined([Header, wh_json:normalize_key(Header)], ReqData) of
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
            couch_mgr:flush_cache_doc(cb_context:account_db(Context), Id),
            leak_doc_id(Context1);
        _Status -> Context1
    end.

-spec maybe_delete_template(cb_context:context(), ne_binary(), ne_binary()) ->
                                   cb_context:context().
-spec maybe_delete_template(cb_context:context(), ne_binary(), ne_binary(), wh_json:object()) ->
                                   cb_context:context().
maybe_delete_template(Context, Id, ContentType) ->
    maybe_delete_template(Context, Id, ContentType, cb_context:doc(Context)).

maybe_delete_template(Context, Id, ContentType, TemplateJObj) ->
    AttachmentName = attachment_name_by_media_type(ContentType),
    case wh_doc:attachment(TemplateJObj, AttachmentName) of
        'undefined' ->
            lager:debug("failed to find attachment ~s", [AttachmentName]),
            cb_context:add_system_error(
              'bad_identifier'
              ,wh_json:from_list([{<<"cause">>, ContentType}])
              , Context
             );
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

-spec acceptable_content_types(cb_context:context()) -> wh_proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec maybe_read(cb_context:context(), ne_binary()) -> cb_context:context().
-spec maybe_read(cb_context:context(), ne_binary(), wh_proplist(), media_values()) -> cb_context:context().
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

-spec is_acceptable_accept(wh_proplist(), ne_binary(), ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:any(fun({T, S}) ->
                      T =:= Type andalso S =:= SubType
              end, Acceptable).

-type load_from() :: 'system' | 'account' | 'system_migrate'.

-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
-spec read(cb_context:context(), ne_binary(), load_from()) -> cb_context:context().
read(Context, Id) ->
    read(Context, Id, 'system').

read(Context, Id, LoadFrom) ->
    Context1 =
        case cb_context:account_db(Context) of
            'undefined' when LoadFrom =:= 'system'; LoadFrom =:= 'system_migrate' ->
                lager:debug("no account id, loading ~s from system", [Id]),
                read_system(Context, Id);
            _AccountDb ->
                lager:debug("reading ~s from account first", [Id]),
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
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    crossbar_doc:load(Id, cb_context:set_account_db(Context, MasterAccountDb)).

-spec read_account(cb_context:context(), ne_binary(), load_from()) -> cb_context:context().
read_account(Context, Id, LoadFrom) ->
    Context1 = crossbar_doc:load(Id, Context),
    case {cb_context:resp_error_code(Context1)
          ,cb_context:resp_status(Context1)
         }
    of
        {404, 'error'} when LoadFrom =:= 'system'; LoadFrom =:= 'system_migrate' ->
            lager:debug("~s not found in account, reading from master", [Id]),
            read_system_for_account(Context, Id, LoadFrom);
        {_Code, 'success'} ->
            lager:debug("loaded ~s from account database", [Id]),
            cb_context:set_resp_data(Context1
                                     ,note_account_override(cb_context:resp_data(Context1))
                                    );
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
    lager:debug("saving template ~s from master to account ~s", [Id, cb_context:account_id(Context)]),

    Template = cb_context:fetch(Context, 'db_doc'),

    Context1 =
        crossbar_doc:ensure_saved(
          cb_context:set_doc(Context
                             ,kz_notification:set_base_properties(
                                wh_doc:public_fields(Template)
                                ,Id
                               )
                            )
         ),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("saved template ~s to account ~s", [Id, cb_context:account_db(Context1)]),
            Context2 = migrate_template_attachments(Context1, Id, wh_doc:attachments(Template)),
            maybe_note_notification_preference(Context2),
            Context2;
        _Status -> Context1
    end.

-spec maybe_hard_delete(cb_context:context(), ne_binary()) -> 'ok'.
maybe_hard_delete(Context, Id) ->
    case couch_mgr:del_doc(cb_context:account_db(Context), Id) of
        {'ok', _} ->
            couch_mgr:flush_cache_doc(cb_context:account_db(Context), Id),
            lager:debug("hard-deleted old version of ~s from ~s", [Id, cb_context:account_db(Context)]);
        {'error', 'not_found'} ->
            couch_mgr:flush_cache_doc(cb_context:account_db(Context), Id),
            lager:debug("~s wasn't found in ~s", [Id, cb_context:account_db(Context)]);
        {'error', _E} ->
            lager:debug("error deleting ~s from ~s: ~p", [Id, cb_context:account_db(Context), _E])
    end.

-spec maybe_note_notification_preference(cb_context:context()) -> 'ok'.
-spec maybe_note_notification_preference(ne_binary(), wh_json:object()) -> 'ok'.
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

-spec note_notification_preference(ne_binary(), wh_json:object()) -> 'ok'.
note_notification_preference(AccountDb, AccountJObj) ->
    case couch_mgr:save_doc(AccountDb
                            ,kz_account:set_notification_preference(AccountJObj
                                                                    ,<<"teletype">>
                                                                   )
                           )
    of
        {'ok', _AccountJObj} ->
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
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    wh_json:foldl(fun(AName, AMeta, C) ->
                          lager:debug("migrate attachment ~s", [AName]),
                          migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, C)
                  end, Context, Attachments).

-spec migrate_template_attachment(ne_binary(), ne_binary(), ne_binary(), wh_json:object(), cb_context:context()) ->
                                         cb_context:context().
migrate_template_attachment(MasterAccountDb, Id, AName, AMeta, Context) ->
    case couch_mgr:fetch_attachment(MasterAccountDb, Id, AName) of
        {'ok', Bin} ->
            ContentType = wh_json:get_value(<<"content_type">>, AMeta),
            lager:debug("saving attachment for ~s(~s): ~s", [Id, AName, ContentType]),
            crossbar_doc:save_attachment(Id
                                         ,attachment_name_by_content_type(ContentType)
                                         ,Bin
                                         ,Context
                                         ,[{'headers'
                                            ,[{'content_type', wh_util:to_list(ContentType)}]
                                           }
                                          ]
                                        );
        {'error', _E} ->
            lager:debug("failed to load attachment ~s for ~s: ~p", [AName, Id, _E]),
            Context
    end.

-spec note_account_override(wh_json:object()) -> wh_json:object().
note_account_override(JObj) ->
    wh_json:set_value(<<"account_overridden">>, 'true', JObj).

-spec read_success(cb_context:context()) -> cb_context:context().
read_success(Context) ->
    cb_context:setters(Context
                       ,[fun leak_attachments/1
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
    case wh_doc:attachment(Doc, AttachmentName) of
        'undefined' ->
            lager:debug("failed to find attachment ~s in ~s", [AttachmentName, Id]),
            crossbar_util:response_faulty_request(Context);
        _Meta ->
            lager:debug("found attachment ~s in ~s", [AttachmentName, Id]),

            cb_context:add_resp_headers(
              read_account_attachment(Context, Id, AttachmentName)
              ,[{<<"Content-Disposition">>, attachment_filename(Id, Accept)}
                ,{<<"Content-Type">>, wh_doc:attachment_content_type(Doc, AttachmentName)}
                ,{<<"Content-Length">>, wh_doc:attachment_length(Doc, AttachmentName)}
               ])
    end.

-spec attachment_filename(ne_binary(), ne_binary()) -> iolist().
attachment_filename(Id, Accept) ->
    [<<"attachment; filename=">>
     ,kz_notification:resp_id(Id)
     ,$., cb_modules_util:content_type_to_extension(Accept)
    ].

-spec read_system_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_system_attachment(Context, DocId, Name) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    crossbar_doc:load_attachment(DocId, Name, cb_context:set_account_db(Context, MasterAccountDb)).

-spec read_account_attachment(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_account_attachment(Context, DocId, Name) ->
    Context1 = crossbar_doc:load_attachment(DocId, Name, Context),
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

-spec update_template(cb_context:context(), path_token(), wh_json:object()) ->
                             cb_context:context().
update_template(Context, Id, FileJObj) ->
    Contents = wh_json:get_value(<<"contents">>, FileJObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    lager:debug("file content type for ~s: ~s", [Id, CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],

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
    <<"template.", (cow_qs:urlencode(CT))/binary>>.

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
        {'ok', Available} ->
            crossbar_doc:handle_json_success(Available, Context);
        {'error', 'not_found'} ->
            fetch_summary_available(Context)
    end.

-spec fetch_summary_available(cb_context:context()) -> cb_context:context().
fetch_summary_available(Context) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    Context1 =
        crossbar_doc:load_view(?CB_LIST
                               ,[]
                               ,cb_context:set_account_db(Context, MasterAccountDb)
                               ,select_normalize_fun(Context)
                              ),
    cache_available(Context1),
    Context1.

-spec cache_available(cb_context:context()) -> 'ok'.
cache_available(Context) ->
    wh_cache:store_local(?CROSSBAR_CACHE
                         ,{?MODULE, 'available'}
                         ,cb_context:doc(Context)
                         ,[{'origin', [{'db', cb_context:account_db(Context), kz_notification:pvt_type()}]}]
                        ).

-spec flush() -> non_neg_integer().
flush() ->
    wh_cache:filter_erase_local(?CROSSBAR_CACHE
                                ,fun is_cache_key/2
                               ).

-spec is_cache_key(_, _) -> boolean().
is_cache_key({?MODULE, 'available'}, _) -> 'true';
is_cache_key(_K, _V) -> 'false'.

-spec fetch_available() -> {'ok', wh_json:objects()} |
                           {'error', 'not_found'}.
fetch_available() ->
    wh_cache:fetch_local(?CROSSBAR_CACHE, {?MODULE, 'available'}).

-spec summary_account(cb_context:context()) -> cb_context:context().
-spec summary_account(cb_context:context(), wh_json:objects()) -> cb_context:context().
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
    Context1 = summary_available(Context),
    Available = filter_available(Context1),
    lager:debug("loaded system available"),

    crossbar_doc:handle_json_success(
      merge_available(AccountAvailable, Available)
      ,Context
     ).

-spec filter_available(cb_context:context()) -> wh_json:objects().
filter_available(Context) ->
    [A || A <- cb_context:doc(Context),
          crossbar_doc:filtered_doc_by_qs(wh_json:from_list([{<<"doc">>, A}])
                                          ,'true'
                                          ,Context
                                         )
    ].

-spec merge_available(wh_json:objects(), wh_json:objects()) ->
                             wh_json:objects().
merge_available([], Available) ->
    lager:debug("account has not overridden any, using system notifications"),
    Available;
merge_available(AccountAvailable, Available) ->
    lists:foldl(fun merge_fold/2, Available, AccountAvailable).

-spec merge_fold(wh_json:object(), wh_json:objects()) -> wh_json:objects().
merge_fold(Overridden, Acc) ->
    Id = wh_json:get_value(<<"id">>, Overridden),
    lager:debug("noting ~s is overridden in account", [Id]),
    [note_account_override(Overridden)
     | [JObj || JObj <- Acc,
                wh_json:get_value(<<"id">>, JObj) =/= Id
       ]
    ].

-type normalize_fun() :: fun((wh_json:object(), wh_json:objects()) -> wh_json:objects()).

-spec select_normalize_fun(cb_context:context()) -> normalize_fun().
select_normalize_fun(Context) ->
    Account = cb_context:auth_account_id(Context),
    case wh_util:is_system_admin(Account) of
        'true' -> fun normalize_available_admin/2;
        'false' -> fun(JObj, Acc) -> normalize_available_non_admin(JObj, Acc, Context) end
    end.

-spec normalize_available_admin(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_available_admin(JObj, Acc) ->
    Value = wh_json:get_value(<<"value">>, JObj),
    case kz_notification:category(Value) of
        <<"skel">> -> Acc;
        _Category -> [Value | Acc]
    end.

-spec normalize_available_non_admin(wh_json:object(), wh_json:objects(), cb_context:context()) ->
                                           wh_json:objects().
normalize_available_non_admin(JObj, Acc, Context) ->
    Value = wh_json:get_value(<<"value">>, JObj),
    case kz_notification:category(Value) of
        <<"system">> -> Acc;
        <<"skel">> -> Acc;
        <<"port">> -> normalize_available_port(Value, Acc, Context);
        _Category -> [Value | Acc]
    end.

-spec normalize_available_port(wh_json:object(), wh_json:objects(), cb_context:context()) ->
                                      wh_json:objects().
normalize_available_port(Value, Acc, Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),

    case wh_services:is_reseller(AuthAccountId)
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

-spec master_notification_doc(ne_binary()) -> {'ok', wh_json:object()} |
                                              {'error', _}.
master_notification_doc(DocId) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    couch_mgr:open_cache_doc(MasterAccountDb, DocId).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    ReqTemplate = clean_req_doc(cb_context:doc(Context)),

    DocId = kz_notification:db_id(ReqTemplate),

    case master_notification_doc(DocId) of
        {'ok', _JObj} ->
            Doc = kz_notification:set_base_properties(ReqTemplate, DocId),
            cb_context:set_doc(Context, Doc);
        {'error', 'not_found'} ->
            handle_missing_master_notification(Context, DocId, ReqTemplate);
        {'error', _E} ->
            lager:debug("error fetching ~s from master account: ~p", [DocId, _E]),
            crossbar_util:response_db_fatal(Context)
    end;
on_successful_validation(Id, Context) ->
    ReqTemplate = clean_req_doc(cb_context:doc(Context)),

    CleanedContext = cb_context:set_doc(Context, ReqTemplate),

    Context1 = crossbar_doc:load_merge(Id, CleanedContext),

    case {cb_context:resp_status(Context1)
          ,cb_context:resp_error_code(Context1)
         }
    of
        {'error', 404} ->
            lager:debug("load/merge of ~s failed with a 404", [Id]),
            handle_missing_account_notification(CleanedContext, Id, cb_context:req_nouns(CleanedContext));
        {'success', _} -> Context1;
        {_Status, _Code} ->
            lager:debug("load/merge of ~s failed with ~p / ~p", [_Status, _Code]),
            Context1
    end.

-spec handle_missing_account_notification(cb_context:context(), ne_binary(), wh_proplist()) -> cb_context:context().
handle_missing_account_notification(Context, Id, [{<<"notifications">>, [Id, ?PREVIEW]}|_]) ->
    lager:debug("preview request, ignoring if notification ~s is missing", [Id]),
    on_successful_validation(Id, Context);
handle_missing_account_notification(Context, Id, _ReqNouns) ->
    _ = maybe_hard_delete(Context, Id),
    _Context = read_system_for_account(Context, Id, 'system_migrate'),
    on_successful_validation(Id, Context).

-spec handle_missing_master_notification(cb_context:context(), ne_binary(), wh_json:object()) ->
                                                cb_context:context().
handle_missing_master_notification(Context, DocId, ReqTemplate) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("creating master notification for ~s", [DocId]),
            create_new_notification(Context, DocId, ReqTemplate, MasterAccountId);
        MasterAccountId ->
            lager:debug("creating master notification for ~s", [DocId]),
            create_new_notification(Context, DocId, ReqTemplate, MasterAccountId);
        _AccountId ->
            lager:debug("doc ~s does not exist in the master account, not letting ~s create it"
                        ,[DocId, _AccountId]
                       ),
            crossbar_util:response_bad_identifier(kz_notification:resp_id(DocId), Context)
    end.

-spec create_new_notification(cb_context:context(), ne_binary(), wh_json:object(), ne_binary()) ->
                                     cb_context:context().
create_new_notification(Context, DocId, ReqTemplate, AccountId) ->
    lager:debug("this will create a new template in the master account"),
    Doc = kz_notification:set_base_properties(ReqTemplate, DocId),
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, Doc}
                         ,{fun cb_context:set_account_db/2, wh_util:format_account_id(AccountId, 'encoded')}
                         ,{fun cb_context:set_account_id/2, AccountId}
                        ]).

-spec clean_req_doc(wh_json:object()) -> wh_json:object().
clean_req_doc(Doc) ->
    wh_json:delete_keys([?MACROS
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
                             ,wh_json:set_value(<<"id">>
                                                ,kz_notification:resp_id(RespData)
                                                ,RespData
                                               )
                            ).

-spec leak_attachments(cb_context:context()) -> cb_context:context().
leak_attachments(Context) ->
    Attachments = wh_doc:attachments(cb_context:fetch(Context, 'db_doc'), wh_json:new()),
    Templates = wh_json:foldl(fun leak_attachments_fold/3, wh_json:new(), Attachments),
    cb_context:set_resp_data(Context
                             ,wh_json:set_value(<<"templates">>, Templates, cb_context:resp_data(Context))
                            ).

-spec leak_attachments_fold(wh_json:key(), wh_json:json_term(), wh_json:object()) -> wh_json:object().
leak_attachments_fold(_Attachment, Props, Acc) ->
    wh_json:set_value(wh_json:get_value(<<"content_type">>, Props)
                      ,wh_json:from_list([{<<"length">>, wh_json:get_integer_value(<<"length">>, Props)}])
                      ,Acc
                     ).

-spec load_smtp_log(cb_context:context()) -> cb_context:context().
load_smtp_log(Context) ->
    case cb_modules_util:range_modb_view_options(Context) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(?CB_LIST_SMTP_LOG
                                   ,['include_docs' | ViewOptions]
                                   ,Context
                                   ,fun normalize_view_results/2
                                  );
        Ctx -> Ctx
    end.

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"doc">>, JObj)|Acc].
