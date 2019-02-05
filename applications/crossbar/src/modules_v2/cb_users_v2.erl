%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Users module
%%% Handle client requests for user documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_users_v2).

-export([create_user/1
        ,user_devices/1
        ]).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate_resource/1, validate_resource/2, validate_resource/3, validate_resource/4
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2, post/3
        ,delete/2 ,delete/3
        ,patch/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"users/crossbar_listing">>).
-define(LIST_BY_HOTDESK_ID, <<"users/list_by_hotdesk_id">>).
-define(LIST_BY_PRESENCE_ID, <<"devices/listing_by_presence_id">>).

-define(VCARD, <<"vcard">>).
-define(PHOTO, <<"photo">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%% SUPPORT FOR THE DEPRECIATED CB_SIGNUPS...
-spec create_user(cb_context:context()) -> cb_context:context().
create_user(Context) ->
    Context1 = validate_request('undefined', cb_context:set_req_verb(Context, ?HTTP_PUT)),
    case cb_context:resp_status(Context1) of
        'success' -> put(Context1);
        _Status -> Context1
    end.

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.content_types_provided.users">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.users">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate_resource.users">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.users">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.users">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.users">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.users">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.patch.users">>, ?MODULE, 'patch'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_UserId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_UserId, ?PHOTO) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(_UserId, ?VCARD) ->
    [?HTTP_GET].

-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
content_types_provided(Context) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _, ?VCARD) ->
    cb_context:set_content_types_provided(Context, [{'to_binary', [{<<"text">>, <<"x-vcard">>}
                                                                  ,{<<"text">>, <<"directory">>}
                                                                  ]}
                                                   ]);
content_types_provided(Context, _, ?PHOTO) ->
    cb_context:set_content_types_provided(Context, [{'to_binary', [{<<"application">>, <<"octet-stream">>}
                                                                  ,{<<"application">>, <<"base64">>}
                                                                  ]}
                                                   ]);
content_types_provided(Context, _, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_UserId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_UserId, ?VCARD) -> 'true';
resource_exists(_UserId, ?PHOTO) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(Context) ->
    authenticate_users(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

authenticate_users(?USERS_QCALL_NOUNS(_UserId, _Number), ?HTTP_GET) ->
    lager:debug("authenticating request"),
    'true';
authenticate_users(_Nouns, _Verb) -> 'false'.

-spec authorize(cb_context:context()) -> 'true'.
authorize(Context) ->
    authorize_users(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

authorize_users(?USERS_QCALL_NOUNS(_UserId, _Number), ?HTTP_GET) ->
    lager:debug("authorizing request"),
    'true';
authorize_users(_Nouns, _Verb) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with userId
%%
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) -> Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, UserId) -> validate_user_id(UserId, Context).

-spec validate_resource(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_resource(Context, UserId, _) -> validate_user_id(UserId, Context).

-spec validate_resource(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate_resource(Context, UserId, _, _) -> validate_user_id(UserId, Context).

-spec validate_user_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_user_id(UserId, Context) ->
    case kz_datamgr:open_cache_doc(cb_context:account_db(Context), UserId) of
        {'ok', Doc} -> validate_user_id(UserId, Context, Doc);
        {'error', 'not_found'} ->
            cb_context:add_system_error('bad_identifier'
                                       ,kz_json:from_list([{<<"cause">>, UserId}])
                                       ,Context
                                       );
        {'error', _R} -> crossbar_util:response_db_fatal(Context)
    end.

-spec validate_user_id(kz_term:api_binary(), cb_context:context(), kz_json:object()) -> cb_context:context().
validate_user_id(UserId, Context, Doc) ->
    case kz_doc:is_soft_deleted(Doc) of
        'true' ->
            Msg = kz_json:from_list([{<<"cause">>, UserId}]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        'false'->
            cb_context:setters(Context
                              ,[{fun cb_context:set_user_id/2, UserId}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ])
    end.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_users(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, UserId) ->
    validate_user(Context, UserId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, UserId, ?VCARD) ->
    Context1 = load_user(UserId, Context),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' -> convert_to_vcard(Context1)
    end;
validate(Context, UserId, ?PHOTO) ->
    validate_photo(Context, UserId , cb_context:req_verb(Context)).

validate_users(Context, ?HTTP_GET) ->
    load_user_summary(Context);
validate_users(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate_user(Context, UserId, ?HTTP_GET) ->
    load_user(UserId, Context);
validate_user(Context, UserId, ?HTTP_POST) ->
    validate_request(UserId, Context);
validate_user(Context, UserId, ?HTTP_DELETE) ->
    load_user(UserId, Context);
validate_user(Context, UserId, ?HTTP_PATCH) ->
    validate_patch(UserId, Context).

validate_photo(Context, UserId, ?HTTP_POST) ->
    load_user(UserId, Context);
validate_photo(Context, UserId, ?HTTP_DELETE) ->
    load_user(UserId, Context);
validate_photo(Context, UserId, ?HTTP_GET) ->
    load_attachment(UserId, ?PHOTO, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    _ = crossbar_util:maybe_refresh_fs_xml('user', Context),
    Context1 = cb_modules_util:take_sync_field(Context),
    sync_sip_data(Context1),
    Context2 = crossbar_doc:save(cb_modules_util:remove_plaintext_password(Context1)),
    case cb_context:resp_status(Context2) of
        'success' ->
            _ = maybe_update_devices_presence(Context2),
            Context2;
        _ -> Context2
    end.

-spec sync_sip_data(cb_context:context()) -> 'ok'.
sync_sip_data(Context) ->
    NewDoc = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),

    case cb_context:fetch(Context, 'sync') of
        'false' -> 'ok';
        'true' -> provisioner_util:sync_user(AccountId);
        'force' -> provisioner_util:force_sync_user(AccountId, NewDoc)
    end.

-spec post(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
post(Context, UserId, ?PHOTO) ->
    [{_FileName, FileObj}] = cb_context:req_files(Context),
    Headers = kz_json:get_value(<<"headers">>, FileObj),
    CT = kz_json:get_value(<<"content_type">>, Headers),
    Content = kz_json:get_value(<<"contents">>, FileObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(kzd_users:type())],
    crossbar_doc:save_attachment(UserId, ?PHOTO, Content, Context, Opts).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_email(Context1),
            Context1;
        _ -> Context1
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

-spec delete(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
delete(Context, UserId, ?PHOTO) ->
    crossbar_doc:delete_attachment(UserId, ?PHOTO, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec load_attachment(kz_term:ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(AttachmentId, Context) ->
    Headers =
        #{<<"content-disposition">> => <<"attachment; filename=", AttachmentId/binary>>
         ,<<"content-type">> => kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)
         },
    LoadedContext = crossbar_doc:load_attachment(cb_context:doc(Context)
                                                ,AttachmentId
                                                ,?TYPE_CHECK_OPTION(kzd_users:type())
                                                ,Context
                                                ),
    cb_context:add_resp_headers(LoadedContext, Headers).

-spec load_attachment(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(UserId, AttachmentId, Context) ->
    Context1 = load_user(UserId, Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_attachment(AttachmentId, Context1);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_devices_presence(cb_context:context()) -> 'ok'.
maybe_update_devices_presence(Context) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    Doc = cb_context:doc(Context),
    case kzd_devices:presence_id(DbDoc) =:= kzd_devices:presence_id(Doc) of
        'true' ->
            lager:debug("presence_id did not change, ignoring");
        'false' ->
            update_devices_presence(Context)
    end.

-spec update_devices_presence(cb_context:context()) -> 'ok'.
update_devices_presence(Context) ->
    case user_devices(Context) of
        {'error', _R} ->
            lager:error("failed to query view ~s: ~p", [?LIST_BY_PRESENCE_ID, _R]);
        {'ok', []} ->
            lager:debug("no presence IDs found for user");
        {'ok', DeviceDocs} ->
            update_devices_presence(Context, DeviceDocs)
    end.

-spec update_devices_presence(cb_context:context(), kzd_devices:docs()) -> 'ok'.
update_devices_presence(Context, DeviceDocs) ->
    lists:foreach(fun(DeviceDoc) -> update_device_presence(Context, DeviceDoc) end
                 ,DeviceDocs
                 ).

-spec user_devices(cb_context:context()) ->
                          {'ok', kzd_devices:docs()} |
                          {'error', any()}.
user_devices(Context) ->
    UserId = kz_doc:id(cb_context:doc(Context)),
    AccountDb = cb_context:account_db(Context),

    Options = [{'key', UserId}, 'include_docs'],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_PRESENCE_ID, Options) of
        {'error', _}=E -> E;
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]}
    end.

-spec update_device_presence(cb_context:context(), kzd_devices:doc()) -> pid().
update_device_presence(Context, DeviceDoc) ->
    AuthToken = cb_context:auth_token(Context),
    ReqId = cb_context:req_id(Context),

    lager:debug("re-provisioning device ~s", [kz_doc:id(DeviceDoc)]),

    kz_util:spawn(fun() ->
                          kz_util:put_callid(ReqId),
                          provisioner_v5:update_device(DeviceDoc, AuthToken)
                  end).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_email(cb_context:context()) -> 'ok'.
maybe_send_email(Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"send_email_on_creation">>, 'true')) of
        'false' -> 'ok';
        'true' -> send_email(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_email(cb_context:context()) -> 'ok'.
send_email(Context) ->
    lager:debug("trying to publish new user notification"),
    Doc = cb_context:doc(Context),
    ReqData = cb_context:req_data(Context),
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"User-ID">>, kz_doc:id(Doc)}
          ,{<<"Password">>, kz_json:get_value(<<"password">>, ReqData)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_new_user/1).

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_user_summary(cb_context:context()) -> cb_context:context().
load_user_summary(Context) ->
    fix_envelope(
      crossbar_doc:load_view(?CB_LIST
                            ,[]
                            ,Context
                            ,fun normalize_view_results/2
                            )).

-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    RespData = cb_context:resp_data(Context),
    cb_context:set_resp_data(Context, lists:reverse(RespData)).

%%------------------------------------------------------------------------------
%% @doc Load a user document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_user(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
load_user(UserId, Context) -> crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(UserId, Context) ->
    Context1 = prepare_username(UserId, Context),
    check_hotdesk_id(UserId, Context1).

-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(UserId, Context) ->
    crossbar_doc:patch_and_validate(UserId, Context, fun validate_request/2).

-spec prepare_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
prepare_username(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    case kz_json:get_ne_value(<<"username">>, JObj) of
        'undefined' -> check_user_name(UserId, Context);
        Username ->
            JObj1 = kz_json:set_value(<<"username">>, kz_term:to_lower_binary(Username), JObj),
            check_user_name(UserId, cb_context:set_req_data(Context, JObj1))
    end.

-spec check_user_name(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_user_name(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    UserName = kz_json:get_ne_value(<<"username">>, JObj),
    AccountDb = cb_context:account_db(Context),
    case is_username_unique(AccountDb, UserId, UserName) of
        'true' ->
            lager:debug("username ~p is unique", [UserName]),
            check_emergency_caller_id(UserId, Context);
        'false' ->
            Context1 = non_unique_username_error(Context, UserName),
            lager:error("username ~p is already used", [UserName]),
            check_emergency_caller_id(UserId, Context1)
    end.

-spec non_unique_username_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
non_unique_username_error(Context, Username) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"User name is not unique for this account">>}
            ,{<<"cause">>, Username}
            ]),
    cb_context:add_validation_error([<<"username">>], <<"unique">>, Msg, Context).

-spec check_emergency_caller_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_emergency_caller_id(UserId, Context) ->
    Context1 = crossbar_util:format_emergency_caller_id_number(Context),
    check_user_schema(UserId, Context1).

-spec is_username_unique(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_username_unique(AccountDb, UserId, UserName) ->
    ViewOptions = [{'key', UserName}],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= UserId;
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?LIST_BY_USERNAME, AccountDb]),
            'false'
    end.

-spec check_user_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_user_schema(UserId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(UserId, C) end,
    cb_context:validate_request_data(<<"users">>, Context, OnSuccess).

-spec check_hotdesk_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_hotdesk_id(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    HotdeskId = kz_json:get_ne_value([<<"hotdesk">>, <<"id">>], JObj),
    AccountDb = cb_context:account_db(Context),
    case is_hotdesk_id_unique(AccountDb, UserId, HotdeskId) of
        'true' ->
            Context;
        'false' ->
            lager:debug("hotdesk.id ~p is already used", [HotdeskId]),
            non_unique_hotdesk_id_error(Context, HotdeskId);
        {'error', DatamgrError} ->
            crossbar_doc:handle_datamgr_errors(DatamgrError, HotdeskId, Context)
    end.

-spec is_hotdesk_id_unique(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean() | kz_datamgr:data_error().
is_hotdesk_id_unique(AccountDb, UserId, HotdeskId) ->
    ViewOptions = [{'key', HotdeskId}],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_HOTDESK_ID, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= UserId;
        Else ->
            lager:debug("error ~p checking view ~p in ~p", [Else, ?LIST_BY_HOTDESK_ID, AccountDb]),
            Else
    end.

-spec non_unique_hotdesk_id_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
non_unique_hotdesk_id_error(Context, HotdeskId) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Hotdesk.id is not unique for this account">>}
            ,{<<"cause">>, HotdeskId}
            ]),
    cb_context:add_validation_error([<<"hotdesk">>, <<"id">>], <<"unique">>, Msg, Context).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_users:type()}],
    maybe_import_credintials('undefined'
                            ,cb_context:set_doc(Context
                                               ,kz_json:set_values(Props, cb_context:doc(Context))
                                               )
                            );
on_successful_validation(UserId, Context) ->
    maybe_import_credintials(UserId, crossbar_doc:load_merge(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type()))).

-spec maybe_import_credintials(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_import_credintials(UserId, Context) ->
    JObj = cb_context:doc(Context),
    case kz_json:get_ne_value(<<"credentials">>, JObj) of
        'undefined' -> maybe_validate_username(UserId, Context);
        Creds ->
            RemoveKeys = [<<"credentials">>, <<"pvt_sha1_auth">>],
            C = cb_context:set_doc(Context
                                  ,kz_json:set_value(<<"pvt_md5_auth">>, Creds
                                                    ,kz_json:delete_keys(RemoveKeys, JObj)
                                                    )
                                  ),
            maybe_validate_username(UserId, C)
    end.

-spec maybe_validate_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_validate_username(UserId, Context) ->
    JObj = cb_context:doc(Context),
    NewUsername = kz_json:get_ne_value(<<"username">>, JObj),
    CurrentUsername =
        case cb_context:fetch(Context, 'db_doc') of
            'undefined' -> NewUsername;
            CurrentJObj ->
                kz_json:get_ne_value(<<"username">>, CurrentJObj, NewUsername)
        end,
    case kz_term:is_empty(NewUsername)
        orelse CurrentUsername =:= NewUsername
        orelse username_doc_id(NewUsername, Context)
    of
        %% username is unchanged
        'true' -> maybe_rehash_creds(UserId, NewUsername, Context);
        %% updated username that doesn't exist
        'undefined' ->
            manditory_rehash_creds(UserId, NewUsername, Context);
        %% updated username to existing, collect any further errors...
        _Else ->
            Context1 = non_unique_username_error(Context, NewUsername),
            manditory_rehash_creds(UserId, NewUsername, Context1)
    end.

-spec maybe_rehash_creds(kz_term:api_binary(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_rehash_creds(UserId, Username, Context) ->
    case kz_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        %% No user name or hash, no creds for you!
        'undefined' when Username =:= 'undefined' ->
            HashKeys = [<<"pvt_md5_auth">>, <<"pvt_sha1_auth">>],
            cb_context:set_doc(Context, kz_json:delete_keys(HashKeys, cb_context:doc(Context)));
        %% User name without password, creds status quo
        'undefined' -> Context;
        %% Got a password, hope you also have a user name...
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec manditory_rehash_creds(kz_term:api_binary(), kz_term:api_binary(), cb_context:context()) ->
                                    cb_context:context().
manditory_rehash_creds(UserId, Username, Context) ->
    case kz_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        'undefined' ->
            required_password_error(Context);
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec required_password_error(cb_context:context()) -> cb_context:context().
required_password_error(Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"The password must be provided when updating the user name">>}
            ]),
    cb_context:add_validation_error(<<"password">>, <<"required">>, Msg, Context).

-spec rehash_creds(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) ->
                          cb_context:context().
rehash_creds(_UserId, 'undefined', _Password, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"The user name must be provided when updating the password">>}
            ]),
    cb_context:add_validation_error(<<"username">>, <<"required">>, Msg, Context);
rehash_creds(_UserId, Username, Password, Context) ->
    lager:debug("password set on doc, updating hashes for ~s", [Username]),
    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),
    JObj1 = kz_json:set_values([{<<"pvt_md5_auth">>, MD5}
                               ,{<<"pvt_sha1_auth">>, SHA1}
                               ]
                              ,cb_context:doc(Context)
                              ),
    crossbar_auth:reset_identity_secret(
      cb_context:set_doc(Context, kz_json:delete_key(<<"password">>, JObj1))
     ).

%%------------------------------------------------------------------------------
%% @doc This function will determine if the user name in the request is
%% unique or belongs to the request being made
%% @end
%%------------------------------------------------------------------------------
-spec username_doc_id(kz_term:api_binary(), cb_context:context()) -> kz_term:api_binary().
username_doc_id(Username, Context) ->
    username_doc_id(Username, Context, cb_context:account_db(Context)).
username_doc_id(_, _, 'undefined') -> 'undefined';
username_doc_id(Username, Context, _AccountDb) ->
    Username = kz_term:to_lower_binary(Username),
    Context1 = crossbar_doc:load_view(?LIST_BY_USERNAME, [{'key', Username}], Context),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso cb_context:doc(Context1)
    of
        [JObj] -> kz_doc:id(JObj);
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec(normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects()).
normalize_view_results(JObj, Acc) -> [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Converts context to vcard
%% @end
%%------------------------------------------------------------------------------
-spec convert_to_vcard(cb_context:context()) -> cb_context:context().
convert_to_vcard(Context) ->
    JObj = cb_context:doc(Context),
    JProfile = kz_json:get_value(<<"profile">>, JObj, kz_json:new()),
    JObj1 = kz_json:merge_jobjs(JObj, JProfile),
    JObj2 = set_photo(JObj1, Context),
    JObj3 = set_org(JObj2, Context),
    RespData = kzd_users:to_vcard(JObj3),
    cb_context:set_resp_data(Context, [RespData, <<"\n">>]).

-spec set_photo(kz_json:object(), cb_context:context()) -> kz_json:object().
set_photo(JObj, Context) ->
    UserId = kz_doc:id(cb_context:doc(Context)),
    Attach = crossbar_doc:load_attachment(UserId, ?PHOTO, ?TYPE_CHECK_OPTION(kzd_users:type()), Context),
    case cb_context:resp_status(Attach) of
        'error' -> JObj;
        'success' ->
            Data = cb_context:resp_data(Attach),
            CT = kz_doc:attachment_content_type(cb_context:doc(Context), ?PHOTO),
            kz_json:set_value(?PHOTO, kz_json:from_list([{CT, Data}]), JObj)
    end.

-spec set_org(kz_json:object(), cb_context:context()) -> kz_json:object().
set_org(JObj, Context) ->
    case kz_json:get_value(<<"org">>
                          ,cb_context:doc(crossbar_doc:load(cb_context:account_id(Context)
                                                           ,Context
                                                           ,?TYPE_CHECK_OPTION(kzd_users:type())
                                                           )
                                         )
                          )
    of
        'undefined' -> JObj;
        Val -> kz_json:set_value(<<"org">>, Val, JObj)
    end.
