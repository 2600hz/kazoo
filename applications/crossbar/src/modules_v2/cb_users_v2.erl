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

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".users">>).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_email(Context1),
            Context1;
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

-spec delete(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
delete(Context, UserId, ?PHOTO) ->
    crossbar_doc:delete_attachment(UserId, ?PHOTO, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

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

-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(UserId, Context) ->
    crossbar_doc:patch_and_validate(UserId, Context, fun validate_request/2).

-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(UserId, Context) ->
    Routines = [fun normalize_username/2
               ,fun normalize_emergency_caller_id/2
               ,fun maybe_import_credintials/2
                %% check_user_schema will load and merge the current doc as well
               ,fun check_user_schema/2
                %% this check must have the current doc
               ,fun maybe_set_identity_secret/2
                %% this check must have the current doc
               ,fun check_username/2
                %% this check must have the current doc
               ,fun check_hotdesk_id/2
                %% this check must have the current doc
               ,fun maybe_rehash_creds/2
               ],
    lists:foldl(fun(F, C) ->
                        F(UserId, C)
                end
               ,Context
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec normalize_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
normalize_username(_UserId, Context) ->
    JObj = cb_context:req_data(Context),
    case kzd_users:username(JObj) of
        'undefined' -> Context;
        Username ->
            NormalizedUsername = kz_term:to_lower_binary(Username),
            cb_context:set_req_data(Context, kzd_users:set_username(JObj, NormalizedUsername))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec normalize_emergency_caller_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
normalize_emergency_caller_id(_UserId, Context) ->
    crossbar_util:format_emergency_caller_id_number(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_import_credintials(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_import_credintials(_UserId, Context) ->
    JObj = cb_context:doc(Context),
    case kz_json:get_ne_value(<<"credentials">>, JObj) of
        'undefined' -> Context;
        Creds ->
            RemoveKeys = [<<"credentials">>, <<"pvt_sha1_auth">>],
            UpdatedJObj =
                kz_json:set_value(<<"pvt_md5_auth">>
                                 ,Creds
                                 ,kz_json:delete_keys(RemoveKeys, JObj)
                                 ),
            cb_context:set_doc(Context, UpdatedJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_set_identity_secret(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_set_identity_secret(_UserId, Context) ->
    case crossbar_auth:has_identity_secret(Context) of
        'true' -> Context;
        'false' ->
            lager:debug("initalizing identity secret"),
            crossbar_auth:reset_identity_secret(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec check_username(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_username(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    Username = kzd_users:username(JObj),
    CurrentJObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
    CurrentUsername = kzd_users:username(CurrentJObj),
    AccountDb = cb_context:account_db(Context),
    case kz_term:is_empty(Username)
        orelse Username =:= CurrentUsername
        orelse is_username_unique(AccountDb, UserId, Username)
    of
        'true' ->
            lager:debug("username ~s (currently ~s) is unique"
                       ,[Username, CurrentUsername]
                       ),
            Context;
        'false' ->
            lager:error("username ~s (currently ~s) is already used"
                       ,[Username, CurrentUsername]
                       ),
            non_unique_username_error(Context, Username)
    end.

-spec is_username_unique(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_username_unique(AccountDb, UserId, UserName) ->
    ViewOptions = [{'key', UserName}],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_USERNAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= UserId;
        {'error', _R} ->
            lager:error("error checking view ~p in ~p"
                       ,[?LIST_BY_USERNAME, AccountDb, _R]
                       ),
            'false'
    end.

-spec non_unique_username_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
non_unique_username_error(Context, Username) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"User name is not unique for this account">>}
            ,{<<"cause">>, Username}
            ]),
    cb_context:add_validation_error([<<"username">>], <<"unique">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec check_hotdesk_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_hotdesk_id(UserId, Context) ->
    JObj = cb_context:req_data(Context),
    HotdeskId = kzd_users:hotdesk_id(JObj),
    CurrentJObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
    CurrentHotdeskId = kzd_users:hotdesk_id(CurrentJObj),
    AccountDb = cb_context:account_db(Context),
    case kz_term:is_empty(HotdeskId)
        orelse HotdeskId =:= CurrentHotdeskId
        orelse is_hotdesk_id_unique(AccountDb, UserId, HotdeskId)
    of
        'true' ->
            lager:debug("hotdesk ID ~s (currently ~s) is unique"
                       ,[HotdeskId, CurrentHotdeskId]
                       ),
            Context;
        'false' ->
            lager:debug("hotdesk ID ~s (currently ~s) is already used"
                       ,[HotdeskId, CurrentHotdeskId]
                       ),
            non_unique_hotdesk_id_error(Context, HotdeskId)
    end.

-spec is_hotdesk_id_unique(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean() | kz_datamgr:data_error().
is_hotdesk_id_unique(AccountDb, UserId, HotdeskId) ->
    ViewOptions = [{'key', HotdeskId}],
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_HOTDESK_ID, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= UserId;
        {'error', _R} ->
            lager:error("error checking view ~p in ~p: ~p"
                       ,[?LIST_BY_HOTDESK_ID, AccountDb, _R]
                       ),
            'false'
    end.

-spec non_unique_hotdesk_id_error(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
non_unique_hotdesk_id_error(Context, HotdeskId) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Hotdesk ID is not unique for this account">>}
            ,{<<"cause">>, HotdeskId}
            ]),
    cb_context:add_validation_error([<<"hotdesk">>, <<"id">>], <<"unique">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec check_user_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_user_schema(UserId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(UserId, C) end,
    cb_context:validate_request_data(<<"users">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_users:type()}],
    JObj = kz_json:set_values(Props, cb_context:doc(Context)),
    maybe_import_credintials('undefined', cb_context:set_doc(Context, JObj));
on_successful_validation(UserId, Context) ->
    maybe_import_credintials(UserId, crossbar_doc:load_merge(UserId, Context, ?TYPE_CHECK_OPTION(kzd_users:type()))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_rehash_creds(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
maybe_rehash_creds(_UserId, Context) ->
    JObj = cb_context:doc(Context),
    Username = kzd_users:username(JObj),
    CurrentJObj = cb_context:fetch(Context, 'db_doc', kz_json:new()),
    CurrentUsername = kzd_users:username(CurrentJObj),
    Password = kz_json:get_ne_binary_value(<<"password">>, JObj),
    GeneratePassword = kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"generate_password_if_empty">>, 'false'),
    GenerateUsername = kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"generate_username_if_empty">>, 'true'),
    GenerateCreds =
        GenerateUsername
        andalso GeneratePassword,
    case
        {Username =:= CurrentUsername
        ,kz_term:is_empty(Username)
        ,kz_term:is_empty(Password)}
    of
        {'false', 'false', 'false'} ->
            lager:debug("requested different username (new: ~s current: ~s) with a password"
                       ,[Username, CurrentUsername]
                       ),
            rehash_creds(Username, Password, Context);
        {'false', 'false', 'true'} ->
            lager:debug("requested different username (new: ~s current: ~s) without a password"
                       ,[Username, CurrentUsername]
                       ),
            maybe_generated_password_hash(GeneratePassword, Username, Context);
        {'false', 'true', 'false'} ->
            lager:debug("requested no username but provided a password"),
            maybe_generated_username_hash(GenerateUsername, Password, Context);
        {'false', 'true', 'true'} ->
            lager:debug("requested no username or password"),
            maybe_generated_creds_hash(GenerateCreds, Context);
        {'true', 'false', 'false'} ->
            lager:debug("requested same username (new: ~s current: ~s) with a password"
                       ,[Username, CurrentUsername]
                       ),
            rehash_creds(Username, Password, Context);
        {'true', 'false', 'true'} ->
            lager:debug("requested same username (new: ~s current: ~s) without a password"
                       ,[Username, CurrentUsername]
                       ),
            Context;
        {'true', 'true', 'false'} ->
            lager:debug("requested no username (new: ~s current: ~s) with a password"
                       ,[Username, CurrentUsername]
                       ),
            maybe_generated_username_hash(GenerateUsername, Password, Context);
        {'true', 'true', 'true'} ->
            lager:debug("requested no username, no current username, and no password"),
            maybe_generated_creds_hash(GenerateCreds, Context)
    end.

-spec maybe_generated_password_hash(boolean(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
maybe_generated_password_hash('false', _Username, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"The password must be provided when updating the user name">>}
            ]),
    cb_context:add_validation_error(<<"password">>, <<"required">>, Msg, Context);
maybe_generated_password_hash('true', Username, Context) ->
    rehash_creds(Username, generate_password(), Context).

-spec maybe_generated_username_hash(boolean(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
maybe_generated_username_hash('false', _Password, Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"The username must be provided when updating the password">>}
            ]),
    cb_context:add_validation_error(<<"username">>, <<"required">>, Msg, Context);
maybe_generated_username_hash('true', Password, Context) ->
    Username = generate_username(),
    JObj = kzd_users:set_username(Username, cb_context:doc(Context)),
    rehash_creds(Username, Password, cb_context:set_doc(Context, JObj)).

-spec maybe_generated_creds_hash(boolean(), cb_context:context()) -> cb_context:context().
maybe_generated_creds_hash('false', Context) ->
    remove_creds(Context);
maybe_generated_creds_hash('true', Context) ->
    Username = generate_username(),
    JObj = kzd_users:set_username(Username, cb_context:doc(Context)),
    rehash_creds(Username, generate_password(), cb_context:set_doc(Context, JObj)).

-spec generate_username() -> kz_term:ne_binary().
generate_username() ->
    lager:debug("generating random username"),
    <<"user_", (kz_binary:rand_hex(8))/binary>>.

-spec generate_password() -> kz_term:ne_binary().
generate_password() ->
    lager:debug("generating random password"),
    kz_binary:rand_hex(32).

-spec remove_creds(cb_context:context()) -> cb_context:context().
remove_creds(Context) ->
    lager:debug("removing user creds"),
    HashKeys = [<<"pvt_md5_auth">>, <<"pvt_sha1_auth">>],
    cb_context:set_doc(Context, kz_json:delete_keys(HashKeys, cb_context:doc(Context))).

-spec rehash_creds(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) ->
                          cb_context:context().
rehash_creds(Username, Password, Context) ->
    lager:debug("updating cred hashes for ~s", [Username]),
    CurrentJObj = cb_context:doc(Context),
    CurrentMD5 = kz_json:get_ne_value(<<"pvt_md5_auth">>, CurrentJObj),
    CurrentSHA1 = kz_json:get_ne_value(<<"pvt_sha1_auth">>, CurrentJObj),
    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),
    JObj = kz_json:set_values([{<<"pvt_md5_auth">>, MD5}
                              ,{<<"pvt_sha1_auth">>, SHA1}
                              ]
                             ,CurrentJObj
                             ),
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"reset_identity_secret_on_rehash">>, 'true')
        andalso (CurrentMD5 =/= MD5
                 orelse CurrentSHA1 =/= SHA1)
    of
        'false' ->
            cb_context:set_doc(Context, kz_json:delete_key(<<"password">>, JObj));
        'true' ->
            lager:debug("resetting identity secret", []),
            crossbar_auth:reset_identity_secret(
              cb_context:set_doc(Context, kz_json:delete_key(<<"password">>, JObj))
             )
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
