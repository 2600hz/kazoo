%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
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
    load_users_summary(Context);
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
    _ = provisioner_util:maybe_sync_sip_data(Context1, 'user'),
    Context2 = crossbar_doc:save(cb_modules_util:remove_plaintext_password(Context1)),
    case cb_context:resp_status(Context2) of
        'success' ->
            _ = maybe_update_devices_presence(Context2),
            Context2;
        _ -> Context2
    end.

-spec post(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
post(Context, UserId, ?PHOTO) ->
    [{_FileName, FileObj}] = cb_context:req_files(Context),
    Headers = kz_json:get_value(<<"headers">>, FileObj),
    CT = kz_json:get_value(<<"content_type">>, Headers),
    Content = kz_json:get_value(<<"contents">>, FileObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(kzd_user:type())],
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
                                                ,?TYPE_CHECK_OPTION(kzd_user:type())
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
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"User-ID">>, kz_doc:id(Doc)}
          ,{<<"Password">>, cb_context:fetch(Context, <<"req_password">>)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_new_user/1).

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of accounts, each summarized. Or a specific
%% account summary.
%% @end
%%------------------------------------------------------------------------------

-spec load_users_summary(cb_context:context()) -> cb_context:context().
load_users_summary(Context) ->
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
load_user(UserId, Context) -> crossbar_doc:load(UserId, Context, ?TYPE_CHECK_OPTION(kzd_user:type())).

%%------------------------------------------------------------------------------
%% @doc Validate an update request.
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(UserId, Context) ->
    crossbar_doc:patch_and_validate(UserId, Context, fun validate_request/2).

%%------------------------------------------------------------------------------
%% @doc Validate the request JObj passes all validation checks and add / alter
%% any required fields.
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(UserId, Context0) ->
    ReqJObj = cb_context:req_data(Context0),
    AccountId = cb_context:account_id(Context0),

    Context = cb_context:store(Context0, <<"req_password">>, kz_json:get_value(<<"password">>, ReqJObj)),

    case kzd_users:validate(AccountId, UserId, ReqJObj) of
        {'true', UserJObj} when is_binary(UserId) ->
            lager:debug("successfull validated user object update"),
            %% NOTE: We need to load the current (unmodified) user document
            %% into the cb_context KVS db_doc because billing uses that to
            %% determine what changed and charge accordingly
            cb_context:update_successfully_validated_request(load_user(UserId, Context), UserJObj);
        {'true', UserJObj} ->
            lager:debug("successfull validated user object create"),
            cb_context:update_successfully_validated_request(Context, UserJObj);
        {'validation_errors', ValidationErrors} ->
            lager:info("validation errors on user"),
            cb_context:add_doc_validation_errors(Context, ValidationErrors);
        {'system_error', Error} when is_atom(Error) ->
            lager:info("system error validating user: ~p", [Error]),
            cb_context:add_system_error(Error, Context);
        {'system_error', {Error, Message}} ->
            lager:info("system error validating user: ~p, ~p", [Error, Message]),
            cb_context:add_system_error(Error, Message, Context)
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
    RespData = kzd_user:to_vcard(JObj3),
    cb_context:set_resp_data(Context, [RespData, <<"\n">>]).

-spec set_photo(kz_json:object(), cb_context:context()) -> kz_json:object().
set_photo(JObj, Context) ->
    UserId = kz_doc:id(cb_context:doc(Context)),
    Attach = crossbar_doc:load_attachment(UserId, ?PHOTO, ?TYPE_CHECK_OPTION(kzd_user:type()), Context),
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
                                                           ,?TYPE_CHECK_OPTION(kzd_user:type())
                                                           )
                                         )
                          )
    of
        'undefined' -> JObj;
        Val -> kz_json:set_value(<<"org">>, Val, JObj)
    end.
