%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc VMBoxes module
%%% Handle client requests for vmbox documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Hesaam Farhang
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_vmboxes).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
        ,validate/1, validate/2, validate/3, validate/4, validate/5
        ,content_types_accepted/3, content_types_accepted/4, content_types_accepted/5
        ,content_types_provided/3, content_types_provided/4, content_types_provided/5
        ,put/1, put/3, put/5
        ,post/2, post/3, post/4
        ,patch/2
        ,delete/2, delete/3, delete/4

        ,migrate/1
        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(CB_LIST, <<"vmboxes/crossbar_listing">>).
-define(MSG_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).

-define(BOX_ID_KEY_INDEX, 2).

-define(MESSAGES_RESOURCE, ?VM_KEY_MESSAGES).
-define(BIN_DATA, <<"raw">>).

-define(UPLOAD_MIME_TYPES, [{<<"application">>, <<"octet-stream">>, '*'}
                            | ?AUDIO_CONTENT_TYPES
                            ++ ?MULTIPART_CONTENT_TYPES
                            ++ ?JSON_CONTENT_TYPES
                           ]).
-define(DOWNLOAD_MIME_TYPES, ?UPLOAD_MIME_TYPES -- ?JSON_CONTENT_TYPES).
-define(BULK_DOWNLOAD_MIME_TYPE, [{<<"application">>, <<"zip">>, '*'}]).
-define(ALL_MIME_TYPES, ?UPLOAD_MIME_TYPES ++ ?BULK_DOWNLOAD_MIME_TYPE).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".voicemail">>).
-define(NORMALIZATION_FORMAT, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"normalization_format">>, <<"mp3">>)).
-define(TRANSCRIBE_FORMAT, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"transcribe_format">>)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.vmboxes">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.vmboxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.vmboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.vmboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.vmboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.vmboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.vmboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.vmboxes">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.vmboxes">>, ?MODULE, 'delete'),

    sync_config_to_schema().

-spec sync_config_to_schema() -> 'ok'.
sync_config_to_schema() ->
    %% [{ConfigPath, SchemaPath, CastFun}]
    SyncFields = [{[<<"voicemail">>, <<"max_pin_length">>]
                  ,[<<"properties">>, <<"pin">>, <<"maxLength">>]
                  ,fun(ConfigV, SchemaV) -> kz_term:safe_cast(ConfigV, SchemaV, fun kz_term:to_integer/1) end
                  }
                 ],
    lists:foreach(fun sync_field/1, SyncFields).

-spec sync_field({kz_json:get_key(), kz_json:get_key(), fun((kz_json:json_term(), kz_json:json_term()) -> kz_json:json_term())}) -> 'ok'.
sync_field({ConfigPath, SchemaPath, CastFun}) ->
    {'ok', SchemaJObj} = kz_json_schema:load(<<"vmboxes">>),
    SchemaV = kz_json:get_value(SchemaPath, SchemaJObj),

    case kapps_config:get(<<"callflow">>, ConfigPath) of
        'undefined' ->
            lager:debug("config ~s undefined, no schema change necessary", [kz_binary:join(ConfigPath, <<".">>)]);
        ConfigV ->
            case CastFun(ConfigV, SchemaV) of
                SchemaV -> lager:debug("config ~s unchanged from schema", [kz_binary:join(ConfigPath, <<".">>)]);
                ConfigValue ->
                    lager:info("config ~s(~p) differs from schema ~s(~p), updating schema"
                              ,[kz_binary:join(ConfigPath, <<".">>), ConfigValue
                               ,kz_binary:join(SchemaPath, <<".">>), SchemaV
                               ]
                              ),
                    UpdatedSchema = kz_json:set_value(SchemaPath, ConfigValue, SchemaJObj),
                    {'ok', _} = kz_datamgr:save_doc(?KZ_SCHEMA_DB, UpdatedSchema),
                    lager:info("saved vmboxes schema")
            end
    end.

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
allowed_methods(?MESSAGES_RESOURCE) ->
    [?HTTP_GET];
allowed_methods(_VMBoxId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    [?HTTP_POST];
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, _VMMsgId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, _VMMsgId, ?BIN_DATA) ->
    [?HTTP_GET, ?HTTP_PUT].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE, _) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE, _, ?BIN_DATA) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted by this module
%% @end
%%------------------------------------------------------------------------------
-spec acceptable_content_types() -> cowboy_content_types().
acceptable_content_types() -> ?ALL_MIME_TYPES.

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, _VMBox, ?MESSAGES_RESOURCE) ->
    maybe_add_types_accepted(Context, ?MESSAGES_RESOURCE, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, _VMBox, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    maybe_add_types_accepted(Context, ?BIN_DATA, cb_context:req_verb(Context));
content_types_accepted(Context, _VMBox, ?MESSAGES_RESOURCE, _) -> Context.

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, _VMBox, ?MESSAGES_RESOURCE, _MsgId, ?BIN_DATA) ->
    maybe_add_types_accepted(Context, ?BIN_DATA, cb_context:req_verb(Context)).

-spec maybe_add_types_accepted(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
maybe_add_types_accepted(Context, _, ?HTTP_PUT) ->
    CTA = [{'from_binary', ?UPLOAD_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
maybe_add_types_accepted(Context, ?BIN_DATA, ?HTTP_POST) ->
    CTA = [{'from_binary', ?BULK_DOWNLOAD_MIME_TYPE}],
    cb_context:set_content_types_accepted(Context, CTA);
maybe_add_types_accepted(Context, ?BIN_DATA, ?HTTP_GET) ->
    CTA = [{'from_binary', ?DOWNLOAD_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
maybe_add_types_accepted(Context, _, _) -> Context.

%%------------------------------------------------------------------------------
%% @doc Add content types provided by this module
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE) ->
    maybe_add_types_provided(Context, ?MESSAGES_RESOURCE, cb_context:req_verb(Context)).

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    maybe_add_types_provided(Context, ?BIN_DATA, cb_context:req_verb(Context));
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, _) -> Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    maybe_add_types_provided(Context, ?BIN_DATA, cb_context:req_verb(Context)).

-spec maybe_add_types_provided(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
maybe_add_types_provided(Context, _, ?HTTP_PUT) -> Context;
maybe_add_types_provided(Context, ?BIN_DATA, ?HTTP_POST) ->
    CTP = [{'send_file', ?BULK_DOWNLOAD_MIME_TYPE}],
    cb_context:set_content_types_provided(Context, CTP);
maybe_add_types_provided(Context, ?BIN_DATA, ?HTTP_GET) ->
    CTP = [{'to_binary', ?DOWNLOAD_MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP);
maybe_add_types_provided(Context, _, _) -> Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_vmboxes(Context, cb_context:req_verb(Context)).

validate_vmboxes(Context, ?HTTP_GET) ->
    load_vmbox_summary(Context);
validate_vmboxes(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?MESSAGES_RESOURCE) ->
    load_message_summary('undefined', Context);
validate(Context, BoxId) ->
    validate_vmbox(Context, BoxId, cb_context:req_verb(Context)).

validate_vmbox(Context, BoxId, ?HTTP_GET) ->
    load_vmbox(BoxId, Context);
validate_vmbox(Context, BoxId, ?HTTP_POST) ->
    validate_request(BoxId, Context);
validate_vmbox(Context, BoxId, ?HTTP_PATCH) ->
    validate_patch(load_vmbox(BoxId, Context), BoxId);
validate_vmbox(Context, BoxId, ?HTTP_DELETE) ->
    load_vmbox(BoxId, Context).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, BoxId, ?MESSAGES_RESOURCE) ->
    validate_messages(Context, BoxId, cb_context:req_verb(Context)).

-spec validate_messages(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_messages(Context, BoxId, ?HTTP_GET) ->
    load_message_summary(BoxId, Context);
validate_messages(Context, BoxId, ?HTTP_POST) ->
    case kz_json:get_list_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []) of
        [] ->
            Message = kz_json:from_list([{<<"message">>, <<"No array of message ids are specified">>}]),
            cb_context:add_validation_error(<<"messages">>, <<"required">>, Message, Context);
        _ ->
            NewBoxId = kz_json:get_list_value(<<"source_id">>, cb_context:req_data(Context)),
            maybe_load_vmboxes([BoxId | NewBoxId], Context)
    end;
validate_messages(Context, BoxId, ?HTTP_PUT) ->
    C1 = validate_new_message(BoxId, Context),
    case cb_context:resp_status(C1) of
        'success' ->
            validate_media_binary(C1, cb_context:req_files(Context), 'false');
        _ -> C1
    end;
validate_messages(Context, BoxId, ?HTTP_DELETE) ->
    Messages = kvm_messages:get(cb_context:account_id(Context), BoxId),

    Filter = kz_json:get_list_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), get_folder_filter(Context, <<"all">>)),
    ToDelete = filter_messages(Messages, Filter, Context),
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success'), ToDelete).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, BoxId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    load_messages_binaries(BoxId, Context);
validate(Context, BoxId, ?MESSAGES_RESOURCE, MediaId) ->
    validate_message(Context, BoxId, MediaId, cb_context:req_verb(Context)).

-spec validate_message(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_message(Context, BoxId, MessageId, ?HTTP_GET) ->
    load_message(MessageId, BoxId, Context, 'true');
validate_message(Context, BoxId, MessageId, ?HTTP_POST) ->
    RetentionTimestamp = kz_time:now_s() - kvm_util:retention_seconds(cb_context:account_id(Context)),
    case kvm_message:fetch(cb_context:account_id(Context), MessageId, BoxId) of
        {'ok', Msg} ->
            case kzd_box_message:utc_seconds(Msg) < RetentionTimestamp of
                'true' ->
                    ErrMsg = <<"cannot make changes to messages prior to retention duration">>,
                    cb_context:add_validation_error(MessageId
                                                   ,<<"date_range">>
                                                   ,kz_json:from_list(
                                                      [{<<"message">>, ErrMsg}
                                                      ,{<<"cause">>, RetentionTimestamp}
                                                      ])
                                                   ,Context
                                                   );
                'false' ->
                    NewBoxId = kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)),
                    maybe_load_vmboxes(NewBoxId, Context)
            end;
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MessageId, Context)
    end;
validate_message(Context, BoxId, MessageId, ?HTTP_DELETE) ->
    load_message(MessageId, BoxId, Context).

-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, BoxId, ?MESSAGES_RESOURCE, MediaId, ?BIN_DATA) ->
    load_or_upload(BoxId, MediaId, Context, cb_context:req_verb(Context)).

-spec load_or_upload(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context(), http_method()) -> cb_context:context().
load_or_upload(BoxId, MediaId, Context, ?HTTP_PUT) ->
    C1 = load_message(MediaId, BoxId, Context),
    case cb_context:resp_status(C1) of
        'success' ->
            validate_media_binary(C1, cb_context:req_files(Context), 'true');
        _ -> C1
    end;
load_or_upload(BoxId, MediaId, Context, ?HTTP_GET) ->
    load_message_binary(BoxId, MediaId, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _BoxId) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    VMBoxMsgs = kz_json:get_list_value(?VM_KEY_MESSAGES, DbDoc),
    C1 = crossbar_doc:save(check_mailbox_for_messages_array(Context, VMBoxMsgs)),

    %% remove messages array to not let it exposed
    crossbar_util:response(kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1)), C1).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, OldBoxId, ?MESSAGES_RESOURCE) ->
    AccountId = cb_context:account_id(Context),
    MsgIds = kz_json:get_list_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []),
    Folder = get_folder_filter(Context, ?VM_FOLDER_SAVED),

    case kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)) of
        'undefined' ->
            Result = kvm_messages:change_folder(Folder, MsgIds, AccountId, OldBoxId, add_pvt_auth_funs(Context)),
            C = crossbar_util:response(Result, Context),
            update_mwi(C, OldBoxId);
        ?NE_BINARY = NewBoxId ->
            Moved = kvm_messages:move_to_vmbox(AccountId, MsgIds, OldBoxId, NewBoxId, add_pvt_auth_funs(Context)),
            C = crossbar_util:response(Moved, Context),
            update_mwi(C, [OldBoxId, NewBoxId]);
        NewBoxIds ->
            Copied = kvm_messages:copy_to_vmboxes(AccountId, MsgIds, OldBoxId, NewBoxIds, add_pvt_auth_funs(Context)),
            C = crossbar_util:response(Copied, Context),
            update_mwi(C, [OldBoxId | NewBoxIds])
    end.

-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, _BoxId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    Context;
post(Context, OldBoxId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)) of
        'undefined' ->
            Folder = get_folder_filter(Context, ?VM_FOLDER_SAVED),
            case kvm_message:change_folder(Folder, MediaId, AccountId, OldBoxId, add_pvt_auth_funs(Context)) of
                {'ok', Message} ->
                    C = crossbar_util:response(Message, Context),
                    update_mwi(C, OldBoxId);
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
            end;
        ?NE_BINARY = NewBoxId ->
            case kvm_message:move_to_vmbox(AccountId, MediaId, OldBoxId, NewBoxId, add_pvt_auth_funs(Context)) of
                {'ok', Moved} ->
                    C = crossbar_util:response(Moved, Context),
                    update_mwi(C, [OldBoxId, NewBoxId]);
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
            end;
        NewBoxIds ->
            Copied = kvm_message:copy_to_vmboxes(AccountId, MediaId, OldBoxId, NewBoxIds, add_pvt_auth_funs(Context)),
            C = crossbar_util:response(Copied, Context),
            update_mwi(C, [OldBoxId | NewBoxIds])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, _BoxId, ?MESSAGES_RESOURCE) ->
    maybe_save_attachment(crossbar_doc:save(Context)).

-spec put(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
put(Context, _BoxId, ?MESSAGES_RESOURCE, MsgID, ?BIN_DATA) ->
    maybe_save_attachment(cb_context:set_db_name(Context, kvm_util:get_db(cb_context:account_id(Context), MsgID))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, BoxId) ->
    AccountId = cb_context:account_id(Context),
    Msgs = kvm_messages:get(AccountId, BoxId),
    MsgIds = [kzd_box_message:media_id(M) || M <- Msgs],
    _ = kvm_messages:change_folder(?VM_FOLDER_DELETED, MsgIds, AccountId, BoxId, add_pvt_auth_funs(Context)),
    C = crossbar_doc:delete(Context),
    update_mwi(C, BoxId).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, BoxId, ?MESSAGES_RESOURCE) ->
    AccountId = cb_context:account_id(Context),
    MsgIds = cb_context:resp_data(Context),
    Result = kvm_messages:change_folder({?VM_FOLDER_DELETED, 'true'}, MsgIds, AccountId, BoxId, add_pvt_auth_funs(Context)),
    C = crossbar_util:response(Result, Context),
    update_mwi(C, BoxId).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, BoxId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case kvm_message:change_folder({?VM_FOLDER_DELETED, 'true'}, MediaId, AccountId, BoxId, add_pvt_auth_funs(Context)) of
        {'ok', Message} ->
            C = crossbar_util:response(Message, Context),
            update_mwi(C, BoxId);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _Id) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    VMBoxMsgs = kz_json:get_list_value(?VM_KEY_MESSAGES, DbDoc),
    C1 = crossbar_doc:save(check_mailbox_for_messages_array(Context, VMBoxMsgs)),

    %% remove messages array to not let it exposed
    crossbar_util:response(kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1)), C1).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_pvt_auth_funs(cb_context:context()) -> [fun((kz_json:object()) -> kz_json:object())].
add_pvt_auth_funs(Context) ->
    [fun(JObj) -> add_pvt_auth(JObj, Context) end].

-spec add_pvt_auth(kz_json:object(), cb_context:context()) -> kz_json:object().
add_pvt_auth(JObj, Context) ->
    AuthUpdates = crossbar_doc:add_pvt_auth(JObj, [], Context),
    kz_json:set_values(AuthUpdates, JObj).

%%------------------------------------------------------------------------------
%% @doc disallow vmbox messages array changing.
%% Also check if vmbox has still message array
%% and inform client to do migrate their vmbox
%% @end
%%------------------------------------------------------------------------------
-spec check_mailbox_for_messages_array(cb_context:context(), kz_term:api_objects()) -> cb_context:context().
check_mailbox_for_messages_array(Context, 'undefined') -> Context;
check_mailbox_for_messages_array(Context, []) -> Context;
check_mailbox_for_messages_array(Context, VMBoxMsgs) ->
    Props = [{?VM_KEY_MESSAGES, VMBoxMsgs}],
    NewDoc = kz_json:set_values(Props, cb_context:doc(Context)),
    Envelope = kz_json:from_list([{<<"message">>
                                  ,<<"Please migrate your voicemail box messages to MODB">>
                                  }
                                 ]),
    Setters = [{fun cb_context:set_doc/2, NewDoc}
              ,{fun cb_context:set_resp_envelope/2, Envelope}
              ],
    cb_context:setters(Context, Setters).

-spec maybe_save_attachment(cb_context:context()) -> cb_context:context().
maybe_save_attachment(Context) ->
    maybe_save_attachment(crossbar_util:maybe_remove_attachments(Context), cb_context:req_files(Context)).

-spec maybe_save_attachment(cb_context:context(), req_files()) -> cb_context:context().
maybe_save_attachment(Context, []) -> Context;
maybe_save_attachment(Context, [{Filename, FileJObj} | _Others]) ->
    save_attachment(Context, Filename, FileJObj).

-spec save_attachment(cb_context:context(), binary(), kz_json:object()) -> cb_context:context().
save_attachment(Context, Filename, FileJObj) ->
    JObj = cb_context:doc(Context),
    BoxId = kz_doc:id(JObj),
    Contents = kz_json:get_ne_binary_value(<<"contents">>, FileJObj),
    CT = kz_json:get_ne_binary_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT}
           ,{'rev', kz_doc:revision(JObj)}
            | ?TYPE_CHECK_OPTION(<<"mailbox_message">>)
           ],
    AttName = cb_modules_util:attachment_name(Filename, CT),
    C1 = crossbar_doc:save_attachment(BoxId, AttName, Contents, Context, Opts),
    case cb_context:resp_status(C1) of
        'success' ->
            C2 = crossbar_util:response(kzd_box_message:metadata(JObj), C1),
            update_mwi(C2, kzd_box_message:source_id(JObj));
        _ -> C1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_folder_filter(cb_context:context(), kz_term:api_ne_binary()) -> kvm_message:vm_folder() | 'undefined'.
get_folder_filter(Context, Default) ->
    ReqData = cb_context:req_data(Context),
    QS = cb_context:query_string(Context),
    ReqJObj = cb_context:req_json(Context),

    case kz_json:find(?VM_KEY_FOLDER, [ReqData, QS, ReqJObj]) of
        ?VM_FOLDER_NEW -> ?VM_FOLDER_NEW;
        ?VM_FOLDER_SAVED -> ?VM_FOLDER_SAVED;
        ?VM_FOLDER_DELETED -> {?VM_FOLDER_DELETED, 'false'};
        _ -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc Filter messages(JObjs) based on Folder or Ids and
%% apply query strings filters on them as well.
%%
%% Note: Filter can be `<<"all">>' which return all messages.
%% @end
%%------------------------------------------------------------------------------
-type filter_options() :: kvm_message:vm_folder() | kz_term:api_ne_binaries().

-spec filter_messages(kz_json:objects(), filter_options(), cb_context:context()) -> kz_term:ne_binaries().
filter_messages(_, 'undefined', _) ->
    [];
filter_messages(Messages, {?VM_FOLDER_DELETED, _}, Context) ->
    %% move to delete folder and soft-delete
    filter_messages(Messages, ?VM_FOLDER_DELETED, Context);
filter_messages(Messages, Filters, Context) ->
    filter_messages(Messages, Filters, Context, [], crossbar_filter:is_defined(Context)).

%% Filter by folder

-spec filter_messages(kz_json:objects(), filter_options(), cb_context:context(), kz_term:ne_binaries(), boolean()) -> kz_term:ne_binaries().
filter_messages([], _Filters, _Context, Selected, _) ->
    Selected;
filter_messages([Mess|Messages], <<"all">> = Filter, Context, Selected, HasQSFilter) ->
    filter_messages(Messages, Filter, Context, [kzd_box_message:media_id(Mess)|Selected], HasQSFilter);
filter_messages([Mess|Messages], <<_/binary>> = Filter, Context, Selected, HasQSFilter) when Filter =:= ?VM_FOLDER_NEW;
                                                                                             Filter =:= ?VM_FOLDER_SAVED;
                                                                                             Filter =:= ?VM_FOLDER_DELETED ->
    case (HasQSFilter
          andalso crossbar_filter:by_doc(Mess, Context, HasQSFilter)
         )
        orelse kzd_box_message:folder(Mess) =:= Filter
    of
        'true' -> filter_messages(Messages, Filter, Context, [kzd_box_message:media_id(Mess)|Selected], HasQSFilter);
        'false' -> filter_messages(Messages, Filter, Context, Selected, HasQSFilter)
    end;
%% Filter by Ids
filter_messages(_, [], _Context, Selected, _) -> Selected;
filter_messages([Mess|Messages], Filters, Context, Selected, HasQSFilter) ->
    Id = kzd_box_message:media_id(Mess),

    case (HasQSFilter
          andalso crossbar_filter:by_doc(Mess, Context, HasQSFilter)
         )
        orelse lists:member(Id, Filters)
    of
        'true' -> filter_messages(Messages, Filters, Context, [Id|Selected], HasQSFilter);
        'false' -> filter_messages(Messages, Filters, Context, Selected, HasQSFilter)
    end.

-spec validate_media_binary(cb_context:context(), kz_term:proplist(), boolean()) -> cb_context:context().
validate_media_binary(Context, [], 'false') -> Context;
validate_media_binary(Context, [], 'true') ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide a media file">>}])
                                   ,Context
                                   );
validate_media_binary(Context, [{_Filename, FileObj}], _) ->
    maybe_normalize_upload(Context, FileObj);
validate_media_binary(Context, _MultiFiles, _) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide a single media file">>}])
                                   ,Context
                                   ).

-spec maybe_normalize_upload(cb_context:context(), kz_json:object()) -> cb_context:context().
maybe_normalize_upload(Context, FileJObj) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"normalize_media">>, 'false') of
        'true' ->
            lager:debug("normalizing uploaded media"),
            normalize_upload(Context, FileJObj);
        'false' ->
            lager:debug("normalization not enabled, leaving upload as-is"),
            Context
    end.

-spec normalize_upload(cb_context:context(), kz_json:object()) -> cb_context:context().
normalize_upload(Context, FileJObj) ->
    normalize_upload(Context, FileJObj, kz_json:get_ne_binary_value([<<"headers">>, <<"content_type">>], FileJObj)).

-spec normalize_upload(cb_context:context(), kz_json:object(), kz_term:api_binary()) -> cb_context:context().
normalize_upload(Context, FileJObj, UploadContentType) ->
    FromExt = kz_mime:to_extension(UploadContentType),
    ToExt =  normalization_format(Context),

    lager:info("upload is of type '~s', normalizing from ~s to ~s"
              ,[UploadContentType, FromExt, ToExt]
              ),

    {UpdatedContext, _UpdatedFileJObj} =
        cb_modules_util:normalize_media_upload(Context, FromExt, ToExt, FileJObj, [{'to_args', <<"-r 16000">>}]),
    UpdatedContext.

-spec normalization_format(cb_context:context()) -> kz_term:ne_binary().
normalization_format(Context) ->
    BoxId = kzd_box_message:source_id(cb_context:doc(Context)),
    AccountDb = kzs_util:format_account_db(cb_context:account_id(Context)),
    case kz_datamgr:open_cache_doc(AccountDb, BoxId) of
        {'ok', BoxJObj} -> kzd_voicemail_box:media_extension(BoxJObj);
        {'error', _} -> ?NORMALIZATION_FORMAT
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_unique_vmbox(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_unique_vmbox(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context, cb_context:db_name(Context)).

-spec validate_unique_vmbox(kz_term:api_binary(), cb_context:context(), kz_term:api_ne_binary()) -> cb_context:context().
validate_unique_vmbox(VMBoxId, Context, 'undefined') ->
    check_vmbox_schema(VMBoxId, Context);
validate_unique_vmbox(VMBoxId, Context, _AccountDb) ->
    case check_uniqueness(VMBoxId, Context) of
        'true' -> check_vmbox_schema(VMBoxId, Context);
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"Invalid mailbox number or already exists">>}]),
            C = cb_context:add_validation_error(<<"mailbox">>, <<"unique">>, Msg, Context),
            check_vmbox_schema(VMBoxId, C)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_vmbox_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_vmbox_schema(VMBoxId, Context) ->
    Context1 = maybe_migrate_notification_emails(Context),
    OnSuccess = fun(C) -> validate_media_extension(VMBoxId, C) end,
    cb_context:validate_request_data(<<"vmboxes">>, Context1, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_notification_emails(cb_context:context()) -> cb_context:context().
maybe_migrate_notification_emails(Context) ->
    ReqData = cb_context:req_data(Context),
    case maybe_migrate_vm_box(ReqData) of
        {'true', ReqData1} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_req_data/2, ReqData1}
                               ,{fun cb_context:add_resp_header/3
                                ,<<"Warning">>
                                ,<<"214 Transformation applied: attribute notify_email_address replaced">>
                                }
                               ]
                              );
        'false' -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_media_extension(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_media_extension(VMBoxId, Context) ->
    MediaExt = ?TRANSCRIBE_FORMAT,
    case MediaExt /= 'undefined'
        andalso cb_context:req_value(Context, <<"transcribe">>, 'false')
        andalso cb_context:req_value(Context, <<"media_extension">>) /= MediaExt of
        'true' ->
            Resp = [{<<"message">>, <<"if you want transcribe message, then use '", MediaExt/binary, "' media extension">>}],
            cb_context:add_validation_error(<<"media_extension">>, <<"invalid">>, kz_json:from_list(Resp), Context);
        'false' -> on_successful_validation(VMBoxId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) ->
          cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_voicemail_box:type()}],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(VMBoxId, Context) ->
    crossbar_doc:load_merge(VMBoxId, Context, ?TYPE_CHECK_OPTION(kzd_voicemail_box:type())).

%%------------------------------------------------------------------------------
%% @doc Support PATCH - merge vmbox document with request data
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_patch(Context, BoxId)->
    crossbar_doc:patch_and_validate(BoxId, Context, fun validate_request/2).

%%------------------------------------------------------------------------------
%% @doc Attempt to load list of vmboxes, each summarized.
%% @end
%%------------------------------------------------------------------------------
-spec load_vmbox_summary(cb_context:context()) -> cb_context:context().
load_vmbox_summary(Context) ->
    Context1 = crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2),
    CountMap = kvm_messages:count(cb_context:account_id(Context)),
    RspData = add_counts_to_summary_results(cb_context:doc(Context1), CountMap),
    cb_context:set_resp_data(cb_context:set_doc(Context1, RspData), RspData).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj)|Acc].

-spec add_counts_to_summary_results(kz_json:objects(), map()) -> kz_json:objects().
add_counts_to_summary_results(BoxSummaries, CountMap) ->
    MergeFun = fun(BoxSummary, Acc) ->
                       [merge_summary_fold(BoxSummary, CountMap) | Acc]
               end,
    lists:foldl(MergeFun, [], BoxSummaries).

-spec merge_summary_fold(kz_json:object(), map()) -> kz_json:object().
merge_summary_fold(BoxSummary, CountMap) ->
    BoxId = kz_json:get_ne_binary_value(<<"id">>, BoxSummary),
    case maps:get(BoxId, CountMap, 'undefined') of
        'undefined' ->
            BoxSummary;
        BoxCountMap ->
            MsgsArrayCount = kz_json:get_integer_value(?VM_KEY_MESSAGES, BoxSummary, 0),
            New = maps:get(?VM_FOLDER_NEW, BoxCountMap, 0),
            Saved = maps:get(?VM_FOLDER_SAVED, BoxCountMap, 0),
            kz_json:set_value(?VM_KEY_MESSAGES, MsgsArrayCount + New + Saved, BoxSummary)
    end.

%%------------------------------------------------------------------------------
%% @doc Load a vmbox document from the database
%% @end
%%------------------------------------------------------------------------------
-type source_id() :: kz_term:api_binary() | kz_term:api_binaries().

-spec maybe_load_vmboxes(source_id(), cb_context:context()) -> cb_context:context().
maybe_load_vmboxes('undefined', Context) -> cb_context:set_resp_status(Context, 'success');
maybe_load_vmboxes(?NE_BINARY = Id, Context) -> load_vmbox(Id, Context);
maybe_load_vmboxes(<<>>, Context) -> empty_source_id(Context);
maybe_load_vmboxes([], Context) -> empty_source_id(Context);
maybe_load_vmboxes(Ids, Context) -> maybe_load_vmboxes_fold(Ids, Context).

-spec maybe_load_vmboxes_fold(source_id(), cb_context:context()) -> cb_context:context().
maybe_load_vmboxes_fold('undefined', Context) -> Context;
maybe_load_vmboxes_fold(?NE_BINARY = Id, Context) -> load_vmbox(Id, Context);
maybe_load_vmboxes_fold(<<>>, Context) -> empty_source_id(Context);
maybe_load_vmboxes_fold([], Context) -> Context;
maybe_load_vmboxes_fold([<<>>|_], Context) -> empty_source_id(Context);
maybe_load_vmboxes_fold(['undefined'|Ids], Context) ->
    maybe_load_vmboxes_fold(Ids, Context);
maybe_load_vmboxes_fold([Id|Ids], Context) ->
    C1 = load_vmbox(Id, Context),
    case cb_context:resp_status(C1) of
        'success' ->
            maybe_load_vmboxes_fold(Ids, C1);
        _ ->
            C1
    end.

-spec load_vmbox(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_vmbox(BoxId, Context) ->
    C1 = crossbar_doc:load(BoxId, Context, ?TYPE_CHECK_OPTION(kzd_voicemail_box:type())),
    cb_context:set_resp_data(C1, kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1))).

-spec empty_source_id(cb_context:context()) -> cb_context:context().
empty_source_id(Context) ->
    Message = kz_json:from_list([{<<"message">>, <<"empty source_id is specified. please set a single or a list of source_ids.">>}]),
    cb_context:add_validation_error(<<"source_id">>, <<"required">>, Message, Context).

%%------------------------------------------------------------------------------
%% @doc Get messages summary for a given mailbox
%% @end
%%------------------------------------------------------------------------------
-spec load_message_summary(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
load_message_summary('undefined', Context) ->
    Options = [{'range_start_keymap', []}
              ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
              ],
    load_message_summary('undefined', Context, <<"mailbox_messages/listing_by_timestamp">>, Options);
load_message_summary(BoxId, Context) ->
    load_message_summary(BoxId, Context, ?MSG_LISTING_BY_MAILBOX, [{'range_keymap', [BoxId]}]).

-spec load_message_summary(kz_term:api_binary(), cb_context:context(), kz_term:ne_binary(), crossbar_view:options()) -> cb_context:context().
load_message_summary(BoxId, Context, ViewName, Options) ->
    {MaxRange, RetentionSeconds} = get_max_range(Context),
    RetentionTimestamp = kz_time:now_s() - RetentionSeconds,

    Mapper = fun(JObj, Acc) -> message_summary_normalizer(BoxId, JObj, Acc, RetentionTimestamp) end,

    ViewOptions = [{'mapper', Mapper}
                  ,{'max_range', MaxRange}
                   | Options
                  ],
    crossbar_view:load_modb(prefix_qs_filter_keys(Context), ViewName, ViewOptions).

-spec prefix_qs_filter_keys(cb_context:context()) -> cb_context:context().
prefix_qs_filter_keys(Context) ->
    NewQs = kz_json:map(fun(K, V) -> prefix_filter_key(K, V) end
                       ,cb_context:query_string(Context)
                       ),
    cb_context:set_query_string(Context, NewQs).

-spec prefix_filter_key(kz_term:ne_binary(), any()) -> {kz_term:ne_binary(), any()}.
prefix_filter_key(<<"filter_not_", Key/binary>>, Val) ->
    {<<"filter_not_metadata.", Key/binary>>, Val};
prefix_filter_key(<<"filter_", Key/binary>>, Val) ->
    {<<"filter_metadata.", Key/binary>>, Val};
prefix_filter_key(<<"has_key">>, Key) ->
    {<<"has_key">>, <<"metadata.", Key/binary>>};
prefix_filter_key(<<"key_missing">>, Key) ->
    {<<"key_missing">>, <<"metadata.", Key/binary>>};
prefix_filter_key(<<"has_value">>, Key) ->
    {<<"has_value">>, <<"metadata.", Key/binary>>};
prefix_filter_key(Key, Value) ->
    {Key, Value}.

-spec message_summary_normalizer(kz_term:api_binary(), kz_json:object(), kz_json:objects(), kz_time:gregorian_seconds()) ->
          kz_json:objects().
message_summary_normalizer('undefined', JObj, Acc, RetentionTimestamp) ->
    [kz_json:from_list(
       [{kz_json:get_value([<<"key">>, ?BOX_ID_KEY_INDEX], JObj)
        ,kvm_util:enforce_retention(kz_json:get_json_value(<<"value">>, JObj), RetentionTimestamp)
        }
       ])
     | Acc
    ];
message_summary_normalizer(_BoxId, JObj, Acc, RetentionTimestamp) ->
    [kvm_util:enforce_retention(kz_json:get_json_value(<<"value">>, JObj), RetentionTimestamp)
     | Acc
    ].

%%------------------------------------------------------------------------------
%% @doc For Backward compatibility with voicemail box `messages' array, set
%% `max_range' to retention seconds to get all messages which are in
%% retention duration range *only* if request doesn't have
%% `created_from' in the query string.
%%
%% If `created_from' is set, use default Crossbar max range.
%% @end
%%------------------------------------------------------------------------------
-spec get_max_range(cb_context:context()) -> {kz_time:api_seconds(), kz_time:gregorian_seconds()}.
get_max_range(Context) ->
    RetentionSeconds = kvm_util:retention_seconds(cb_context:account_id(Context)),
    case cb_context:req_value(Context, <<"created_from">>) of
        'undefined' -> {RetentionSeconds, RetentionSeconds};
        _ -> {'undefined', RetentionSeconds}
    end.

%%------------------------------------------------------------------------------
%% @doc Get message by its media ID and its context
%% @end
%%------------------------------------------------------------------------------
-spec load_message(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_message(MessageId, BoxId, Context) ->
    load_message(MessageId, BoxId, Context, 'false').

-spec load_message(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context(), boolean()) -> cb_context:context().
load_message(MessageId, BoxId, Context, ReturnMetadata) ->
    case kvm_message:fetch(cb_context:account_id(Context), MessageId, BoxId) of
        {'ok', Msg} when ReturnMetadata ->
            crossbar_doc:handle_json_success(kzd_box_message:metadata(Msg), Context);
        {'ok', Msg} ->
            crossbar_doc:handle_json_success(Msg, Context);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MessageId, Context)
    end.

-spec validate_new_message(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_new_message(BoxId, Context) ->
    C1 = load_vmbox(BoxId, Context),
    case cb_context:resp_status(C1) of
        'success' ->
            BoxJObj = cb_context:doc(C1),
            OnSuccess = fun(C) -> create_new_message_document(C, BoxJObj) end,
            C2 = cb_context:store(Context, 'vmbox', BoxJObj),
            cb_context:validate_request_data(<<"vm_message_metadata">>, C2, OnSuccess);
        _ -> C1
    end.

-spec create_new_message_document(cb_context:context(), kz_json:object()) -> cb_context:context().
create_new_message_document(Context, BoxJObj) ->
    AccountId = cb_context:account_id(Context),
    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),

    Doc = cb_context:doc(Context),

    Props = [{<<"Message-Timestamp">>, kz_json:get_integer_value(<<"timestamp">>, Doc)}
            ,{<<"Box-Num">>, BoxNum}
            ,{<<"Timezone">>, kzd_voicemail_box:timezone(BoxJObj)}
            ,{<<"Box-Id">>, kz_doc:id(BoxJObj)}
            ],
    JObj = kzd_box_message:new(AccountId, Props),
    MsgId = kz_doc:id(JObj),

    AccountRealm = case kzd_accounts:fetch_realm(AccountId) of 'undefined' -> <<"nodomain">>; R -> R end,
    DefaultCID = kz_privacy:anonymous_caller_id_number(AccountId),
    CallerNumber = kz_json:get_ne_binary_value(<<"caller_id_number">>, Doc, DefaultCID),
    CallerName = kz_json:get_ne_binary_value(<<"caller_id_name">>, Doc, DefaultCID),
    From = kz_json:get_ne_binary_value(<<"from">>, Doc, CallerNumber),
    To = kz_json:get_ne_binary_value(<<"to">>, Doc, BoxNum),
    Length = kz_json:get_integer_value(<<"length">>, Doc, 1),

    Timestamp = kz_doc:created(JObj),

    Routines = [{fun kapps_call:set_to/2, <<To/binary, "@", AccountRealm/binary>>}
               ,{fun kapps_call:set_from/2, <<From/binary, "@", AccountRealm/binary>>}
               ,{fun kapps_call:set_call_id/2, kz_json:get_ne_binary_value(<<"call_id">>, Doc, kz_binary:rand_hex(12))}
               ,{fun kapps_call:set_caller_id_number/2, CallerNumber}
               ,{fun kapps_call:set_caller_id_name/2, CallerName}
               ],
    Call = kapps_call:exec(Routines, kapps_call:new()),
    Metadata = kzd_box_message:build_metadata_object(Length, Call, MsgId, CallerNumber, CallerName, Timestamp),

    Message = kzd_box_message:update_media_id(MsgId, kzd_box_message:set_metadata(Metadata, JObj)),
    cb_context:set_doc(cb_context:set_db_name(Context, kz_doc:account_db(Message)), Message).

%%------------------------------------------------------------------------------
%% @doc Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%------------------------------------------------------------------------------
-spec load_message_binary(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_message_binary(BoxId, MediaId, Context) ->
    case kvm_message:fetch(cb_context:account_id(Context), MediaId, BoxId) of
        {'ok', JObj} ->
            case kz_term:is_not_empty(kz_doc:attachment_names(JObj))
                andalso kz_datamgr:open_cache_doc(cb_context:db_name(Context), BoxId)
            of
                'false' ->
                    Msg = <<"voicemail message does not have any audio file">>,
                    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, Msg}]), Context);
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, BoxId, Context);
                {'ok', BoxJObj} ->
                    Timezone = kzd_voicemail_box:timezone(BoxJObj),
                    load_attachment_from_message(JObj, Context, Timezone)
            end;
        {'error', Err} -> crossbar_doc:handle_datamgr_errors(Err, MediaId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_attachment_from_message(kz_json:object(), cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
load_attachment_from_message(Doc, Context, Timezone) ->
    MediaId = kz_doc:id(Doc),
    VMMetaJObj = kzd_box_message:metadata(Doc),

    [AttachmentId] = kz_doc:attachment_names(Doc),
    Filename = generate_media_name(kz_json:get_value(<<"caller_id_number">>, VMMetaJObj)
                                  ,kz_json:get_value(<<"timestamp">>, VMMetaJObj)
                                  ,filename:extension(AttachmentId)
                                  ,Timezone
                                  ),
    case kz_datamgr:fetch_attachment(kz_doc:account_db(Doc), MediaId, AttachmentId) of
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MediaId, Context);
        {'ok', AttachBin} ->
            lager:debug("sending file with filename ~s", [Filename]),
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_resp_data/2, AttachBin}
                      ,{fun cb_context:set_resp_etag/2, 'undefined'}
                      ,{fun cb_context:add_resp_headers/2
                       ,#{<<"content-type">> => kz_doc:attachment_content_type(Doc, AttachmentId)
                         ,<<"content-disposition">> => <<"attachment; filename=", Filename/binary>>
                         }
                       }
                      ],
            cb_context:setters(Context, Setters)
    end.

-spec load_messages_binaries(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_messages_binaries(BoxId, Context) ->
    WorkDir = kz_term:to_list(<<"/tmp/", (cb_context:req_id(Context))/binary, "/">>),
    Ids = kz_json:get_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []),
    case Ids =/= []
        andalso kz_datamgr:open_cache_doc(cb_context:db_name(Context), BoxId)
    of
        'false' ->
            Message = kz_json:from_list([{<<"message">>, <<"No array of message ids are specified">>}]),
            cb_context:add_validation_error(<<"messages">>, <<"required">>, Message, Context);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, BoxId, Context);
        {'ok', BoxJObj} ->
            Timezone = kzd_voicemail_box:timezone(BoxJObj),
            _ = save_attachments_to_file(Ids, BoxId, Context, Timezone, WorkDir),
            maybe_create_zip_file(WorkDir, Context)
    end.

-spec save_attachments_to_file(kz_term:ne_binaries(), kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), string()) ->
          cb_context:context().
save_attachments_to_file([], _, Context, _, _) -> Context;
save_attachments_to_file([Id|Ids], BoxId, Context, Timezone, WorkDir) ->
    _ = file:make_dir(WorkDir),
    try save_attachment_to_file(Id, BoxId, Context, Timezone, WorkDir) of
        'ok' -> save_attachments_to_file(Ids, BoxId, Context, Timezone, WorkDir);
        {'error', Error} ->
            _ = del_dir(WorkDir),
            crossbar_doc:handle_datamgr_errors(Error, Id, Context)
    catch
        _:_ ->
            _ = del_dir(WorkDir),
            cb_context:add_system_error('unspecified_fault', Context)
    end.

-spec save_attachment_to_file(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), string()) ->
          'ok' | {'error', any()}.
save_attachment_to_file(MsgId, BoxId, Context, Timezone, WorkDir) ->
    case kvm_message:fetch(cb_context:account_id(Context), MsgId, BoxId) of
        {'ok', Doc} ->
            VMMetaJObj = kzd_box_message:metadata(Doc),

            [AttachmentId] = kz_doc:attachment_names(Doc),
            Filename = generate_media_name(kz_json:get_value(<<"caller_id_number">>, VMMetaJObj)
                                          ,kz_json:get_value(<<"timestamp">>, VMMetaJObj)
                                          ,filename:extension(AttachmentId)
                                          ,Timezone
                                          ),
            case kz_datamgr:fetch_attachment(kz_doc:account_db(Doc), MsgId, AttachmentId) of
                {'error', _} = E -> E;
                {'ok', AttachBin} ->
                    'ok' = file:write_file(lists:concat([WorkDir, kz_term:to_list(Filename)]), AttachBin)
            end;
        {'error', _}=E -> E
    end.

-spec maybe_create_zip_file(string(), cb_context:context()) -> cb_context:context().
maybe_create_zip_file(WorkDir, Context) ->
    Files = [kz_term:to_list(F) || F <- filelib:wildcard("*", WorkDir)],
    try Files =/= []
             andalso create_zip_file(WorkDir, Files, Context)
    of
        'false' ->
            _ = del_dir(WorkDir),
            cb_context:add_system_error('not_found', Context);
        C -> C
    catch
        _T:_E ->
            lager:debug("failed to generate a zip file of voicemail messages: ~p:~p", [_T, _E]),
            _ = del_dir(WorkDir),
            cb_context:add_system_error('unspecified_fault', Context)
    end.

-spec create_zip_file(string(), [string()], cb_context:context()) -> cb_context:context().
create_zip_file(WorkDir, Files, Context) ->
    ZipName = lists:concat([kz_term:to_list(cb_context:req_id(Context)), ".zip"]),
    ZipPath = ["/tmp/", ZipName],
    {'ok', _} = zip:zip(ZipPath , Files, [{'cwd', WorkDir}]),
    _ = del_dir(WorkDir),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_etag/2, 'undefined'}
              ,{fun cb_context:set_resp_file/2, kz_term:to_binary(ZipPath)}
              ,{fun cb_context:add_resp_headers/2
               ,#{<<"content-type">> => <<"application/zip">>
                 ,<<"content-disposition">> => <<"attachment; filename=", (kz_term:to_binary(ZipName))/binary>>
                 }
               }
              ],
    cb_context:setters(Context, Setters).

-spec del_dir(string()) -> 'ok' | {'error', any()}.
del_dir(Dir) ->
    _ = del_all_files(Dir),
    file:del_dir(Dir).

-spec del_all_files(string()) -> any().
del_all_files(Dir) ->
    {'ok', Files} = file:list_dir(Dir),
    lists:foreach(fun(F) ->
                          file:delete(Dir ++ F)
                  end, Files
                 ).

%%------------------------------------------------------------------------------
%% @doc generate a media name based on CallerID and creation date
%% CallerID_YYYY-MM-DD_HH-MM-SS.ext
%% @end
%%------------------------------------------------------------------------------
-spec generate_media_name(kz_term:api_ne_binary(), kz_time:gregorian_seconds() | kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
generate_media_name('undefined', GregorianSeconds, Ext, Timezone) ->
    generate_media_name(<<"unknown">>, GregorianSeconds, Ext, Timezone);
generate_media_name(CallerId, <<GregorianSeconds/binary>>, Ext, Timezone) ->
    generate_media_name(CallerId, kz_term:to_integer(GregorianSeconds), Ext, Timezone);
generate_media_name(CallerId, GregorianSeconds, Ext, Timezone) ->
    LocalTime = try kz_time:adjust_utc_timestamp(GregorianSeconds, Timezone) of
                    AdjustedGregorianSeconds when is_integer(AdjustedGregorianSeconds) ->
                        lager:debug("converted to TZ: ~s", [Timezone]),
                        calendar:gregorian_seconds_to_datetime(AdjustedGregorianSeconds)
                catch
                    'throw':{'error', 'unknown_tz'} ->
                        lager:debug("unknown TZ: ~p", [Timezone]),
                        calendar:gregorian_seconds_to_datetime(GregorianSeconds)
                end,
    Date = kz_time:pretty_print_datetime(LocalTime),
    list_to_binary([CallerId, "_", Date, Ext]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec check_uniqueness(kz_term:api_binary(), cb_context:context()) -> boolean().
check_uniqueness(VMBoxId, Context) ->
    try kz_json:get_integer_value(<<"mailbox">>, cb_context:req_data(Context)) of
        Mailbox ->
            check_uniqueness(VMBoxId, Context, Mailbox)
    catch
        _:_ ->
            lager:debug("can't convert mailbox number to integer", []),
            'false'
    end.

-spec check_uniqueness(kz_term:api_binary(), cb_context:context(), pos_integer()) -> boolean().
check_uniqueness(VMBoxId, Context, Mailbox) ->
    ViewOptions = [{'key', Mailbox}],
    case kz_datamgr:get_results(cb_context:db_name(Context)
                               ,<<"vmboxes/listing_by_mailbox">>
                               ,ViewOptions
                               )
    of
        {'ok', []} -> 'true';
        {'ok', [VMBox]} ->
            VMBoxId =:= kz_doc:id(VMBox);
        {'ok', _} ->
            lager:warning("found multiple mailboxes for '~p'", [Mailbox]),
            'false';
        {'error', _E} ->
            lager:debug("failed to load listing_by_mailbox view: ~p", [_E]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec update_mwi(cb_context:context(), kz_term:ne_binary() | kz_term:ne_binaries()) -> cb_context:context().
update_mwi(Context, BoxId) when is_binary(BoxId) ->
    update_mwi(Context, [BoxId]);
update_mwi(Context, []) -> Context;
update_mwi(Context, [BoxId | BoxIds]) ->
    _ = update_mwi(Context, BoxId, cb_context:resp_status(Context)),
    update_mwi(Context, BoxIds).

-spec update_mwi(cb_context:context(), kz_term:ne_binary() | kz_term:ne_binaries(), atom()) -> cb_context:context().
update_mwi(Context, BoxId, 'success') ->
    AccountId = cb_context:account_id(Context),
    _ = kvm_mwi:notify_vmbox(AccountId, BoxId),
    Context;
update_mwi(Context, _BoxId, _Status) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_vm_box(kzd_voicemail_box:doc()) ->
          {'true', kzd_voicemail_box:doc()} |
          'false'.
maybe_migrate_vm_box(Box) ->
    case kz_json:get_value(<<"notify_email_address">>, Box) of
        'undefined' -> 'false';
        Emails ->
            {'true', kzd_voicemail_box:set_notification_emails(Box, Emails)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate(kz_term:ne_binary()) -> 'ok'.
migrate(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'error', _E} -> io:format("failed to check account ~s for voicemail boxes: ~p~n", [Account, _E]);
        {'ok', Boxes} ->
            maybe_migrate_boxes(AccountDb, Boxes)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_boxes(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
maybe_migrate_boxes(AccountDb, Boxes) ->
    ToUpdate =
        lists:foldl(fun(View, Acc) ->
                            Box = kz_json:get_value(<<"doc">>, View),
                            case maybe_migrate_vm_box(Box) of
                                'false' -> Acc;
                                {'true', Box1} -> [Box1 | Acc]
                            end
                    end
                   ,[]
                   ,Boxes
                   ),
    io:format("migrating ~p out of ~p boxes~n", [length(ToUpdate), length(Boxes)]),
    maybe_update_boxes(AccountDb, ToUpdate).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_boxes(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
maybe_update_boxes(_AccountDb, []) -> 'ok';
maybe_update_boxes(AccountDb, Boxes) ->
    case kz_datamgr:save_docs(AccountDb, Boxes) of
        {'ok', _Saved} -> io:format("  updated ~p boxes in ~s~n", [length(_Saved), AccountDb]);
        {'error', _E}-> io:format("  failed to update boxes: ~p~n", [_E])
    end.
