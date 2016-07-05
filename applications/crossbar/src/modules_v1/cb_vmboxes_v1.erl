%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% VMBoxes module
%%%
%%% Handle client requests for vmbox documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_vmboxes_v1).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
        ,validate/1, validate/2, validate/3, validate/4, validate/5
        ,content_types_provided/5
        ,put/1
        ,post/2, post/3, post/4
        ,patch/2
        ,delete/2, delete/3, delete/4

        ,migrate/1
        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(MESSAGES_RESOURCE, ?VM_KEY_MESSAGES).
-define(BIN_DATA, <<"raw">>).
-define(MEDIA_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.vmboxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.vmboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.vmboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.vmboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.vmboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.vmboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.patch.vmboxes">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.vmboxes">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_VMBoxID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE, _MsgID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE, _) -> 'true'.
resource_exists(_, ?MESSAGES_RESOURCE, _, ?BIN_DATA) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec acceptable_content_types() -> kz_proplist().
acceptable_content_types() -> ?MEDIA_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    content_types_provided_for_vm_download(Context, cb_context:req_verb(Context)).

content_types_provided_for_vm_download(Context, ?HTTP_GET) ->
    CTP = [{'to_binary', ?MEDIA_MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided_for_vm_download(Context, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_vmboxes(Context, cb_context:req_verb(Context)).

validate_vmboxes(Context, ?HTTP_GET) ->
    load_vmbox_summary(Context);
validate_vmboxes(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate(Context, DocId) ->
    validate_vmbox(Context, DocId, cb_context:req_verb(Context)).

validate_vmbox(Context, DocId, ?HTTP_GET) ->
    load_vmbox(DocId, Context);
validate_vmbox(Context, DocId, ?HTTP_POST) ->
    validate_request(DocId, Context);
validate_vmbox(Context, DocId, ?HTTP_PATCH) ->
    validate_patch(load_vmbox(DocId, Context), DocId);
validate_vmbox(Context, DocId, ?HTTP_DELETE) ->
    load_vmbox(DocId, Context).

validate(Context, DocId, ?MESSAGES_RESOURCE) ->
    validate_messages(Context, DocId, cb_context:req_verb(Context)).

validate(Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    validate_message(Context, DocId, MediaId, cb_context:req_verb(Context)).

validate(Context, DocId, ?MESSAGES_RESOURCE, MediaId, ?BIN_DATA) ->
    load_message_binary(DocId, MediaId, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, DocId) ->
    AccountId = cb_context:account_id(Context),
    Doc = cb_context:doc(Context),
    DocM = kz_json:get_value(?VM_KEY_MESSAGES, Doc),
    {Diff, BoxMsg} = kz_vm_message:find_message_differences(AccountId, DocId, DocM),
    _ = kz_vm_message:bulk_update(cb_context:account_id(Context), DocId, Diff),

    Props = [{?VM_KEY_MESSAGES, BoxMsg}],
    C = crossbar_doc:save(cb_context:set_doc(Context, kz_json:set_values(Props, Doc))),
    update_mwi(C, DocId).

post(Context, OldBoxId, ?MESSAGES_RESOURCE) ->
    AccountId = cb_context:account_id(Context),
    MsgIds = cb_context:req_value(Context, ?VM_KEY_MESSAGES, []),
    Folder = get_folder_filter(Context, ?VM_FOLDER_SAVED),

    case cb_context:req_value(Context, <<"source_id">>) of
        'undefined' ->
            {'ok', Result} = kz_vm_message:update_folder(Folder, MsgIds, AccountId, OldBoxId),
            C = cb_context:set_resp_data(Context, Result),
            update_mwi(C, OldBoxId);
        NewBoxId ->
            Moved = kz_vm_message:change_vmbox(AccountId, MsgIds, OldBoxId, NewBoxId),
            C = cb_context:set_resp_data(Context, Moved),
            update_mwi(C, [OldBoxId, NewBoxId])
    end.

post(Context, OldBoxId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case cb_context:req_value(Context, <<"source_id">>) of
        'undefined' ->
            C = update_message_folder(OldBoxId, MediaId, Context, ?VM_FOLDER_SAVED),
            update_mwi(C, OldBoxId);
        NewBoxId ->
            Moved = kz_vm_message:change_vmbox(AccountId, MediaId, OldBoxId, NewBoxId),
            C = cb_context:set_resp_data(Context, Moved),
            update_mwi(C, [OldBoxId, NewBoxId])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, DocId) ->
    AccountId = cb_context:account_id(Context),
    Msgs = kz_vm_message:messages(AccountId, DocId),
    _ = kz_vm_message:update_folder(?VM_FOLDER_DELETED, Msgs, cb_context:account_id(Context), DocId),
    C = crossbar_doc:delete(Context),
    update_mwi(C, DocId).

delete(Context, DocId, ?MESSAGES_RESOURCE) ->
    MsgIds = cb_context:resp_data(Context),
    {'ok', Result} = kz_vm_message:update_folder({?VM_FOLDER_DELETED, 'true'}, MsgIds, cb_context:account_id(Context), DocId),
    C = cb_context:set_resp_data(Context, Result),
    update_mwi(C, DocId).

delete(Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case kz_vm_message:update_folder({?VM_FOLDER_DELETED, 'true'}, MediaId, AccountId, DocId) of
        {'ok', Message} ->
            C = crossbar_util:response(Message, cb_context:set_resp_status(Context, 'success')),
            update_mwi(C, DocId);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    Doc = cb_context:doc(Context),
    DocM = kz_json:get_value(?VM_KEY_MESSAGES, Doc),
    {Diff, BoxMsg} = kz_vm_message:find_message_differences(AccountId, Id, DocM),
    {'ok', _} = kz_vm_message:bulk_update(cb_context:account_id(Context), Id, Diff),

    Props = [{?VM_KEY_MESSAGES, BoxMsg}],
    crossbar_doc:save(cb_context:set_doc(Context, kz_json:set_values(Props, Doc))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_message(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_message(Context, DocId, MediaId, ?HTTP_GET) ->
    case load_message(MediaId, DocId, 'undefined', Context) of
        {'true', C1} ->
            C2 = update_message_folder(DocId, MediaId, C1, ?VM_FOLDER_NEW),
            update_mwi(C2, DocId);
        {_, C} -> C
    end;
validate_message(Context, DocId, MediaId, ?HTTP_POST) ->
    case load_message_doc(MediaId, DocId, Context) of
        {'ok', _Doc} ->
            NewBoxId = cb_context:req_value(Context, <<"source_id">>),
            maybe_load_vmboxes(NewBoxId, Context);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
    end;
validate_message(Context, DocId, MediaId, ?HTTP_DELETE) ->
    Update = kz_json:from_list([{?VM_KEY_FOLDER, ?VM_FOLDER_DELETED}]),
    {_, C} = load_message(MediaId, DocId, Update, Context),
    C.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_messages(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_messages(Context, DocId, ?HTTP_GET) ->
    load_message_summary(DocId, Context);
validate_messages(Context, DocId, ?HTTP_POST) ->
    case cb_context:req_value(Context, ?VM_KEY_MESSAGES, []) =/= [] of
        'false' ->
            Message = kz_json:from_list([{<<"message">>, <<"No message ids are specified">>}]),
            cb_context:add_validation_error(<<"messages">>, <<"required">>, Message, Context);
        _ ->
            NewBoxId = cb_context:req_value(Context, <<"source_id">>),
            maybe_load_vmboxes([DocId | NewBoxId], Context)
    end;
validate_messages(Context, DocId, ?HTTP_DELETE) ->
    Messages = kz_vm_message:messages(cb_context:account_id(Context), DocId),

    Filter = case cb_context:req_value(Context, ?VM_KEY_MESSAGES) of
                 L when is_list(L) -> L;
                 _ -> get_folder_filter(Context, <<"all">>)
             end,

    ToDelete = filter_messages(Messages, Filter),

    cb_context:set_resp_data(
      cb_context:set_resp_status(Context, 'success')
                            ,ToDelete
     ).

-spec get_folder_filter(cb_context:context(), ne_binary()) -> kz_vm_message:vm_folder().
get_folder_filter(Context, Default) ->
    ReqData = cb_context:req_data(Context),
    QS = cb_context:query_string(Context),
    ReqJObj = cb_context:req_json(Context),

    case kz_json:find(?VM_KEY_FOLDER, [ReqData, QS, ReqJObj]) of
        ?VM_FOLDER_NEW -> ?VM_FOLDER_NEW;
        ?VM_FOLDER_SAVED ->  {?VM_FOLDER_DELETED, 'false'};
        _ -> Default
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_messages(kz_json:objects(), kz_vm_message:vm_folder() | ne_binaries()) -> ne_binaries().
-spec filter_messages(kz_json:objects(), ne_binary() | ne_binaries(), ne_binaries()) -> ne_binaries().
filter_messages(Messages, {?VM_FOLDER_DELETED, _}) ->
    filter_messages(Messages, ?VM_FOLDER_DELETED, []);
filter_messages(Messages, Filters) ->
    filter_messages(Messages, Filters, []).

filter_messages([], _Filters, Selected) ->
    Selected;
filter_messages([Mess|Messages], <<"all">>=Filters, Selected) ->
    Id = kzd_box_message:media_id(Mess),
    filter_messages(Messages, Filters, [Id|Selected]);
filter_messages([Mess|Messages], Filters, Selected) when Filters =:= ?VM_FOLDER_NEW;
                                                         Filters =:= ?VM_FOLDER_SAVED;
                                                         Filters =:= ?VM_FOLDER_DELETED ->
    Id = kzd_box_message:media_id(Mess),
    case kzd_box_message:folder(Mess) of
        Filters -> filter_messages(Messages, Filters, [Id|Selected]);
        _ -> filter_messages(Messages, Filters, Selected)
    end;
filter_messages(_, [], Selected) ->
    Selected;
filter_messages([Mess|Messages], Filters, Selected) ->
    Id = kzd_box_message:media_id(Mess),
    case lists:member(Id, Filters) of
        'true' -> filter_messages(Messages, Filters, [Id|Selected]);
        'false' -> filter_messages(Messages, Filters, Selected)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_unique_vmbox(api_binary(), cb_context:context()) -> cb_context:context().
-spec validate_unique_vmbox(api_binary(), cb_context:context(), api_binary()) -> cb_context:context().
validate_unique_vmbox(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context, cb_context:account_db(Context)).

validate_unique_vmbox(VMBoxId, Context, 'undefined') ->
    check_vmbox_schema(VMBoxId, Context);
validate_unique_vmbox(VMBoxId, Context, _AccountDb) ->
    case check_uniqueness(VMBoxId, Context) of
        'true' -> check_vmbox_schema(VMBoxId, Context);
        'false' ->
            C = cb_context:add_validation_error(
                  <<"mailbox">>
                                               ,<<"unique">>
                                               ,kz_json:from_list([{<<"message">>, <<"Invalid mailbox number or already exists">>}])
                                               ,Context
                 ),
            check_vmbox_schema(VMBoxId, C)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_vmbox_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_vmbox_schema(VMBoxId, Context) ->
    Context1 = maybe_migrate_notification_emails(Context),
    OnSuccess = fun(C) -> on_successful_validation(VMBoxId, C) end,
    cb_context:validate_request_data(<<"vmboxes">>, Context1, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) ->
                                      cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_voicemail_box:type()}],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(VMBoxId, Context) ->
    crossbar_doc:load_merge(VMBoxId, Context, ?TYPE_CHECK_OPTION(kzd_voicemail_box:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Support PATCH - merge vmbox document with request data
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(cb_context:context(), ne_binary()) -> cb_context:context().
validate_patch(Context, DocId)->
    crossbar_doc:patch_and_validate(DocId, Context, fun validate_request/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of vmboxes, each summarized.
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox_summary(cb_context:context()) -> cb_context:context().
load_vmbox_summary(Context) ->
    case kz_vm_message:vmbox_summary(cb_context:account_id(Context)) of
        {'ok', JObj} -> crossbar_doc:handle_json_success(JObj, Context);
        {'error', _} -> cb_context:add_system_error('datastore_fault', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a vmbox document from the database
%% @end
%%--------------------------------------------------------------------
-spec maybe_load_vmboxes(api_binary() | api_binaries(), cb_context:context()) -> cb_context:context().
maybe_load_vmboxes('undefined', Context) -> cb_context:set_resp_status(Context, 'success');
maybe_load_vmboxes(Id, Context) when is_binary(Id) -> load_vmbox(Id, Context);
maybe_load_vmboxes([], Context) -> Context;
maybe_load_vmboxes(['undefined'|Ids], Context) ->
    maybe_load_vmboxes(Ids, Context);
maybe_load_vmboxes([Id|Ids], Context) ->
    C1 = load_vmbox(Id, Context),
    maybe_load_vmboxes(Ids, C1).

-spec load_vmbox(ne_binary(), cb_context:context()) -> cb_context:context().
load_vmbox(DocId, Context) ->
    case kz_vm_message:load_vmbox(cb_context:account_id(Context), DocId, 'true') of
        {'ok', JObj} -> crossbar_doc:handle_json_success(JObj, Context);
        {'error', Error} -> crossbar_doc:handle_datamgr_errors(Error, DocId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get messages summary for a given mailbox
%% @end
%%--------------------------------------------------------------------
-spec load_message_summary(ne_binary(), cb_context:context()) -> cb_context:context().
load_message_summary(DocId, Context) ->
    Messages = kz_vm_message:messages(cb_context:account_id(Context), DocId),
    crossbar_util:response(Messages, cb_context:set_resp_status(Context, 'success')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message by its media ID and its context
%% @end
%%--------------------------------------------------------------------
-spec load_message(ne_binary(), ne_binary(), api_object(), cb_context:context()) ->
                          {boolean(), cb_context:context()}.
load_message(MediaId, BoxId, 'undefined', Context) ->
    load_message(MediaId, BoxId, kz_json:new(), Context);
load_message(MediaId, BoxId, UpdateJObj, Context) ->
    case load_message_doc(MediaId, BoxId, Context) of
        {'ok', MDoc} ->
            Message = kzd_box_message:metadata(MDoc),
            C = crossbar_doc:handle_json_success(Message, Context),
            ensure_message_in_folder(Message, UpdateJObj, C);
        {'error', Error} ->
            {'false', crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)}
    end.

-spec load_message_doc(ne_binary(), ne_binary(), cb_context:context()) -> {atom(), any()}.
load_message_doc(MediaId, BoxId, Context) ->
    case kz_vm_message:message_doc(cb_context:account_id(Context), MediaId) of
        {'ok', MDoc}=OK ->
            case kzd_box_message:source_id(MDoc) of
                BoxId -> OK;
                _ -> {'error', 'not_found'}
            end;
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_message_in_folder(kz_json:object(), kz_json:object(), cb_context:context()) ->
                                      {boolean(), cb_context:context()}.
ensure_message_in_folder(Message, UpdateJObj, Context) ->
    CurrentFolder = kzd_box_message:folder(Message, ?VM_FOLDER_NEW),

    RequestedFolder = cb_context:req_value(Context
                                          ,?VM_KEY_FOLDER
                                          ,kzd_box_message:folder(UpdateJObj, CurrentFolder)
                                          ),
    lager:debug("ensuring message is in folder ~s", [RequestedFolder]),
    NewMessage = kz_json:merge_jobjs(kzd_box_message:set_folder(RequestedFolder, UpdateJObj)
                                    ,Message
                                    ),
    {CurrentFolder =/= RequestedFolder
    ,cb_context:set_resp_data(cb_context:set_doc(Context, NewMessage), NewMessage)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%--------------------------------------------------------------------
-spec load_message_binary(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_message_binary(DocId, MediaId, Context) ->
    case kz_vm_message:message_doc(cb_context:account_id(Context), MediaId) of
        {'ok', JObj} ->
            case kz_datamgr:open_cache_doc(cb_context:account_db(Context), DocId) of
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, DocId, Context);
                {'ok', BoxJObj} ->
                    Timezone = kzd_voicemail_box:timezone(BoxJObj),
                    load_attachment_from_message(JObj, Context, Timezone)
            end;
        {'error', Err} -> crossbar_doc:handle_datamgr_errors(Err, MediaId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_attachment_from_message(kz_json:object(), cb_context:context(), ne_binary()) -> cb_context:context().
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
            lager:debug("Sending file with filename ~s", [Filename]),
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_resp_data/2, AttachBin}
                      ,{fun cb_context:set_resp_etag/2, 'undefined'}
                      ,{fun cb_context:add_resp_headers/2
                       ,[{<<"Content-Type">>, kz_doc:attachment_content_type(Doc, AttachmentId)}
                        ,{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                        ,{<<"Content-Length">>, kz_doc:attachment_length(Doc, AttachmentId)}
                        ]
                       }
                      ],
            cb_context:setters(Context, Setters)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% generate a media name based on CallerID and creation date
%% CallerID_YYYY-MM-DD_HH-MM-SS.ext
%% @end
%%--------------------------------------------------------------------
-spec generate_media_name(api_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
generate_media_name('undefined', GregorianSeconds, Ext, Timezone) ->
    generate_media_name(<<"unknown">>, GregorianSeconds, Ext, Timezone);
generate_media_name(CallerId, GregorianSeconds, Ext, Timezone) ->
    UTCDateTime = calendar:gregorian_seconds_to_datetime(kz_util:to_integer(GregorianSeconds)),
    LocalTime = case localtime:utc_to_local(UTCDateTime, kz_util:to_list(Timezone)) of
                    {{_,_,_},{_,_,_}}=LT -> lager:debug("Converted to TZ: ~s", [Timezone]), LT;
                    _ -> lager:debug("Bad TZ: ~p", [Timezone]), UTCDateTime
                end,
    Date = kz_util:pretty_print_datetime(LocalTime),
    list_to_binary([CallerId, "_", Date, Ext]).

%%--------------------------------------------------------------------
%% @private
%% @doc update message folder
%% @end
%%--------------------------------------------------------------------
-spec update_message_folder(ne_binary(), ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
update_message_folder(BoxId, MediaId, Context, DefaultFolder) ->
    AccountId = cb_context:account_id(Context),
    Folder = get_folder_filter(Context, DefaultFolder),
    case kz_vm_message:update_folder(Folder, MediaId, AccountId, BoxId) of
        {'ok', Message} ->
            crossbar_util:response(Message, cb_context:set_resp_status(Context, 'success'));
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_uniqueness(api_binary(), cb_context:context()) -> boolean().
-spec check_uniqueness(api_binary(), cb_context:context(), pos_integer()) -> boolean().
check_uniqueness(VMBoxId, Context) ->
    try kz_json:get_integer_value(<<"mailbox">>, cb_context:req_data(Context)) of
        Mailbox ->
            check_uniqueness(VMBoxId, Context, Mailbox)
    catch
        _:_ ->
            lager:debug("can't convert mailbox number to integer", []),
            'false'
    end.

check_uniqueness(VMBoxId, Context, Mailbox) ->
    ViewOptions = [{'key', Mailbox}],
    case kz_datamgr:get_results(cb_context:account_db(Context)
                               ,<<"vmboxes/listing_by_mailbox">>
                               ,ViewOptions
                               )
    of
        {'ok', []} -> 'true';
        {'ok', [VMBox]} ->
            VMBoxId =:= kz_doc:id(VMBox);
        {'ok', _} ->
            lager:warning("found multiple mailboxs for '~p'", [Mailbox]),
            'false';
        {'error', _E} ->
            lager:debug("failed to load listing_by_mailbox view: ~p", [_E]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_mwi(cb_context:context(), ne_binary() | ne_binaries()) -> cb_context:context().
-spec update_mwi(cb_context:context(), ne_binary() | ne_binaries(), atom()) -> cb_context:context().
update_mwi(Context, BoxId) when is_binary(BoxId) ->
    update_mwi(Context, [BoxId]);
update_mwi(Context, []) -> Context;
update_mwi(Context, [BoxId | BoxIds]) ->
    _ = update_mwi(Context, BoxId, cb_context:resp_status(Context)),
    update_mwi(Context, BoxIds).

update_mwi(Context, BoxId, 'success') ->
    AccountId = cb_context:account_id(Context),
    _ = cb_modules_util:update_mwi(BoxId, AccountId),
    Context;
update_mwi(Context, _BoxId, _Status) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_migrate_vm_box(kzd_voicemail_box:doc()) ->
                                  {'true', kzd_voicemail_box:doc()} |
                                  'false'.
maybe_migrate_vm_box(Box) ->
    case kz_json:get_value(<<"notify_email_address">>, Box) of
        'undefined' -> 'false';
        Emails ->
            {'true', kzd_voicemail_box:set_notification_emails(Box, Emails)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate(ne_binary()) -> 'ok'.
migrate(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?CB_LIST, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'error', _E} -> io:format("failed to check account ~s for voicemail boxes: ~p~n", [Account, _E]);
        {'ok', Boxes} ->
            maybe_migrate_boxes(AccountDb, Boxes)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_migrate_boxes(ne_binary(), kz_json:objects()) -> 'ok'.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_boxes(ne_binary(), kz_json:objects()) -> 'ok'.
maybe_update_boxes(_AccountDb, []) -> 'ok';
maybe_update_boxes(AccountDb, Boxes) ->
    case kz_datamgr:save_docs(AccountDb, Boxes) of
        {'ok', _Saved} -> io:format("  updated ~p boxes in ~s~n", [length(_Saved), AccountDb]);
        {'error', _E}-> io:format("  failed to update boxes: ~p~n", [_E])
    end.
