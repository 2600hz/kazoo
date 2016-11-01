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
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(cb_vmboxes).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
        ,validate/1, validate/2, validate/3, validate/4, validate/5
        ,content_types_accepted/4
        ,content_types_provided/4, content_types_provided/5
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
-define(MSG_LISTING_BY_MAILBOX, <<"mailbox_messages/listing_by_mailbox">>).

-define(BOX_ID_KEY_INDEX, 1).

-define(MESSAGES_RESOURCE, ?VM_KEY_MESSAGES).
-define(BIN_DATA, <<"raw">>).
-define(MEDIA_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}
                          ,{<<"application">>, <<"zip">>}
                          ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_accpeted.vmboxes">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.vmboxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.vmboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.vmboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.vmboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.vmboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.vmboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.vmboxes">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.vmboxes">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.finish_request.post.vmboxes">>, ?MODULE, 'finish_request'),
    ok.

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
allowed_methods(?MESSAGES_RESOURCE) ->
    [?HTTP_GET];
allowed_methods(_VMBoxId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    [?HTTP_POST];
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, _VMMsgId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_VMBoxId, ?MESSAGES_RESOURCE, _VMMsgId, ?BIN_DATA) ->
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

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _VMBox, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    CTA = [{'from_json', ?JSON_CONTENT_TYPES}],
    cb_context:set_content_types_provided(Context, CTA).

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    content_types_provided_for_vm_download(Context, cb_context:req_verb(Context));
content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, _) ->
    Context.

content_types_provided(Context, _VMBox, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    content_types_provided_for_vm_download(Context, cb_context:req_verb(Context)).

content_types_provided_for_vm_download(Context, ?HTTP_GET) ->
    CTP = [{'to_binary', ?MEDIA_MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided_for_vm_download(Context, ?HTTP_POST) ->
    CTP = [{'send_file', ?MEDIA_MIME_TYPES}],
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

validate(Context, ?MESSAGES_RESOURCE) ->
    load_message_summary('undefined', Context);
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

validate(Context, DocId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    load_messages_binaries(DocId, Context);
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
post(Context, _DocId) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    VMBoxMsgs = kz_json:get_value(?VM_KEY_MESSAGES, DbDoc),
    C1 = crossbar_doc:save(check_mailbox_for_messages_array(Context, VMBoxMsgs)),

    %% remove messages array to not let it exposed
    crossbar_util:response(kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1)), C1).

%%--------------------------------------------------------------------
%% @private
%% @doc disallow vmbox messages array changing
%% also check if vmbox has still message array
%% and inform client to do migrate their vmbox
%% @end
%%--------------------------------------------------------------------
-spec check_mailbox_for_messages_array(cb_context:context(), api_objects()) -> cb_context:context().
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

post(Context, OldBoxId, ?MESSAGES_RESOURCE) ->
    AccountId = cb_context:account_id(Context),
    MsgIds = kz_json:get_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []),
    Folder = get_folder_filter(Context, ?VM_FOLDER_SAVED),

    case kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)) of
        'undefined' ->
            Result = kvm_messages:change_folder(Folder, MsgIds, AccountId, OldBoxId),
            C = crossbar_util:response(Result, Context),
            update_mwi(C, OldBoxId);
        ?NE_BINARY = NewBoxId ->
            Moved = kvm_messages:move_to_vmbox(AccountId, MsgIds, OldBoxId, NewBoxId),
            C = crossbar_util:response(Moved, Context),
            update_mwi(C, [OldBoxId, NewBoxId]);
        NewBoxIds ->
            Copied = kvm_messages:copy_to_vmboxes(AccountId, MsgIds, OldBoxId, NewBoxIds),
            C = crossbar_util:response(Copied, Context),
            update_mwi(C, [OldBoxId | NewBoxIds])
    end.

post(Context, _DocId, ?MESSAGES_RESOURCE, ?BIN_DATA) ->
    Context;
post(Context, OldBoxId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)) of
        'undefined' ->
            Folder = get_folder_filter(Context, ?VM_FOLDER_SAVED),
            case kvm_message:change_folder(Folder, MediaId, AccountId, OldBoxId) of
                {'ok', Message} ->
                    C = crossbar_util:response(Message, Context),
                    update_mwi(C, OldBoxId);
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
            end;
        ?NE_BINARY = NewBoxId ->
            case kvm_message:move_to_vmbox(AccountId, MediaId, OldBoxId, NewBoxId) of
                {'ok', Moved} ->
                    C = crossbar_util:response(Moved, Context),
                    update_mwi(C, [OldBoxId, NewBoxId]);
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, MediaId, Context)
            end;
        NewBoxIds ->
            Copied = kvm_message:copy_to_vmboxes(AccountId, MediaId, OldBoxId, NewBoxIds),
            C = crossbar_util:response(Copied, Context),
            update_mwi(C, [OldBoxId | NewBoxIds])
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
    Msgs = kvm_messages:get(AccountId, DocId),
    MsgIds = [kzd_box_message:media_id(M) || M <- Msgs],
    _ = kvm_messages:change_folder(?VM_FOLDER_DELETED, MsgIds, cb_context:account_id(Context), DocId),
    C = crossbar_doc:delete(Context),
    update_mwi(C, DocId).

delete(Context, DocId, ?MESSAGES_RESOURCE) ->
    MsgIds = cb_context:resp_data(Context),
    Result = kvm_messages:change_folder({?VM_FOLDER_DELETED, 'true'}, MsgIds, cb_context:account_id(Context), DocId),
    C = crossbar_util:response(Result, Context),
    update_mwi(C, DocId).

delete(Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    AccountId = cb_context:account_id(Context),
    case kvm_message:change_folder({?VM_FOLDER_DELETED, 'true'}, MediaId, AccountId, DocId) of
        {'ok', Message} ->
            C = crossbar_util:response(Message, Context),
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
patch(Context, _Id) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    VMBoxMsgs = kz_json:get_value(?VM_KEY_MESSAGES, DbDoc),
    C1 = crossbar_doc:save(check_mailbox_for_messages_array(Context, VMBoxMsgs)),

    %% remove messages array to not let it exposed
    crossbar_util:response(kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1)), C1).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_message(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_message(Context, BoxId, MessageId, ?HTTP_GET) ->
    load_message(MessageId, BoxId, Context);
validate_message(Context, BoxId, MessageId, ?HTTP_POST) ->
    RetenTimestamp = kz_util:current_tstamp() - kvm_util:retention_seconds(),
    case kvm_message:fetch(cb_context:account_id(Context), MessageId, BoxId) of
        {'ok', Msg} ->
            case kzd_box_message:utc_seconds(Msg) < RetenTimestamp of
                'true' ->
                    ErrMsg = <<"cannot make changes to messages prior to retention duration">>,
                    cb_context:add_validation_error(MessageId
                                                   ,<<"date_range">>
                                                   ,kz_json:from_list(
                                                      [{<<"message">>, ErrMsg}
                                                      ,{<<"cause">>, RetenTimestamp}
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_messages(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_messages(Context, DocId, ?HTTP_GET) ->
    load_message_summary(DocId, Context);
validate_messages(Context, DocId, ?HTTP_POST) ->
    case kz_json:get_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []) of
        [] ->
            Message = kz_json:from_list([{<<"message">>, <<"No array of message ids are specified">>}]),
            cb_context:add_validation_error(<<"messages">>, <<"required">>, Message, Context);
        _ ->
            NewBoxId = kz_json:get_value(<<"source_id">>, cb_context:req_data(Context)),
            maybe_load_vmboxes([DocId | NewBoxId], Context)
    end;
validate_messages(Context, DocId, ?HTTP_DELETE) ->
    Messages = kvm_messages:get(cb_context:account_id(Context), DocId),

    Filter = case kz_json:get_value(?VM_KEY_MESSAGES, cb_context:req_data(Context)) of
                 L when is_list(L) -> L;
                 _ -> get_folder_filter(Context, <<"all">>)
             end,

    ToDelete = filter_messages(Messages, Filter, Context),

    cb_context:set_resp_data(
      cb_context:set_resp_status(Context, 'success')
                            ,ToDelete
     ).

-spec get_folder_filter(cb_context:context(), ne_binary()) -> kvm_message:vm_folder().
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

%%--------------------------------------------------------------------
%% @private
%% @doc Filter messages(JObjs) based on Folder or Ids and
%% apply query strings filters on them as well
%%
%% Note: Filter can be <<"all">> which return all messages.
%% @end
%%--------------------------------------------------------------------
-type filter_options() :: kvm_message:vm_folder() | ne_binaries().

-spec filter_messages(kz_json:objects(), filter_options(), cb_context:context()) -> ne_binaries().
-spec filter_messages(kz_json:objects(), filter_options(), cb_context:context(), ne_binaries()) -> ne_binaries().
filter_messages(Messages, {?VM_FOLDER_DELETED, _}, Context) ->
    %% move to delete folder and soft-delete
    filter_messages(Messages, ?VM_FOLDER_DELETED, Context, []);
filter_messages(Messages, Filters, Context) ->
    filter_messages(Messages, Filters, Context, []).

%% Filter by folder
filter_messages([], _Filters, _Context, Selected) -> Selected;
filter_messages([Mess|Messages], <<"all">> = Filter, Context, Selected) ->
    Id = kzd_box_message:media_id(Mess),
    filter_messages(Messages, Filter, Context, [Id|Selected]);
filter_messages([Mess|Messages], <<_/binary>> = Filter, Context, Selected)
  when Filter =:= ?VM_FOLDER_NEW;
       Filter =:= ?VM_FOLDER_SAVED;
       Filter =:= ?VM_FOLDER_DELETED ->
    Id = kzd_box_message:media_id(Mess),
    QsFiltered = should_filter_by_qs(Mess, crossbar_doc:has_qs_filter(Context), Context),
    case kzd_box_message:folder(Mess) of
        Filter when not QsFiltered -> filter_messages(Messages, Filter, Context, [Id|Selected]);
        _ -> filter_messages(Messages, Filter, Context, Selected)
    end;
%% Filter by Ids
filter_messages(_, [], _Context, Selected) -> Selected;
filter_messages([Mess|Messages], Filters, Context, Selected) ->
    Id = kzd_box_message:media_id(Mess),
    QsFiltered = should_filter_by_qs(Mess, crossbar_doc:has_qs_filter(Context), Context),
    case lists:member(Id, Filters) of
        'true' when not QsFiltered -> filter_messages(Messages, Filters, Context, [Id|Selected]);
        'false' -> filter_messages(Messages, Filters, Context, Selected)
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
            Msg = kz_json:from_list([{<<"message">>, <<"Invalid mailbox number or already exists">>}]),
            C = cb_context:add_validation_error(<<"mailbox">>, <<"unique">>, Msg, Context),
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
    Props = [{<<"pvt_type">>, kzd_voicemail_box:type()}
            ],
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
    Context1 = crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2),
    AccountMsgCounts = kvm_messages:count(cb_context:account_id(Context)),
    RspData = merge_summary_results(cb_context:doc(Context1), AccountMsgCounts),
    cb_context:set_resp_data(cb_context:set_doc(Context1, RspData), RspData).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec merge_summary_results(kz_json:objects(), kz_json:object()) -> kz_json:objects().
merge_summary_results(BoxSummary, AccountMsgCounts) ->
    MergeFun = fun(JObj, Acc) ->
                       [merge_summary_fold(JObj, AccountMsgCounts) | Acc]
               end,
    lists:foldl(MergeFun, [], BoxSummary).

-spec merge_summary_fold(kz_json:object(), kz_json:object()) -> kz_json:object().
merge_summary_fold(JObj, AccountMsgCounts) ->
    BoxId = kz_json:get_value(<<"id">>, JObj),
    case kz_json:get_value(BoxId, AccountMsgCounts) of
        'undefined' ->
            JObj;
        J ->
            BCount = kz_json:get_integer_value(?VM_KEY_MESSAGES, JObj, 0),
            New = kz_json:get_integer_value(?VM_FOLDER_NEW, J, 0),
            Saved = kz_json:get_integer_value(?VM_FOLDER_SAVED, J, 0),
            kz_json:set_value(?VM_KEY_MESSAGES, BCount + New + Saved, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a vmbox document from the database
%% @end
%%--------------------------------------------------------------------
-type source_id() :: api_binary() | api_binaries().

-spec maybe_load_vmboxes(source_id(), cb_context:context()) -> cb_context:context().
-spec maybe_load_vmboxes_fold(source_id(), cb_context:context()) -> cb_context:context().
maybe_load_vmboxes('undefined', Context) -> cb_context:set_resp_status(Context, 'success');
maybe_load_vmboxes(?NE_BINARY = Id, Context) -> load_vmbox(Id, Context);
maybe_load_vmboxes(<<>>, Context) -> empty_source_id(Context);
maybe_load_vmboxes([], Context) -> empty_source_id(Context);
maybe_load_vmboxes(Ids, Context) -> maybe_load_vmboxes_fold(Ids, Context).

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

-spec load_vmbox(ne_binary(), cb_context:context()) -> cb_context:context().
load_vmbox(DocId, Context) ->
    C1 = crossbar_doc:load(DocId, Context, ?TYPE_CHECK_OPTION(kzd_voicemail_box:type())),
    cb_context:set_resp_data(C1, kz_json:delete_key(?VM_KEY_MESSAGES, cb_context:resp_data(C1))).

-spec empty_source_id(cb_context:context()) -> cb_context:context().
empty_source_id(Context) ->
    Message = kz_json:from_list([{<<"message">>, <<"empty source_id is specified. please set a single or a list of source_ids.">>}]),
    cb_context:add_validation_error(<<"source_id">>, <<"required">>, Message, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get messages summary for a given mailbox
%% @end
%%--------------------------------------------------------------------
-spec load_message_summary(api_binary(), cb_context:context()) -> cb_context:context().
load_message_summary(BoxId, Context) ->
    MaxRetenSecond = kvm_util:retention_seconds(),
    RetenTimestamp = kz_util:current_tstamp() - MaxRetenSecond,
    case message_summary_view_options(Context, BoxId, MaxRetenSecond) of
        {'ok', ViewOptions} ->
            NormlizeFun = fun(J, Acc) ->
                                  message_summary_normalizer(BoxId, J, Acc, RetenTimestamp)
                          end,
            C1 = cb_context:set_resp_status(Context, 'success'),
            ViewResults = crossbar_doc:load_view(?MSG_LISTING_BY_MAILBOX
                                                ,ViewOptions
                                                ,C1
                                                ,NormlizeFun
                                                ),
            maybe_fix_envelope(ViewResults);
        Ctx -> Ctx
    end.

-spec message_summary_normalizer(api_binary(), kz_json:object(), kz_json:objects(), gregorian_seconds()) ->
                                        kz_json:objects().
message_summary_normalizer('undefined', JObj, Acc, RetenTimestamp) ->
    [kz_json:from_list(
       [{kz_json:get_value([<<"key">>, ?BOX_ID_KEY_INDEX], JObj)
        ,kvm_util:maybe_set_deleted_by_retention(kz_json:get_value(<<"value">>, JObj), RetenTimestamp)
        }
       ])
     | Acc
    ];
message_summary_normalizer(_BoxId, JObj, Acc, RetenTimestamp) ->
    [kvm_util:maybe_set_deleted_by_retention(kz_json:get_value(<<"value">>, JObj), RetenTimestamp)
     | Acc
    ].

-spec message_summary_view_options(cb_context:context(), api_binary(), gregorian_seconds()) ->
                                          {'ok', crossbar_doc:view_options()} |
                                          cb_context:context().
message_summary_view_options(Context, BoxId, MaxRetenSecond) ->
    AccountId = cb_context:account_id(Context),
    MaxRange = get_max_range(Context, MaxRetenSecond),
    case cb_modules_util:range_view_options(Context, MaxRange) of
        {CreatedFrom, CreatedTo} ->
            MODBs = lists:reverse(kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo)),
            {'ok', maybe_add_start_end_key(BoxId, CreatedTo, CreatedFrom, MODBs)};
        Ctx -> Ctx
    end.

-spec maybe_add_start_end_key(api_binary(), gregorian_seconds(), gregorian_seconds(), ne_binaries()) ->
                                     crossbar_doc:view_options().
maybe_add_start_end_key('undefined', _CreatedTo, _CreatedFrom, MODBs) ->
    [{'databases', MODBs}
    ,'descending'
    ];
maybe_add_start_end_key(BoxId, CreatedTo, CreatedFrom, MODBs) ->
    [{'databases', MODBs}
    ,{'startkey', [BoxId, CreatedTo]}
    ,{'endkey', [BoxId, CreatedFrom]}
    ,'descending'
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% check if create_from is exists or not, if not set max_range to max_retention
%% to return all messages in retention duration
%% @end
%%--------------------------------------------------------------------
-spec get_max_range(cb_context:context(), gregorian_seconds()) -> gregorian_seconds().
get_max_range(Context, MaxRetenSecond) ->
    case cb_context:req_value(Context, <<"created_from">>) of
        'undefined' -> MaxRetenSecond;
        _ -> ?MAX_RANGE
    end.

-spec maybe_fix_envelope(cb_context:context()) -> cb_context:context().
maybe_fix_envelope(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> fix_envelope(Context);
        _Status -> Context
    end.

-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    UpdatedEnvelope =
        case {cb_context:doc(Context)
             ,cb_context:resp_envelope(Context)
             }
        of
            {[], Envelope} ->
                kz_json:delete_keys([<<"start_key">>, <<"next_start_key">>], Envelope);
            {_, Envelope} ->
                fix_keys(Envelope)
        end,
    cb_context:set_resp_envelope(Context, UpdatedEnvelope).

-spec fix_keys(kz_json:object()) -> kz_json:object().
fix_keys(Envelope) ->
    lists:foldl(fun fix_key_fold/2
               ,Envelope
               ,[<<"start_key">>, <<"next_start_key">>]
               ).

-spec fix_key_fold(kz_json:key(), kz_json:object()) -> kz_json:object().
fix_key_fold(Key, Envelope) ->
    case kz_json:get_value(Key, Envelope) of
        [_BoxId, Timestamp] -> kz_json:set_value(Key, Timestamp, Envelope);
        'undefined' -> Envelope
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message by its media ID and its context
%% @end
%%--------------------------------------------------------------------
-spec load_message(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_message(MessageId, BoxId, Context) ->
    case kvm_message:message(cb_context:account_id(Context), MessageId, BoxId) of
        {'ok', Msg} ->
            crossbar_doc:handle_json_success(Msg, Context);
        {'error', Error} ->
            crossbar_doc:handle_datamgr_errors(Error, MessageId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%--------------------------------------------------------------------
-spec load_message_binary(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_message_binary(BoxId, MediaId, Context) ->
    case kvm_message:fetch(cb_context:account_id(Context), MediaId, BoxId) of
        {'ok', JObj} ->
            case kz_datamgr:open_cache_doc(cb_context:account_db(Context), BoxId) of
                {'error', Error} ->
                    crossbar_doc:handle_datamgr_errors(Error, BoxId, Context);
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
-spec load_attachment_from_message(kz_json:object(), cb_context:context(), ne_binary()) ->
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

-spec load_messages_binaries(ne_binary(), cb_context:context()) -> cb_context:context().
load_messages_binaries(BoxId, Context) ->
    WorkDir = kz_util:to_list(<<"/tmp/", (cb_context:req_id(Context))/binary, "/">>),
    Ids = kz_json:get_value(?VM_KEY_MESSAGES, cb_context:req_data(Context), []),
    case Ids =/= []
        andalso kz_datamgr:open_cache_doc(cb_context:account_db(Context), BoxId)
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

-spec save_attachments_to_file(ne_binaries(), ne_binary(), cb_context:context(), ne_binary(), string()) ->
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

-spec save_attachment_to_file(ne_binary(), ne_binary(), cb_context:context(), ne_binary(), string()) ->
                                     'ok' | {atom(), any()}.
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
                    'ok' = file:write_file(lists:concat([WorkDir, kz_util:to_list(Filename)]), AttachBin)
            end;
        {'error', _}=E -> E
    end.

-spec maybe_create_zip_file(string(), cb_context:context()) -> cb_context:context().
maybe_create_zip_file(WorkDir, Context) ->
    Files = [kz_util:to_list(F) || F <- filelib:wildcard("*", WorkDir)],
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
    ZipName = lists:concat([kz_util:to_list(cb_context:req_id(Context)), ".zip"]),
    ZipPath = ["/tmp/", ZipName],
    {'ok', _} = zip:zip(ZipPath , Files, [{'cwd', WorkDir}]),
    _ = del_dir(WorkDir),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_etag/2, 'undefined'}
              ,{fun cb_context:set_resp_file/2, kz_util:to_binary(ZipPath)}
              ,{fun cb_context:add_resp_headers/2
               ,[{<<"Content-Type">>, <<"application/zip">>}
                ,{<<"Content-Disposition">>, <<"attachment; filename=", (kz_util:to_binary(ZipName))/binary>>}
                ,{<<"Content-Length">>, filelib:file_size(ZipPath)}
                ]
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

-spec should_filter_by_qs(kz_json:object(), boolean(), cb_context:context()) -> boolean().
should_filter_by_qs(_, 'false', _Context) -> 'true';
should_filter_by_qs(JObj, 'true', Context) ->
    crossbar_doc:filtered_doc_by_qs(JObj, Context).

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
