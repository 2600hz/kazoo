%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
-module(cb_vmboxes).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
         ,validate/1, validate/2, validate/3, validate/4, validate/5
         ,content_types_provided/5
         ,put/1
         ,post/2, post/4
         ,patch/2
         ,delete/2, delete/4
         ,cleanup_heard_voicemail/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(MESSAGES_RESOURCE, <<"messages">>).
-define(BIN_DATA, <<"raw">>).
-define(MEDIA_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_provided.vmboxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.vmboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.vmboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.vmboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.vmboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.vmboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.vmboxes">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.vmboxes">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(crossbar_cleanup:binding_account(), ?MODULE, 'cleanup_heard_voicemail').

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
    [?HTTP_GET].
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
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context,_VMBox, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    content_types_provided_for_download(Context, cb_context:req_verb(Context)).

content_types_provided_for_download(Context, ?HTTP_GET) ->
    CTP = [{'to_binary', ?MEDIA_MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided_for_download(Context, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @private
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
    load_message_summary(DocId, Context).

validate(Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    validate_message(Context, DocId, MediaId, cb_context:req_verb(Context)).

validate_message(Context, DocId, MediaId, ?HTTP_GET) ->
    case load_message(DocId, MediaId, 'undefined', Context) of
        {'true', C1} ->
            C2 = crossbar_doc:save(C1),
            update_mwi(
              cb_context:set_resp_data(C2, cb_context:resp_data(C1))
             );
        {_, C} -> C
    end;
validate_message(Context, DocId, MediaId, ?HTTP_POST) ->
    {_, C} = load_message(DocId, MediaId, 'undefined', Context),
    C;
validate_message(Context, DocId, MediaId, ?HTTP_DELETE) ->
    Update = wh_json:from_list([{<<"folder">>, <<"deleted">>}]),
    {_, C} = load_message(DocId, MediaId, Update, Context),
    C.

validate(Context, DocId, ?MESSAGES_RESOURCE, MediaId, ?BIN_DATA) ->
    case load_message_binary(DocId, MediaId, Context) of
        {'true', C1} ->
            C2 = crossbar_doc:save(C1),
            update_mwi(
              cb_context:set_resp_data(C2, cb_context:resp_data(C1))
             );
        {_, C} -> C
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, _DocId) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).
post(Context, _DocId, ?MESSAGES_RESOURCE, _MediaID) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, _DocID) ->
    C = crossbar_doc:delete(Context),
    update_mwi(C).
delete(Context, _DocID, ?MESSAGES_RESOURCE, _MediaID) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _Id) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox_summary(cb_context:context()) -> cb_context:context().
load_vmbox_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context).

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
                    ,wh_json:from_list([
                        {<<"message">>, <<"Invalid mailbox number or already exists">>}
                     ])
                    ,Context
                ),
            check_vmbox_schema(VMBoxId, C)
    end.

check_vmbox_schema(VMBoxId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(VMBoxId, C) end,
    cb_context:validate_request_data(<<"vmboxes">>, Context, OnSuccess).

on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"vmbox">>}],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(VMBoxId, Context) ->
    crossbar_doc:load_merge(VMBoxId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Support PATCH - merge vmbox document with request data
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(cb_context:context(), ne_binary()) -> cb_context:context().
validate_patch(Context, DocId)->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = wh_doc:public_fields(cb_context:req_data(Context)),
            VmJObj = wh_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnValidateReqDataSuccess = fun(C) -> crossbar_doc:load_merge(DocId, C) end,
            cb_context:validate_request_data(<<"vmboxes">>, cb_context:set_req_data(Context, VmJObj), OnValidateReqDataSuccess);

        _Status -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a vmbox document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox(ne_binary(), cb_context:context()) -> cb_context:context().
load_vmbox(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get messages summary for a given mailbox
%% @end
%%--------------------------------------------------------------------
-spec load_message_summary(ne_binary(), cb_context:context()) -> cb_context:context().
load_message_summary(DocId, Context) ->
    Context1 = crossbar_doc:load(DocId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Messages = [Message
                        || Message <- wh_json:get_ne_value(<<"messages">>, cb_context:doc(Context1), []),
                           wh_json:get_value(<<"folder">>, Message) =/= <<"deleted">>
                       ],
            crossbar_util:response(Messages, Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message by its media ID and its context
%% @end
%%--------------------------------------------------------------------
-spec load_message(ne_binary(), ne_binary(), api_object(), cb_context:context()) ->
                          {boolean(), cb_context:context()}.
load_message(DocId, MediaId, 'undefined', Context) ->
    load_message(DocId, MediaId, wh_json:new(), Context);
load_message(DocId, MediaId, UpdateJObj, Context) ->
    Context1 = crossbar_doc:load(DocId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Messages = wh_json:get_value(<<"messages">>, cb_context:doc(Context1), []),
            case get_message_index(MediaId, Messages) of
                'false' ->
                    {'false'
                     ,cb_context:add_system_error(
                          'bad_identifier'
                          ,wh_json:from_list([{<<"cause">>, DocId}])
                          ,Context
                      )
                    };
                Index ->
                    CurrentMetaData = wh_json:get_value([<<"messages">>, Index], cb_context:doc(Context1), wh_json:new()),
                    CurrentFolder = wh_json:get_value(<<"folder">>, CurrentMetaData, <<"new">>),

                    RequestedFolder = cb_context:req_value(Context
                                                           ,<<"folder">>
                                                           ,wh_json:get_value(<<"folder">>, UpdateJObj, CurrentFolder)
                                                          ),
                    lager:debug("ensuring message is in folder ~s", [RequestedFolder]),
                    MetaData = wh_json:merge_jobjs(wh_json:set_value(<<"folder">>, RequestedFolder, UpdateJObj)
                                                   ,CurrentMetaData
                                                  ),
                    {CurrentFolder =/= RequestedFolder
                     ,cb_context:set_resp_data(
                        cb_context:set_doc(Context1, wh_json:set_value([<<"messages">>, Index], MetaData, cb_context:doc(Context1)))
                        ,MetaData
                       )}
            end;
        _Status ->
            {'false', Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%--------------------------------------------------------------------
-spec load_message_binary(ne_binary(), ne_binary(), cb_context:context()) ->
                                 {boolean(), cb_context:context()}.
load_message_binary(DocId, MediaId, Context) ->
    {Update, Context1} = load_message(DocId, MediaId, 'undefined', Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_message_binary_from_message(MediaId, Context1, Update);
        _Status -> {Update, Context1}
    end.

-spec load_message_binary_from_message(ne_binary(), cb_context:context(), boolean()) ->
                                              {boolean(), cb_context:context()}.
-spec load_message_binary_from_message(ne_binary(), cb_context:context(), boolean(), wh_json:object()) ->
                                              {boolean(), cb_context:context()}.
load_message_binary_from_message(MediaId, Context, Update) ->
    case couch_mgr:open_cache_doc(cb_context:account_db(Context), MediaId) of
        {'error', 'not_found'} ->
            {'false', set_bad_media_identifier(Context, MediaId)};
        {'error', _E} ->
            {'false', cb_context:add_system_error('datastore_fault', Context)};
        {'ok', Media} ->
            load_message_binary_from_message(MediaId, Context, Update, Media)
    end.

load_message_binary_from_message(MediaId, Context, Update, Media) ->
    VMMetaJObj = cb_context:resp_data(Context),
    Doc = cb_context:doc(Context),

    [AttachmentId] = wh_doc:attachment_names(Media),
    Filename = generate_media_name(wh_json:get_value(<<"caller_id_number">>, VMMetaJObj)
                                   ,wh_json:get_value(<<"timestamp">>, VMMetaJObj)
                                   ,filename:extension(AttachmentId)
                                   ,wh_json:get_value(<<"timezone">>, Doc)
                                  ),
    case couch_mgr:fetch_attachment(cb_context:account_db(Context), MediaId, AttachmentId) of
        {'error', 'db_not_reachable'} ->
            {'false', cb_context:add_system_error('datastore_unreachable', Context)};
        {'error', 'not_found'} ->
            {'false', set_bad_media_identifier(Context, MediaId)};
        {'ok', AttachBin} ->
            lager:debug("Sending file with filename ~s", [Filename]),
            {Update
             ,cb_context:setters(Context
                                 ,[{fun cb_context:set_resp_data/2, AttachBin}
                                   ,{fun cb_context:set_resp_etag/2, 'undefined'}
                                   ,{fun cb_context:add_resp_headers/2
                                     ,[{<<"Content-Type">>, wh_doc:attachment_content_type(Doc, AttachmentId)}
                                       ,{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                                       ,{<<"Content-Length">>, wh_doc:attachment_length(Doc, AttachmentId)}
                                      ]
                                    }
                                  ]
                                )
            }
    end.

-spec set_bad_media_identifier(cb_context:context(), ne_binary()) -> cb_context:context().
set_bad_media_identifier(Context, MediaId) ->
    cb_context:add_system_error(
      'bad_identifier'
      ,wh_json:from_list([{<<"cause">>, MediaId}])
      ,Context
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiate the recursive search of the message index
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message_index(ne_binary(), wh_json:objects()) -> 'false' | pos_integer().
get_message_index(MediaId, Messages) ->
    case lists:takewhile(fun(Message) ->
                                 wh_json:get_value(<<"media_id">>, Message) =/= MediaId
                         end, Messages)
    of
        Messages -> 'false';
        List -> length(List) + 1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% generate a media name based on CallerID and creation date
%% CallerID_YYYY-MM-DD_HH-MM-SS.ext
%% @end
%%--------------------------------------------------------------------
-spec generate_media_name(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
generate_media_name(CallerId, GregorianSeconds, Ext, Timezone) ->
    UTCDateTime = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(GregorianSeconds)),

    LocalTime = case localtime:utc_to_local(UTCDateTime, wh_util:to_list(Timezone)) of
                    {{_,_,_},{_,_,_}}=LT -> lager:debug("Converted to TZ: ~s", [Timezone]), LT;
                    _ -> lager:debug("Bad TZ: ~p", [Timezone]), UTCDateTime
                end,

    Date = wh_util:pretty_print_datetime(LocalTime),

    case CallerId of
        'undefined' -> list_to_binary(["unknown_", Date, Ext]);
        _ -> list_to_binary([CallerId, "_", Date, Ext])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_uniqueness(api_binary(), cb_context:context()) -> boolean().
check_uniqueness(VMBoxId, Context) ->
    try wh_json:get_integer_value(<<"mailbox">>, cb_context:req_data(Context)) of
        Mailbox ->
            ViewOptions = [{'key', Mailbox}],
            case couch_mgr:get_results(cb_context:account_db(Context), <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
                {'ok', []} -> 'true';
                {'ok', [VMBox]} ->
                    VMBoxId =:= wh_json:get_value(<<"id">>, VMBox);
                {'ok', _} ->
                    lager:debug("found multiple mailboxs for '~p'", [Mailbox]),
                    'false';
                {'error', _E} ->
                    lager:debug("failed to load listing_by_mailbox view: ~p", [_E]),
                    'false'
            end
    catch
        _:_ ->
            lager:debug("can't convert mailbox number to integer", []),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_mwi(cb_context:context()) -> cb_context:context().
update_mwi(Context) ->
    update_mwi(Context, cb_context:resp_status(Context)).
update_mwi(Context, 'success') ->
    OwnerId = wh_json:get_value(<<"owner_id">>, cb_context:doc(Context)),
    _ = cb_modules_util:update_mwi(OwnerId, cb_context:account_db(Context)),
    Context;
update_mwi(Context, _Status) ->
    Context.

-spec cleanup_heard_voicemail(ne_binary()) -> any().
-spec cleanup_heard_voicemail(ne_binary(), pos_integer()) -> any().
-spec cleanup_heard_voicemail(ne_binary(), pos_integer(), wh_proplist()) -> any().
cleanup_heard_voicemail(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case whapps_account_config:get_global(AccountDb
                                          ,<<"callflow">>
                                          ,[<<"voicemail">>, <<"message_retention_duration">>]
                                          ,30 %% 30 days
                                  )
    of
        'undefined' -> lager:debug("no limit to voicemail retention");
        Duration ->
            lager:debug("retaining messages for ~p days, delete those older for ~s", [Duration, Account]),
            cleanup_heard_voicemail(AccountDb, wh_util:to_integer(Duration))
    end.

cleanup_heard_voicemail(AccountDb, Duration) ->
    Today = wh_util:current_tstamp(),
    DurationS = Duration * ?SECONDS_IN_DAY, % duration in seconds
    case couch_mgr:get_results(AccountDb, <<"vmboxes/crossbar_listing">>, ['include_docs']) of
        {'ok', []} -> lager:debug("no voicemail boxes in ~s", [AccountDb]);
        {'ok', View} ->
            lager:debug("cleaning up ~b voicemail boxes in ~s", [length(View), AccountDb]),
            cleanup_heard_voicemail(AccountDb
                                    ,Today - DurationS
                                    ,[{wh_json:get_value(<<"doc">>, V)
                                       ,wh_json:get_value([<<"doc">>, <<"messages">>], V)
                                      }
                                      || V <- View
                                     ]
                                   );
        {'error', _E} ->
            lager:debug("failed to get voicemail boxes in ~s: ~p", [AccountDb, _E])
    end.
cleanup_heard_voicemail(AccountDb, Timestamp, Boxes) ->
    [cleanup_voicemail_box(AccountDb, Timestamp, Box) || Box <- Boxes].

-spec cleanup_voicemail_box(ne_binary(), pos_integer(), {wh_json:object(), wh_json:objects()}) -> any().
cleanup_voicemail_box(AccountDb, Timestamp, {Box, Msgs}) ->
    case lists:partition(fun(Msg) ->
                                 %% must be old enough, and not in the NEW folder
                                 wh_json:get_integer_value(<<"timestamp">>, Msg) < Timestamp
                                     andalso wh_json:get_value(<<"folder">>, Msg) =/= <<"new">>
                         end, Msgs)
    of
        {[], _} -> lager:debug("there are no old messages to remove from ~s", [wh_json:get_value(<<"_id">>, Box)]);
        {Older, Newer} ->
            lager:debug("there are ~b old messages to remove", [length(Older)]),

            _ = [catch delete_media(AccountDb, wh_json:get_value(<<"media_id">>, Msg)) || Msg <- Older],
            lager:debug("soft-deleted old messages"),

            Box1 = wh_json:set_value(<<"messages">>, Newer, Box),
            {'ok', Box2} = couch_mgr:save_doc(AccountDb, Box1),
            lager:debug("updated messages in voicemail box ~s", [wh_json:get_value(<<"_id">>, Box2)])
    end.

-spec delete_media(ne_binary(), ne_binary()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
delete_media(AccountDb, MediaId) ->
    {'ok', JObj} = couch_mgr:open_cache_doc(AccountDb, MediaId),
    couch_mgr:ensure_saved(AccountDb, wh_json:set_value(<<"pvt_deleted">>, 'true', JObj)).
