%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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
         ,delete/2, delete/4
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
    _ = crossbar_bindings:bind(<<"*.content_types_provided.vmboxes">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.vmboxes">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.vmboxes">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.vmboxes">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.vmboxes">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.vmboxes">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.vmboxes">>, ?MODULE, delete).


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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
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
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, ?MESSAGES_RESOURCE) -> true.
resource_exists(_, ?MESSAGES_RESOURCE, _) -> true.
resource_exists(_, ?MESSAGES_RESOURCE, _, ?BIN_DATA) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(#cb_context{}, path_token(), path_token(), path_token(), path_token()) -> #cb_context{}.
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context
                       ,_VMBox, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    CTP = [{to_binary, ?MEDIA_MIME_TYPES}],
    Context#cb_context{content_types_provided=CTP}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
-spec validate(#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec validate(#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
-spec validate(#cb_context{}, path_token(), path_token(), path_token(), path_token()) -> #cb_context{}.

validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_vmbox_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId) ->
    load_vmbox(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, DocId) ->
    validate_request(DocId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, DocId) ->
    load_vmbox(DocId, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId, ?MESSAGES_RESOURCE) ->
    load_message_summary(DocId, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    case load_message(DocId, MediaId, undefined, Context) of
        {true, #cb_context{resp_data=Data}=C1} ->
            C2 = crossbar_doc:save(C1),
            update_mwi(C2#cb_context{resp_data=Data});
        {_, C} -> C
    end;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    load_message(DocId, MediaId, undefined, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    Update = wh_json:from_list([{<<"folder">>, <<"deleted">>}]),
    load_message(DocId, MediaId, Update, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, DocId, ?MESSAGES_RESOURCE, MediaId, ?BIN_DATA) ->
    case load_message_binary(DocId, MediaId, Context) of
        {true, #cb_context{resp_data=Data}=C1} ->
            C2 = crossbar_doc:save(C1),
            update_mwi(C2#cb_context{resp_data=Data});
        {_, C} -> C
    end.

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
-spec post(#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DocId) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).
post(#cb_context{}=Context, _DocId, ?MESSAGES_RESOURCE, _MediaID) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).

-spec put(#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
-spec delete(#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _DocID) ->
    C = crossbar_doc:delete(Context),
    update_mwi(C).
delete(#cb_context{}=Context, _DocID, ?MESSAGES_RESOURCE, _MediaID) ->
    C = crossbar_doc:save(Context),
    update_mwi(C).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox_summary(#cb_context{}) -> #cb_context{}.
load_vmbox_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
validate_request(VMBoxId, Context) ->
    validate_unique_vmbox(VMBoxId, Context).

validate_unique_vmbox(VMBoxId, #cb_context{db_name=undefined}=Context) ->
    check_vmbox_schema(VMBoxId, Context);
validate_unique_vmbox(VMBoxId, Context) ->
    case check_uniqueness(VMBoxId, Context) of
        true -> check_vmbox_schema(VMBoxId, Context);
        false ->
            C = cb_context:add_validation_error(<<"mailbox">>
                                                    ,<<"unique">>
                                                    ,<<"Invalid mailbox number or already exists">>
                                                    ,Context),
            check_vmbox_schema(VMBoxId, C)
    end.

check_vmbox_schema(VMBoxId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(VMBoxId, C) end,
    cb_context:validate_request_data(<<"vmboxes">>, Context, OnSuccess).

on_successful_validation(undefined, #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"vmbox">>}],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(VMBoxId, #cb_context{}=Context) -> 
    crossbar_doc:load_merge(VMBoxId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a vmbox document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox(ne_binary(), #cb_context{}) -> #cb_context{}.
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
-spec load_message_summary(ne_binary(), #cb_context{}) -> #cb_context{}.
load_message_summary(DocId, Context) ->
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=C ->
            Messages = [Message 
                        || Message <- wh_json:get_ne_value(<<"messages">>, Doc, []),
                           wh_json:get_value(<<"folder">>, Message) =/= <<"deleted">>
                       ],
            crossbar_util:response(Messages, C);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message by its media ID and its context
%% @end
%%--------------------------------------------------------------------
-spec load_message(ne_binary(), ne_binary(), 'undefined' | wh_json:object(), #cb_context{}) ->
                                {boolean(), #cb_context{}}.
load_message(DocId, MediaId, undefined, Context) ->
    load_message(DocId, MediaId, wh_json:new(), Context);
load_message(DocId, MediaId, UpdateJObj, #cb_context{req_data=ReqData, query_json=QueryData}=Context) ->
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=C ->        
            Messages = wh_json:get_value(<<"messages">>, Doc, []),
            case get_message_index(MediaId, Messages) of
                false -> {false, cb_context:add_system_error(bad_identifier, [{details, DocId}], Context)};
                Index ->
                    CurrentMetaData = wh_json:get_value([<<"messages">>, Index], Doc, wh_json:new()),
                    CurrentFolder = wh_json:get_value(<<"folder">>, CurrentMetaData, <<"new">>),
                    RequestedFolder = wh_json:get_ne_value(<<"folder">>, QueryData
                                                           ,wh_json:get_ne_value(<<"folder">>, ReqData, CurrentFolder)),
                    lager:debug("Ensuring message is in folder ~s", [RequestedFolder]),
                    MetaData = wh_json:merge_jobjs(wh_json:set_value(<<"folder">>, RequestedFolder, UpdateJObj)
                                                   ,CurrentMetaData),
                    {CurrentFolder =/= RequestedFolder
                     ,C#cb_context{doc=wh_json:set_value([<<"messages">>, Index], MetaData, Doc)
                                   ,resp_data=MetaData
                                  }}
            end;
        Else ->
            {false, Else}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%--------------------------------------------------------------------
-spec load_message_binary(ne_binary(), ne_binary(), #cb_context{}) -> {boolean(), #cb_context{}}.
load_message_binary(DocId, MediaId, #cb_context{db_name=Db, resp_headers=RespHeaders}=Context) ->
    case load_message(DocId, MediaId, undefined, Context) of
        {Update, #cb_context{resp_status=success, resp_data=VMMetaJObj, doc=Doc}=C} ->
            case couch_mgr:open_doc(Db, MediaId) of
                {error, _R} -> cb_context:add_system_error(bad_identifier, [{details, MediaId}], C);
                {ok, Media} ->
                    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, Media),
                    Filename = generate_media_name(wh_json:get_value(<<"caller_id_number">>, VMMetaJObj)
                                                   ,wh_json:get_value(<<"timestamp">>, VMMetaJObj)
                                                   ,filename:extension(AttachmentId)
                                                   ,wh_json:get_value(<<"timezone">>, Doc)
                                                  ),
                    case couch_mgr:fetch_attachment(Db, MediaId, AttachmentId) of
                        {error, db_not_reachable} ->
                            {false, cb_context:add_system_error(datastore_unreachable, C)};
                        {error, not_found} ->
                            {false, cb_context:add_system_error(bad_identifier, [{details, MediaId}], C)};
                        {ok, AttachBin} ->
                            lager:debug("Sending file with filename ~s", [Filename]),
                            {Update
                             ,C#cb_context{resp_status=success
                                           ,resp_headers=[{<<"Content-Type">>, wh_json:get_value([<<"_attachments">>, AttachmentId, <<"content_type">>], Doc)}
                                                          ,{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                                                          ,{<<"Content-Length">>, wh_util:to_binary(wh_json:get_value([<<"_attachments">>, AttachmentId, <<"length">>], Doc))}
                                                          | RespHeaders
                                                         ]
                                           ,resp_etag=undefined
                                           ,resp_data=AttachBin
                                          }}
                    end
            end;
        Else ->
            Else
    end.

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
        Messages -> false;
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
        undefined -> list_to_binary(["unknown_", Date, Ext]);
        _ -> list_to_binary([CallerId, "_", Date, Ext])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_uniqueness('undefined' | ne_binary(), #cb_context{}) -> boolean().
check_uniqueness(VMBoxId, #cb_context{db_name=AccountDb, req_data=JObj}) ->
    try wh_json:get_integer_value(<<"mailbox">>, JObj) of
        Mailbox ->
            ViewOptions = [{key, Mailbox}],
            case couch_mgr:get_results(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
                {ok, []} -> true;
                {ok, [VMBox]} ->
                    VMBoxId =:= wh_json:get_value(<<"id">>, VMBox);
                {ok, _} -> 
                    lager:debug("found multiple mailboxs for '~p'", [Mailbox]),
                    false;
                {error, _E} ->
                    lager:debug("failed to load listing_by_mailbox view: ~p", [_E]),
                    false
            end
    catch
        _:_ ->
            lager:debug("can't convert mailbox number to integer", []),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_mwi(#cb_context{}) -> #cb_context{}.
update_mwi(#cb_context{resp_status=success, db_name=AccountDb, doc=JObj}=Context) ->
    OwnerId = wh_json:get_value(<<"owner_id">>, JObj),
    _ = cb_modules_util:update_mwi(OwnerId, AccountDb),
    Context;
update_mwi(Context) ->
    Context.
