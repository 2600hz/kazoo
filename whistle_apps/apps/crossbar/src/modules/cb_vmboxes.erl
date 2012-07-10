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

-include("../../include/crossbar.hrl").

-define(CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(MESSAGES_RESOURCE, <<"messages">>).
-define(BIN_DATA, <<"raw">>).
-define(MEDIA_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.vmboxes">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.vmboxes">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.vmboxes">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.vmboxes">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.vmboxes">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.vmboxes">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.vmboxes">>, ?MODULE, delete).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods().
-spec allowed_methods/4 :: (path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_VMBoxID) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE) ->
    ['GET'].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE, _MsgID) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_VMBoxID, ?MESSAGES_RESOURCE, _MsgID, ?BIN_DATA) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
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
-spec content_types_provided/5 :: (#cb_context{}, path_token(), path_token(), path_token(), path_token()) -> #cb_context{}.
content_types_provided(#cb_context{req_verb = <<"get">>}=Context
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
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec validate/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
-spec validate/5 :: (#cb_context{}, path_token(), path_token(), path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_vmbox_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_vmbox(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_vmbox(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_vmbox(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_vmbox(DocId, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId, ?MESSAGES_RESOURCE) ->
    load_message_summary(DocId, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    load_message(DocId, MediaId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    update_message(DocId, MediaId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId, ?MESSAGES_RESOURCE, MediaId) ->
    delete_message(DocId, MediaId, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId, ?MESSAGES_RESOURCE, MediaId, ?BIN_DATA) ->
    load_message_binary(DocId, MediaId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DocId) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, _DocId, ?MESSAGES_RESOURCE, _MediaID) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec delete/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _DocID) ->
    crossbar_doc:delete(Context).
delete(#cb_context{}=Context, _DocID, ?MESSAGES_RESOURCE, _MediaID) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_vmbox_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new vmbox document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_vmbox/1 :: (#cb_context{}) -> #cb_context{}.
create_vmbox(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"vmboxes">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            case mailbox_exists(Context, JObj) of
                true ->
                    crossbar_util:response_invalid_data(
                      wh_json:from_list([{<<"mailbox">>, <<"invalid mailbox number or number exists">>}])
                      ,Context);
                false ->
                    Context#cb_context{
                      doc=wh_json:set_value(<<"pvt_type">>, <<"vmbox">>, JObj)
                      ,resp_status=success
                     }
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a vmbox document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_vmbox/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_vmbox(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing vmbox document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_vmbox/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_vmbox(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"vmboxes">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            case mailbox_exists(Context, JObj) of
                true ->
                    crossbar_util:response_invalid_data(
                      wh_json:from_list([{<<"mailbox">>, <<"invalid mailbox number or number exists">>}])
                      ,Context);
                false ->
                    crossbar_doc:load_merge(DocId, JObj, Context)
            end
    end.

-spec mailbox_exists/2 :: (#cb_context{}, wh_json:json_object()) -> boolean().
mailbox_exists(#cb_context{db_name=Db}, JObj) ->
    Mailbox = wh_json:get_value(<<"mailbox">>, JObj),

    try wh_util:to_integer(Mailbox) of
        BoxNum ->
            lager:debug("does a mailbox with number ~b exist?", [BoxNum]),
            case couch_mgr:get_results(Db, <<"vmboxes/listing_by_mailbox">>, [{key, BoxNum}]) of
                {ok, []} -> false;
                {ok, [VMBox]} ->
                    lager:debug("found existing vm box: ~p", [wh_json:get_value(<<"id">>, VMBox)]),
                    lager:debug("compared to submitted: ~p", [wh_json:get_value(<<"id">>, JObj)]),
                    wh_json:get_value(<<"id">>, JObj) =/= wh_json:get_value(<<"id">>, VMBox);
                {error, _E} ->
                    lager:debug("failed to load listing_by_mailbox view: ~p", [_E]),
                    true
            end
    catch
        _:_ ->
            lager:debug("can't convert mailbox to integer: ~p", [Mailbox]),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get messages summary for a given mailbox
%% @end
%%--------------------------------------------------------------------
-spec load_message_summary/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_message_summary(DocId, Context) ->
    case load_messages(DocId, Context) of
        [] ->            
            crossbar_util:response([], Context);
        [JObj]=Messages ->
            case wh_json:is_empty(JObj) of
                true ->
                    crossbar_util:response([], Context);
                false ->
                    crossbar_util:response(Messages,Context)
            end;
        Messages ->
            crossbar_util:response(Messages,Context)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get associated  Messages as a List of json obj
%% @end
%%--------------------------------------------------------------------
-spec load_messages/2 :: (ne_binary(), #cb_context{}) -> wh_json:json_objects().
load_messages(DocId, Context) ->
    #cb_context{resp_status=success, doc=Doc} = crossbar_doc:load(DocId, Context),
    wh_json:get_value(<<"messages">>, Doc, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message by its media ID and its context
%% @end
%%--------------------------------------------------------------------
-spec load_message/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
load_message(DocId, MediaId, Context) ->
    case lists:filter(fun(M) -> wh_json:get_value(<<"media_id">>, M) =:= MediaId end, load_messages(DocId, Context)) of
        [M] ->
            crossbar_util:response(M,Context);
        _ ->
            crossbar_util:response_bad_identifier(MediaId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get message binary content so it can be downloaded
%% VMBoxId is the doc id for the voicemail box document
%% VMId is the id for the voicemail document, containing the binary data
%% @end
%%--------------------------------------------------------------------
-spec load_message_binary/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
load_message_binary(VMBoxId, VMId, #cb_context{req_data=ReqData, db_name=Db, resp_headers=RespHeaders}=Context) ->
    {ok, VMJObj} = couch_mgr:open_doc(Db, VMId),
    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, VMJObj),

    #cb_context{resp_data=VMMetaJObj} = load_message(VMBoxId, VMId, Context),

    {ok, VMBoxJObj} = couch_mgr:open_doc(Db, VMBoxId),

    Filename = generate_media_name(wh_json:get_value(<<"caller_id_number">>, VMMetaJObj)
                                   ,wh_json:get_value(<<"timestamp">>, VMMetaJObj)
                                   ,filename:extension(AttachmentId)
                                   ,wh_json:get_value(<<"timezone">>, VMBoxJObj)
                                  ),
    lager:debug("Sending file with filename ~s", [Filename]),

    Ctx = crossbar_doc:load_attachment(VMId, AttachmentId, Context),

    _ = case wh_json:get_value(<<"folder">>, ReqData) of
            undefined -> ok;
            Folder ->
                lager:debug("Moving message to ~s", [Folder]),
                spawn(fun() ->
                              _ = crossbar_util:put_reqid(Context),
                              Context1 = update_message1(VMBoxId, VMId, Context),
                              #cb_context{resp_status=success}=crossbar_doc:save(Context1),
                              lager:debug("Saved message to new folder ~s", [Folder]),
                              update_mwi(VMBoxJObj, Db)
                      end)
        end,

    Ctx#cb_context{resp_headers=[{<<"Content-Type">>, wh_json:get_value([<<"_attachments">>, AttachmentId, <<"content_type">>], VMJObj)}
                                 ,{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                                 ,{<<"Content-Length">> ,wh_util:to_binary(wh_json:get_value([<<"_attachments">>, AttachmentId, <<"length">>], VMJObj))}
                                 | RespHeaders
                                ]
                   ,resp_etag=undefined
                  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% DELETE the message (set folder prop to deleted)
%% @end
%%--------------------------------------------------------------------
-spec delete_message/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
delete_message(VMBoxId, MediaId, #cb_context{db_name=Db}=Context) ->
    Context1 = #cb_context{doc=VMBox} = crossbar_doc:load(VMBoxId, Context),
    Messages = wh_json:get_value(<<"messages">>, VMBox, []),

    case get_message_index(MediaId, Messages) of
        Index when Index > 0 ->
            VMBox1 = wh_json:set_value([<<"messages">>, Index, <<"folder">>], <<"deleted">>, VMBox),

            %% let's not forget the associated private_media doc
            {ok, D} = couch_mgr:open_doc(Db, MediaId),
            couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_deleted">>, true, D)),

            _ = spawn(fun() -> _ = crossbar_util:put_reqid(Context), update_mwi(VMBox1, Db) end),

            Context1#cb_context{doc=VMBox1};
        _ ->
            crossbar_util:response_bad_identifier(MediaId, Context)
    end.

-spec update_mwi/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
update_mwi(VMBox, DB) ->
    OwnerID = wh_json:get_value(<<"owner_id">>, VMBox),
    false = wh_util:is_empty(OwnerID),

    lager:debug("Sending MWI update to devices owned by ~s", [OwnerID]),

    Messages = wh_json:get_value(<<"messages">>, VMBox, []),

    Devices = cb_module_util:get_devices_owned_by(OwnerID, DB),

    New = count_messages(Messages, <<"new">>),
    Saved = count_messages(Messages, <<"saved">>),

    lager:debug("New: ~b Saved: ~b", [New, Saved]),

    CommonHeaders = [{<<"Messages-New">>, New}
                     ,{<<"Messages-Saved">>, Saved}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],

    lists:foreach(fun(Device) ->
                          User = wh_json:get_value([<<"sip">>, <<"username">>], Device),
                          Realm = wh_json:get_value([<<"sip">>, <<"realm">>], Device),

                          Command = wh_json:from_list([{<<"Notify-User">>, User}
                                                       ,{<<"Notify-Realm">>, Realm}
                                                       | CommonHeaders
                                                      ]),
                          catch wapi_notifications:publish_mwi_update(Command)
                  end, Devices).

-spec count_messages/2 :: (wh_json:json_objects(), ne_binary()) -> non_neg_integer().
count_messages(Messages, Folder) ->
    lists:sum([1 || Message <- Messages, wh_json:get_value(<<"folder">>, Message) =:= Folder]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiate the recursive search of the message index
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message_index/2 :: (ne_binary(), wh_json:json_objects()) -> pos_integer().
get_message_index(MediaId, Messages) ->
    find_index(MediaId, Messages, 1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Recursively finds the index of message
%%
%% @end
%%--------------------------------------------------------------------
-spec find_index/3 :: (ne_binary(), wh_json:json_objects(), pos_integer()) -> integer().
find_index(MediaId, [Message | Ms], Index) ->
    case wh_json:get_value(<<"media_id">>, Message) =:= MediaId of
        true -> Index;
        false -> find_index(MediaId, Ms, Index + 1)
    end;
find_index(_, [], _) ->
    -1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the message informations
%% Only Folder prop is editable atm
%% @end
%%--------------------------------------------------------------------
-spec update_message/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
update_message(DocId, MediaId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"vmboxes">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            update_message1(DocId, MediaId, Context#cb_context{doc=JObj})
    end. 

-spec update_message1/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
update_message1(VMBoxId, MediaId, #cb_context{req_data=ReqData}=Context) ->
    RequestedValue = wh_json:get_value(<<"folder">>, ReqData),

    Context1 = #cb_context{doc=VMBox} = crossbar_doc:load(VMBoxId, Context),

    Messages = wh_json:get_value(<<"messages">>, VMBox, []),

    case get_message_index(MediaId, Messages) of
        Index when Index > 0 ->
            VMBox1 = wh_json:set_value([<<"messages">>, Index, <<"folder">>], RequestedValue, VMBox),
            Context1#cb_context{doc=VMBox1};
        _Idx ->
            crossbar_util:response_bad_identifier(MediaId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% generate a media name based on CallerID and creation date
%% CallerID_YYYY-MM-DD_HH-MM-SS.ext
%% @end
%%--------------------------------------------------------------------
-spec generate_media_name/4 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
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
