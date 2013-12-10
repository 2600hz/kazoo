%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handles port request lifecycles
%%% GET /port_requests - list all the account's port requests
%%% GET /port_requests/descendants - detailed report of a port request
%%% GET /port_requests/{id} - detailed report of a port request
%%%
%%% PUT /port_requests - start a new port request
%%% PUT /port_requests/{id}/ready - indicate a port request is ready and let port dept know
%%%   Causes billing to occur
%%%   Ensure there's at least one attachment of non-0 length
%%% PUT /port_requests/{id}/progress - SDA indicates the port request is being processed
%%% PUT /port_requests/{id}/completion - SDA can force completion of the port request (populate numbers DBs)
%%% PUT /port_requests/{id}/rejection - SDA can force rejection of the port request
%%%
%%% POST /port_requests/{id} - update a port request
%%% DELETE /port_requests/{id} - delete a port request, only if in "waiting" or "rejected"
%%%
%%% GET /port_request/{id}/attachments - List attachments on the port request
%%% PUT /port_request/{id}/attachments - upload a document
%%% GET /port_request/{id}/attachments/{attachment_id} - download the document
%%% POST /port_request/{id}/attachments/{attachment_id} - replace a document
%%% DELETE /port_request/{id}/attachments/{attachment_id} - delete a document
%%%
%%% { "numbers":{
%%%   "+12225559999":{
%%%   },
%%%   "port_state": ["waiting", "ready", "progress", "completion", "rejection"]
%%% }
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_port_requests).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3, content_types_provided/4
         ,content_types_accepted/3, content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1, put/3
         ,post/2, post/4
         ,delete/2, delete/4
         ,cleanup/1
        ]).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"pdf">>}
                                ,{<<"application">>, <<"octet-stream">>}
                                ,{<<"text">>, <<"plain">>}
                               ]).

-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).

-define(UNFINISHED_PORT_REQUEST_LIFETIME
        ,whapps_config:get_integer(?CONFIG_CAT, <<"unfinished_port_request_lifetime_s">>, ?SECONDS_IN_DAY * 30)
       ).

-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle_number_manager/include/wh_port_request.hrl").
-include("../crossbar.hrl").

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
    wh_port_request:init(),

    _ = crossbar_bindings:bind(crossbar_cleanup:binding_system(), ?MODULE, 'cleanup'),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.port_requests">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.port_requests">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.port_requests">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.port_requests">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.port_requests">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.port_requests">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.port_requests">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.port_requests">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.port_requests">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Cleanup expired port requests
%% @end
%%--------------------------------------------------------------------
-spec cleanup(ne_binary()) -> 'ok'.
-spec cleanup(ne_binary(), wh_json:objects()) -> 'ok'.

cleanup(?KZ_PORT_REQUESTS_DB = Db) ->
    ModifiedBefore = wh_util:current_tstamp() - ?UNFINISHED_PORT_REQUEST_LIFETIME,
    ViewOpts = [{'startkey', [0]}
                ,{'endkey', [ModifiedBefore]}
                ,{'limit', 5000}
                ,'include_docs'
               ],
    case couch_mgr:get_results(Db, <<"port_requests/listing_by_modified">>, ViewOpts) of
        {'ok', []} -> lager:debug("no port requests older than ~p", [ModifiedBefore]);
        {'ok', OldPortReqeusts} -> cleanup(Db, OldPortReqeusts);
        {'error', _E} -> lager:debug("failed to query old port requests: ~p", [_E])
    end;
cleanup(_) -> 'ok'.

cleanup(Db, OldPortRequests) ->
    lager:debug("checking ~b old port requests", [length(OldPortRequests)]),

    Deletable = [wh_json:get_value(<<"doc">>, OldPortRequest)
                 || OldPortRequest <- OldPortRequests,
                    should_delete_port_request(wh_json:get_value(<<"key">>, OldPortRequest))
                ],
    lager:debug("found ~p deletable", [length(Deletable)]),
    couch_mgr:del_docs(Db, Deletable),
    'ok'.

-spec should_delete_port_request([pos_integer() | ne_binary(),...]) -> boolean().
should_delete_port_request([_Modified, ?PORT_READY]) ->
    'false';
should_delete_port_request([_Modified, ?PORT_PROGRESS]) ->
    'false';
should_delete_port_request(_) ->
    'true'.

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
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?PORT_DESCENDANTS) ->
    [?HTTP_GET];
allowed_methods(_Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_Id, ?PORT_READY) ->
    [?HTTP_PUT];
allowed_methods(_Id, ?PORT_PROGRESS) ->
    [?HTTP_PUT];
allowed_methods(_Id, ?PORT_COMPLETE) ->
    [?HTTP_PUT];
allowed_methods(_Id, ?PORT_REJECT) ->
    [?HTTP_PUT];
allowed_methods(_Id, ?PORT_ATTACHMENT) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /port_requests => []
%%    /port_requests/foo => [<<"foo">>]
%%    /port_requests/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.

resource_exists(_Id) -> 'true'.

resource_exists(_Id, ?PORT_READY) -> 'true';
resource_exists(_Id, ?PORT_PROGRESS) -> 'true';
resource_exists(_Id, ?PORT_COMPLETE) -> 'true';
resource_exists(_Id, ?PORT_REJECT) -> 'true';
resource_exists(_Id, ?PORT_ATTACHMENT) -> 'true';
resource_exists(_Id, _Unknown) -> 'false'.

resource_exists(_Id, ?PORT_ATTACHMENT, _AttachmentId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{}=Context) ->
    Context.
content_types_provided(#cb_context{}=Context, _Id) ->
    Context.
content_types_provided(#cb_context{}=Context, _Id, _) ->
    Context.
content_types_provided(#cb_context{req_verb=?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    case crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)) of
        #cb_context{resp_status='success', doc=JObj}=Context1 ->
            lager:debug("ctp: ~p", [JObj]),
            ContentTypeKey = [<<"_attachments">>, AttachmentId, <<"content_type">>],
            lager:debug("ctp: ~s", [ContentTypeKey]),
            case wh_json:get_value(ContentTypeKey, JObj) of
                'undefined' ->
                    lager:debug("no content type defined"),
                    Context1;
                ContentType ->
                    lager:debug("found content type ~s", [ContentType]),
                    [Type, SubType] = binary:split(ContentType, <<"/">>),
                    Context1#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end;
        Context1 -> Context1
    end;
content_types_provided(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT) ->
    CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA};
content_types_accepted(#cb_context{}=Context, _Id, _) ->
    Context.

content_types_accepted(#cb_context{req_verb=?HTTP_POST}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA};
content_types_accepted(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /port_requests mights load a list of port_request objects
%% /port_requests/123 might load the port_request object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) ->
                      cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?PORT_DESCENDANTS) ->
    read_descendants(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    is_deletable(crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB))).

validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_READY) ->
    maybe_move_state(Id, Context, ?PORT_READY);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_PROGRESS) ->
    maybe_move_state(Id, Context, ?PORT_PROGRESS);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_COMPLETE) ->
    maybe_move_state(Id, Context, ?PORT_COMPLETE);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_REJECT) ->
    maybe_move_state(Id, Context, ?PORT_REJECT);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT) ->
    summary_attachments(Id, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_ATTACHMENT) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    load_attachment(Id, AttachmentId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    load_attachment(Id, AttachmentId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    is_deletable(load_attachment(Id, AttachmentId, Context)).

-spec is_deletable(cb_context:context()) -> cb_context:context().
-spec is_deletable(cb_context:context(), ne_binary()) -> cb_context:context().
is_deletable(Context) ->
    is_deletable(Context, wh_port_request:current_state(cb_context:doc(Context))).
is_deletable(Context, ?PORT_WAITING) ->
    Context;
is_deletable(Context, ?PORT_REJECT) ->
    Context;
is_deletable(Context, _PortState) ->
    lager:debug("port is in state ~s, can't modify", [_PortState]),
    cb_context:add_system_error(<<"port request is not modifiable in this state">>, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(#cb_context{}=Context) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

put(#cb_context{}=Context, Id, ?PORT_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),

    Contents = wh_json:get_value(<<"contents">>, FileJObj),

    Opts = [{'headers', [{'content_type', CT = wh_json:get_string_value([<<"headers">>, <<"content_type">>], FileJObj)}
                        ]
            }],

    crossbar_doc:save_attachment(Id
                                 ,cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                 ,Opts
                                );
put(#cb_context{}=Context, Id, ?PORT_READY) ->
    try send_port_request_notification(Context, Id) of
        _ ->
            lager:debug("port request notification sent"),
            post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the port request notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(<<"failed to send port request email to system admins">>, Context)
    end;
put(#cb_context{}=Context, Id, ?PORT_PROGRESS) ->
    post(Context, Id);
put(#cb_context{}=Context, Id, ?PORT_COMPLETE) ->
    post(Context, Id);
put(#cb_context{}=Context, Id, ?PORT_REJECT) ->
    post(Context, Id).

-spec send_port_request_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_request_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_request/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(#cb_context{}=Context, _Id) ->
    case crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)) of
        #cb_context{resp_status='success'}=Context1 ->
            cb_context:set_resp_data(Context1, wh_port_request:public_fields(cb_context:doc(Context1)));
        Context1 ->
            Context1
    end.

post(#cb_context{}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),

    Contents = wh_json:get_value(<<"contents">>, FileJObj),

    Opts = [{'headers', [{'content_type', wh_json:get_string_value([<<"headers">>, <<"content_type">>], FileJObj)}]
            }],
    OldAttachments = wh_json:get_value(<<"_attachments">>, cb_context:doc(Context), wh_json:new()),

    case wh_json:get_value(AttachmentId, OldAttachments) of
        'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
        _AttachmentMeta ->
            lager:debug("deleting old attachment ~s", [AttachmentId]),
            couch_mgr:delete_attachment(cb_context:account_db(Context), Id, AttachmentId)
    end,

    crossbar_doc:save_attachment(Id
                                 ,AttachmentId
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                 ,Opts
                                ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(#cb_context{}=Context, _Id) ->
    crossbar_doc:delete(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).
delete(#cb_context{}=Context, Id, ?PORT_ATTACHMENT, AttachmentName) ->
    crossbar_doc:delete_attachment(Id, AttachmentName, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"port_requests">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    case cb_context:resp_status(Context1) of
        'success' ->
            PubDoc = wh_port_request:public_fields(cb_context:doc(Context1)),
            cb_context:set_resp_data(cb_context:set_doc(Context1, PubDoc)
                                     ,PubDoc
                                    );
        _ -> Context1
    end.

-spec read_descendants(cb_context:context()) -> cb_context:context().
read_descendants(Context) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS
                                      , [{<<"startkey">>, [cb_context:account_id(Context)]}
                                         ,{<<"endkey">>, [cb_context:account_id(Context), wh_json:new()]}
                                        ]
                                      ,cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> read_descendants(Context1, cb_context:doc(Context1));
        _ -> Context1
    end.

read_descendants(Context, SubAccounts) ->
    AllPortRequests = lists:foldl(fun(Account, Acc) ->
                                          AccountId = wh_json:get_value(<<"id">>, Account),
                                          PortRequests = read_descendant(Context, AccountId),
                                          wh_json:set_value(AccountId, PortRequests, Acc)
                                  end, wh_json:new(), SubAccounts),
    crossbar_doc:handle_json_success(AllPortRequests, Context).

-spec read_descendant(cb_context:context(), ne_binary()) -> wh_json:object().
read_descendant(Context, Id) ->
    Context1 = summary(cb_context:set_account_id(Context, Id)),
    case cb_context:resp_status(Context1) of
        'success' -> cb_context:doc(Context1);
        _ -> wh_json:new()
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"port_requests">>, Context, OnSuccess).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    ViewOptions = [{'startkey', [cb_context:account_id(Context)]}
                   ,{'endkey', [cb_context:account_id(Context), wh_json:new()]}
                   ,'include_docs'
                  ],

    crossbar_doc:load_view(<<"port_requests/crossbar_listing">>
                           ,ViewOptions
                           ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                           ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(Res, Acc) ->
    [wh_port_request:public_fields(wh_json:get_value(<<"doc">>, Res)) | Acc].

-spec summary_attachments(ne_binary(), cb_context:context()) -> cb_context:context().
summary_attachments(Id, Context) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),

    A = wh_json:get_value(<<"_attachments">>, cb_context:doc(Context1), wh_json:new()),
    cb_context:set_resp_data(Context1
                             ,wh_port_request:normalize_attachments(A)
                            ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{}=Context) ->
    on_successful_validation('undefined', Context, 'true');
on_successful_validation(Id, Context) ->
    Context1 = crossbar_doc:load_merge(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    on_successful_validation(Id, Context1, can_update_port_request(Context1)).

on_successful_validation(Id, Context, 'true') ->
    JObj = cb_context:doc(Context),
    Numbers = wh_json:get_keys(wh_json:get_value(<<"numbers">>, JObj)),

    Context1 = lists:foldl(fun(Number, ContextAcc) ->
                                   check_number_portability(Id, Number, ContextAcc)
                           end, Context, Numbers),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("number(s) checked out for ~s", [Id]),
            successful_validation(Id, Context);
        _ -> Context1
    end;
on_successful_validation(_Id, Context, 'false') ->
    PortState = wh_json:get_value(?PORT_STATE, cb_context:doc(Context)),
    lager:debug("port state ~s is not valid for updating a port request"
                ,[PortState]
               ),
    cb_context:add_validation_error(PortState, <<"type">>, <<"Updating port requests not allowed in current port state">>, Context).

-spec can_update_port_request(cb_context:context()) -> boolean().
-spec can_update_port_request(cb_context:context(), ne_binary()) -> boolean().
can_update_port_request(Context) ->
    lager:debug("port req: ~p", [cb_context:doc(Context)]),
    can_update_port_request(Context, wh_port_request:current_state(cb_context:doc(Context))).

can_update_port_request(_Context, ?PORT_WAITING) ->
    'true';
can_update_port_request(Context, ?PORT_READY) ->
    cb_modules_util:is_superduper_admin(Context);
can_update_port_request(Context, ?PORT_PROGRESS) ->
    cb_modules_util:is_superduper_admin(Context);
can_update_port_request(_Context, ?PORT_COMPLETE) ->
    'false';
can_update_port_request(_Context, ?PORT_REJECT) ->
    'true'.

-spec successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
successful_validation('undefined', Context) ->
    JObj = cb_context:doc(Context),
    cb_context:set_doc(Context, wh_json:set_values([{<<"pvt_type">>, <<"port_request">>}
                                                    ,{?PORT_PVT_STATE, ?PORT_WAITING}
                                                   ]
                                                   ,wh_port_request:normalize_numbers(JObj)
                                                  ));
successful_validation(_Id, #cb_context{}=Context) ->
    cb_context:set_doc(Context, wh_port_request:normalize_numbers(cb_context:doc(Context))).

-spec check_number_portability(api_binary(), ne_binary(), cb_context:context()) ->
                                      cb_context:context().
-spec check_number_portability(api_binary(), ne_binary(), cb_context:context(), ne_binary(), wh_json:object()) ->
                                      cb_context:context().
check_number_portability(PortId, Number, Context) ->
    E164 = wnm_util:to_e164(Number),
    lager:debug("checking ~s(~s) for portability", [E164, Number]),

    PortOptions = [{'key', E164}],
    case couch_mgr:get_results(?KZ_PORT_REQUESTS_DB, <<"port_requests/port_in_numbers">>, PortOptions) of
        {'ok', []} -> check_number_existence(E164, Number, Context);
        {'ok', [PortReq]} ->
            check_number_portability(PortId, Number, Context, E164, PortReq);
        {'ok', [_|_]=_PortReqs} ->
            lager:debug("number ~s(~s) exists on multiple port request docs. That's bad!", [E164, Number]),
            cb_context:add_validation_error(Number, <<"type">>, <<"Number is currently on multiple port requests. Contact a system admin to rectify">>, Context);
        {'error', _E} ->
            lager:debug("failed to query the port request view: ~p", [_E]),
            cb_context:add_validation_error(Number, <<"type">>, <<"Failed to query backend services, cannot port at this time">>, Context)
    end.

check_number_portability(PortId, Number, Context, E164, PortReq) ->
    case {wh_json:get_value(<<"value">>, PortReq) =:= cb_context:account_id(Context)
          ,wh_json:get_value(<<"id">>, PortReq) =:= PortId
         }
    of
        {'true', 'true'} ->
            lager:debug("number ~s(~s) is on this existing port request for this account(~s)"
                        ,[E164, Number, cb_context:account_id(Context)]
                       ),
            cb_context:set_resp_status(Context, 'success');
        {'true', 'false'} ->
            lager:debug("number ~s(~s) is on a different port request in this account(~s): ~s"
                        ,[E164, Number, cb_context:account_id(Context), wh_json:get_value(<<"id">>, PortReq)]
                       ),
            cb_context:add_validation_error(Number, <<"type">>, <<"Number is on a port request already: ", (wh_json:get_value(<<"id">>, PortReq))/binary>>, Context);
        {'false', _} ->
            lager:debug("number ~s(~s) is on existing port request for other account(~s)"
                        ,[E164, Number, wh_json:get_value(<<"value">>, PortReq)]
                       ),
            cb_context:add_validation_error(Number, <<"type">>, <<"Number is being ported for a different account">>, Context)
    end.

-spec check_number_existence(ne_binary(), ne_binary(), cb_context:context()) ->
                                    cb_context:context().
check_number_existence(E164, Number, Context) ->
    case wh_number_manager:lookup_account_by_number(E164) of
        {'ok', _AccountId, _} ->
            lager:debug("number ~s exists and belongs to ~s", [E164, _AccountId]),
            cb_context:add_validation_error(Number, <<"type">>, <<"Number exists on the system already">>, Context);
        {'error', 'not_found'} ->
            lager:debug("number ~s not found in numbers db (portable!)", [E164]),
            cb_context:set_resp_status(Context, 'success');
        {'error', 'unassigned'} ->
            lager:debug("number ~s not assigned to an account (portable!)", [E164]),
            cb_context:set_resp_status(Context, 'success');
        {'error', E} ->
            lager:debug("number ~s errored when looking up: ~p", [E164, E]),
            cb_context:add_validation_error(Number, <<"type">>, wh_util:to_binary(E), Context)
    end.

-spec load_attachment(ne_binary(), ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(Id, AttachmentId, Context) ->
    Context1 = read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_attachment(AttachmentId, Context1);
        _ -> Context1
    end.

-spec load_attachment(ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(AttachmentId, Context) ->
    AttachmentMeta = wh_json:get_value([<<"_attachments">>, AttachmentId], cb_context:doc(Context)),


    cb_context:add_resp_headers(
      crossbar_doc:load_attachment(cb_context:doc(Context)
                                   ,AttachmentId
                                   ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                  )
      ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
        ,{<<"Content-Type">>, wh_json:get_value([<<"content_type">>], AttachmentMeta)}
        ,{<<"Content-Length">>, wh_json:get_value([<<"length">>], AttachmentMeta)}
       ]).

-spec maybe_move_state(ne_binary(), cb_context:context(), ne_binary()) ->
                              cb_context:context().
maybe_move_state(Id, Context, PortState) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso wh_port_request:transition_state(cb_context:doc(Context1), PortState)
    of
        'false' -> Context1;
        {'ok', PortRequest} ->
            cb_context:set_doc(Context1, PortRequest);
        {'error', 'invalid_state_transition'} ->
            cb_context:add_validation_error(<<"port_state">>, <<"enum">>, <<"cannot move to new state from current state">>, Context)
    end.
