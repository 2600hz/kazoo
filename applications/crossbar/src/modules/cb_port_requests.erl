%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handles port request lifecycles
%%% GET /port_requests - list all the account's port requests
%%% GET /port_requests/{id} - detailed report of a port request
%%%
%%% PUT /port_requests - start a new port request
%%% PUT /port_requests/{id}/ready - indicate a port request is ready and let port dept know
%%%   Causes billing to occur
%%%   Ensure there's at least one attachment of non-0 length
%%% PUT /port_requests/{id}/progress - SDA indicates the port request is being processed
%%% PUT /port_requests/{id}/complete - SDA can force completion of the port request (populate numbers DBs)
%%% PUT /port_requests/{id}/reject - SDA can force rejection of the port request
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
%%%   "port_state": ["waiting", "ready", "in_progress", "completed", "rejected"]
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
        ]).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"pdf">>}
                                ,{<<"application">>, <<"octet-stream">>}
                                ,{<<"text">>, <<"plain">>}
                               ]).
-define(PORT_WAITING, <<"waiting">>).
-define(PORT_READY, <<"ready">>).
-define(PORT_PROGRESS, <<"progress">>).
-define(PORT_COMPLETE, <<"completion">>).
-define(PORT_REJECT, <<"rejection">>).
-define(PORT_ATTACHMENT, <<"attachments">>).

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
    _ = couch_mgr:db_create(?KZ_PORT_REQUESTS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>),

    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.port_requests">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.port_requests">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.port_requests">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.port_requests">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.port_requests">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.port_requests">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.port_requests">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.port_requests">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.port_requests">>, ?MODULE, 'delete').

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
content_types_provided(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT) ->
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
    Context#cb_context{content_types_accepted=CTA}.

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
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Id, ?PORT_READY) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Id, ?PORT_PROGRESS) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Id, ?PORT_COMPLETE) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Id, ?PORT_REJECT) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT) ->
    summary_attachments(Id, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?PORT_ATTACHMENT) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    load_attachment(Id, AttachmentId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context.

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
    OldAttachments = wh_json:get_value(<<"_attachments">>, cb_context:doc(Context), wh_json:new()),

    _Del = [couch_mgr:delete_attachment(cb_context:account_db(Context), Id, Attachment)
            || Attachment <- wh_json:get_keys(OldAttachments)
           ],

    crossbar_doc:save_attachment(Id
                                 ,cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                 ,Opts
                                ).

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
            cb_context:set_resp_data(Context1, create_public_port_request_doc(cb_context:doc(Context1)));
        Context1 ->
            Context1
    end.

post(#cb_context{}=Context, Id, ?PORT_ATTACHMENT, _AttachmentName) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = wh_json:get_value(<<"contents">>, FileJObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    OldAttachments = wh_json:get_value(<<"_attachments">>, cb_context:doc(Context), wh_json:new()),
    Id = wh_json:get_value(<<"_id">>, cb_context:doc(Context)),
    _ = [couch_mgr:delete_attachment(cb_context:account_db(Context), Id, Attachment)
         || Attachment <- wh_json:get_keys(OldAttachments)
        ],
    crossbar_doc:save_attachment(Id, cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents, Context, Opts
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
    cb_context:set_doc(Context1, create_public_port_request_doc(cb_context:doc(Context1))).

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
    [create_public_port_request_doc(wh_json:get_value(<<"doc">>, Res)) | Acc].

-spec create_public_port_request_doc(wh_json:object()) -> wh_json:object().
create_public_port_request_doc(JObj) ->
    wh_json:set_values([{<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
                        ,{<<"created">>, wh_json:get_value(<<"pvt_created">>, JObj)}
                        ,{<<"updated">>, wh_json:get_value(<<"pvt_modified">>, JObj)}
                        ,{<<"port_state">>, wh_json:get_value(<<"pvt_port_state">>, JObj, ?PORT_WAITING)}
                       ], wh_doc:public_fields(JObj)).

-spec summary_attachments(ne_binary(), cb_context:context()) -> cb_context:context().
summary_attachments(Id, Context) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    cb_context:set_resp_data(Context1
                             ,wh_json:get_value(<<"_attachments">>, cb_context:doc(Context1), [])
                            ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    cb_context:set_doc(Context, wh_json:set_values([{<<"pvt_type">>, <<"port_request">>}
                                                    ,{<<"pvt_port_state">>, ?PORT_WAITING}
                                                   ], JObj));
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

load_attachment(Id, AttachmentId, Context) ->
    Context1 = read(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_attachment(AttachmentId, Context1);
        _ -> Context1
    end.

load_attachment(AttachmentId, Context) ->
    AttachmentMeta = wh_json:get_value([<<"_attachments">>, AttachmentId], cb_context:doc(Context)),

    lists:foldl(fun({K, V}, C) ->
                        cb_context:add_resp_header(K, V, C)
                end
                ,crossbar_doc:load_attachment(cb_context:doc(Context)
                                              ,AttachmentId
                                              ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                             )
                ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
                  ,{<<"Content-Type">>, wh_json:get_value([<<"content_type">>], AttachmentMeta)}
                  ,{<<"Content-Length">>, wh_json:get_value([<<"length">>], AttachmentMeta)}
                 ]).
