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
%%% PUT /port_request/{id}/attachment - upload a document
%%% GET /port_request/{id}/attachment/{attachment_id} - download the document
%%% POST /port_request/{id}/attachment/{attachment_id} - replace a document
%%% DELETE /port_request/{id}/attachment/{attachment_id} - delete a document
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
         ,content_types_provided/1, content_types_provided/4
         ,content_types_accepted/3, content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,get/1, get/2, get/4
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
-define(PORT_ATTACHMENT, <<"attachment">>).

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
    [?HTTP_PUT].

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
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{}=Context) ->
    Context.

content_types_provided(#cb_context{req_verb=?HTTP_GET}=Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    case read(Id, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            ContentTypeKey = [<<"_attachments">>, AttachmentId, <<"content_type">>],
            case wh_json:get_value(ContentTypeKey, JObj) of
                'undefined' -> Context;
                ContentType ->
                    [Type, SubType] = binary:split(ContentType, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end
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
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Id, ?PORT_ATTACHMENT) ->
    Context.

validate(#cb_context{req_verb = ?HTTP_GET}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context;
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context()) -> cb_context:context().
-spec get(cb_context:context(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _Id) ->
    Context.
get(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentName) ->
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
put(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

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
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).
post(#cb_context{}=Context, _Id, ?PORT_ATTACHMENT, _AttachmentName) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

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
    crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

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
                       ], wh_doc:public_fields(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_values([{<<"pvt_type">>, <<"port_request">>}
                                               ,{<<"state">>, ?PORT_WAITING}
                                              ], JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).
