%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% Handles port request life cycles
%%% See doc/port_requests.md
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_port_requests).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/3, content_types_provided/4
         ,content_types_accepted/3, content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,get/3
         ,put/1, put/3
         ,patch/3
         ,post/2, post/4
         ,delete/2, delete/4
         ,cleanup/1
         ,find_template/1, find_template/2
         ,authority/1

         ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(MY_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".port_requests">>).

-define(TEMPLATE_DOC_ID, <<"notify.loa">>).
-define(TEMPLATE_ATTACHMENT_ID, <<"template">>).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}
                                ,{<<"text">>, <<"plain">>}
                                | ?PDF_CONTENT_TYPES
                               ]).

-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(ACCOUNTS_BY_SIMPLE_ID, <<"accounts/listing_by_simple_id">>).
-define(PORT_REQ_NUMBERS, <<"port_requests/port_in_numbers">>).
-define(ALL_PORT_REQ_NUMBERS, <<"port_requests/all_port_in_numbers">>).
-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).
-define(DESCENDANT_LISTING_BY_STATE, <<"port_requests/listing_by_descendant_state">>).

-define(DESCENDANTS, <<"descendants">>).

-define(UNFINISHED_PORT_REQUEST_LIFETIME
        ,kapps_config:get_integer(?MY_CONFIG_CAT, <<"unfinished_port_request_lifetime_s">>, ?SECONDS_IN_DAY * 30)
       ).

-define(PATH_TOKEN_LOA, <<"loa">>).

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
    knm_port_request:init(),

    Bindings = [{crossbar_cleanup:binding_system(), 'cleanup'}
                ,{<<"*.allowed_methods.port_requests">>, 'allowed_methods'}
                ,{<<"*.resource_exists.port_requests">>, 'resource_exists'}
                ,{<<"*.content_types_provided.port_requests">>, 'content_types_provided'}
                ,{<<"*.content_types_accepted.port_requests">>, 'content_types_accepted'}
                ,{<<"*.validate.port_requests">>, 'validate'}
                ,{<<"*.execute.get.port_requests">>, 'get'}
                ,{<<"*.execute.put.port_requests">>, 'put'}
                ,{<<"*.execute.patch.port_requests">>, 'patch'}
                ,{<<"*.execute.post.port_requests">>, 'post'}
                ,{<<"*.execute.delete.port_requests">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Cleanup expired port requests
%% @end
%%--------------------------------------------------------------------
-spec cleanup(ne_binary()) -> 'ok'.
-spec cleanup(ne_binary(), kz_json:objects()) -> 'ok'.

cleanup(?KZ_PORT_REQUESTS_DB = Db) ->
    ModifiedBefore = kz_util:current_tstamp() - ?UNFINISHED_PORT_REQUEST_LIFETIME,
    ViewOpts = [{'startkey', [0]}
                ,{'endkey', [ModifiedBefore]}
                ,{'limit', kz_datamgr:max_bulk_insert()}
                ,'include_docs'
               ],
    case kz_datamgr:get_results(Db, <<"port_requests/listing_by_modified">>, ViewOpts) of
        {'ok', []} -> lager:debug("no port requests older than ~p", [ModifiedBefore]);
        {'ok', OldPortReqeusts} -> cleanup(Db, OldPortReqeusts);
        {'error', _E} -> lager:debug("failed to query old port requests: ~p", [_E])
    end;
cleanup(_) -> 'ok'.

cleanup(Db, OldPortRequests) ->
    lager:debug("checking ~b old port requests", [length(OldPortRequests)]),

    Deletable = [kz_json:get_value(<<"doc">>, OldPortRequest)
                 || OldPortRequest <- OldPortRequests,
                    should_delete_port_request(kz_json:get_value(<<"key">>, OldPortRequest))
                ],
    lager:debug("found ~p deletable", [length(Deletable)]),
    kz_datamgr:del_docs(Db, Deletable),
    'ok'.

-spec should_delete_port_request([pos_integer() | ne_binary(),...]) -> boolean().
should_delete_port_request([_Modified, ?PORT_SUBMITTED]) ->
    'false';
should_delete_port_request([_Modified, ?PORT_SCHEDULED]) ->
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

allowed_methods(?PORT_SUBMITTED) ->
    [?HTTP_GET];
allowed_methods(?PORT_PENDING) ->
    [?HTTP_GET];
allowed_methods(?PORT_SCHEDULED) ->
    [?HTTP_GET];
allowed_methods(?PORT_COMPLETED) ->
    [?HTTP_GET];
allowed_methods(?PORT_REJECTED) ->
    [?HTTP_GET];
allowed_methods(?PORT_CANCELED) ->
    [?HTTP_GET];
allowed_methods(_PortRequestId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_PortRequestId, ?PORT_SUBMITTED) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_PENDING) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_SCHEDULED) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_COMPLETED) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_REJECTED) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_CANCELED) ->
    [?HTTP_PATCH];
allowed_methods(_PortRequestId, ?PORT_ATTACHMENT) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(_PortRequestId, ?PATH_TOKEN_LOA) ->
    [?HTTP_GET].

allowed_methods(_PortRequestId, ?PORT_ATTACHMENT, _AttachmentId) ->
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

resource_exists(_PortRequestId) -> 'true'.

resource_exists(_PortRequestId, ?PORT_SUBMITTED) -> 'true';
resource_exists(_PortRequestId, ?PORT_PENDING) -> 'true';
resource_exists(_PortRequestId, ?PORT_SCHEDULED) -> 'true';
resource_exists(_PortRequestId, ?PORT_COMPLETED) -> 'true';
resource_exists(_PortRequestId, ?PORT_REJECTED) -> 'true';
resource_exists(_PortRequestId, ?PORT_CANCELED) -> 'true';
resource_exists(_PortRequestId, ?PORT_ATTACHMENT) -> 'true';
resource_exists(_PortRequestId, ?PATH_TOKEN_LOA) -> 'true';
resource_exists(_PortRequestId, _) -> 'false'.

resource_exists(_PortRequestId, ?PORT_ATTACHMENT, _AttachmentId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec acceptable_content_types() -> kz_proplist().
acceptable_content_types() -> ?ATTACHMENT_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _Id, ?PATH_TOKEN_LOA) ->
    cb_context:add_content_types_provided(Context, [{'to_binary', ?PDF_CONTENT_TYPES}]).

content_types_provided(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET -> content_types_provided_get(Context, _Id, _AttachmentId);
        _Verb -> Context
    end.

-spec content_types_provided_get(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
content_types_provided_get(Context, Id, AttachmentId) ->
    cb_context:add_attachment_content_type(
      cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
      ,Id
      ,AttachmentId
     ).

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
content_types_accepted(Context, _Id, ?PORT_ATTACHMENT) ->
    CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
    cb_context:add_content_types_accepted(Context, CTA);
content_types_accepted(Context, _Id, _) ->
    Context.

content_types_accepted(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_POST ->
            CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
            cb_context:add_content_types_accepted(Context, CTA);
        _Verb ->
            Context
    end.

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
validate(Context) ->
    validate_port_request(Context, cb_context:req_verb(Context)).

validate(Context, ?PORT_UNCONFIRMED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_SUBMITTED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_PENDING = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_SCHEDULED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_COMPLETED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_REJECTED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, ?PORT_CANCELED = Type) ->
    validate_load_summary(Context, Type);
validate(Context, Id) ->
    validate_port_request(Context, Id, cb_context:req_verb(Context)).

validate(Context, Id, ?PORT_UNCONFIRMED) ->
    validate_port_request(Context, Id, ?PORT_UNCONFIRMED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_SUBMITTED) ->
    validate_port_request(Context, Id, ?PORT_SUBMITTED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_PENDING) ->
    validate_port_request(Context, Id, ?PORT_PENDING, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_SCHEDULED) ->
    validate_port_request(Context, Id, ?PORT_SCHEDULED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_COMPLETED) ->
    validate_port_request(Context, Id, ?PORT_COMPLETED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_REJECTED) ->
    validate_port_request(Context, Id, ?PORT_REJECTED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_CANCELED) ->
    validate_port_request(Context, Id, ?PORT_CANCELED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_ATTACHMENT) ->
    validate_attachments(Context, Id, cb_context:req_verb(Context));
validate(Context, Id, ?PATH_TOKEN_LOA) ->
    generate_loa(read(Context, Id)).

validate(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    validate_attachment(Context, Id, AttachmentId, cb_context:req_verb(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, Id, ?PATH_TOKEN_LOA) ->
    lager:debug("load LOA for ~s", [Id]),
    cb_context:set_resp_data(Context, kz_json:encode(cb_context:doc(Context))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(update_port_request_for_save(Context)).

-spec update_port_request_for_save(cb_context:context()) -> cb_context:context().
-spec update_port_request_for_save(cb_context:context(), kz_json:object()) -> cb_context:context().
update_port_request_for_save(Context) ->
    update_port_request_for_save(Context, cb_context:doc(Context)).
update_port_request_for_save(Context, Doc) ->
    NewDoc = kz_account:set_tree(Doc, kz_account:tree(cb_context:account_doc(Context))),
    cb_context:setters(Context
                       ,[{fun cb_context:set_account_db/2, ?KZ_PORT_REQUESTS_DB}
                         ,{fun cb_context:set_doc/2, NewDoc}
                        ]
                      ).

put(Context, Id, ?PORT_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),

    Contents = kz_json:get_value(<<"contents">>, FileJObj),

    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(<<"port_request">>)],

    crossbar_doc:save_attachment(Id
                                 ,cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                 ,Opts
                                ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context, Id, ?PORT_SUBMITTED) ->
    Callback = fun() -> patch_then_notify(Context, Id, ?PORT_SUBMITTED) end,
    crossbar_services:maybe_dry_run(Context, Callback);
patch(Context, Id, ?PORT_PENDING) ->
    patch_then_notify(Context, Id, ?PORT_PENDING);
patch(Context, Id, ?PORT_SCHEDULED) ->
    patch_then_notify(Context, Id, ?PORT_SCHEDULED);
patch(Context, Id, ?PORT_COMPLETED) ->
    patch_then_notify(Context, Id, ?PORT_COMPLETED);
patch(Context, Id, ?PORT_REJECTED) ->
    patch_then_notify(Context, Id, ?PORT_REJECTED);
patch(Context, Id, ?PORT_CANCELED) ->
    patch_then_notify(Context, Id, ?PORT_CANCELED).

%% @private
-spec patch_then_notify(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch_then_notify(Context, PortId, PortState) ->
    Context1 = do_patch(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            send_port_notification(Context1, PortId, PortState);
        _ -> Context1
    end.

%% @private
-spec do_patch(cb_context:context()) -> cb_context:context().
do_patch(Context) ->
    UpdatedDoc =
        kz_json:merge_recursive(
            cb_context:doc(Context)
            ,kz_json:public_fields(cb_context:req_data(Context))
        ),
    Context1 = crossbar_doc:save(update_port_request_for_save(Context, UpdatedDoc)),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(
                Context1
                ,knm_port_request:public_fields(cb_context:doc(Context1))
            );
        _Status ->
            Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, Id) ->
    do_post(Context, Id).

post(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(<<"port_request">>)],

    case kz_doc:attachment(cb_context:doc(Context), AttachmentId) of
        'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
        _AttachmentMeta ->
            lager:debug("deleting old attachment ~s", [AttachmentId]),
            kz_datamgr:delete_attachment(cb_context:account_db(Context), Id, AttachmentId)
    end,
    crossbar_doc:save_attachment(Id
                                 ,AttachmentId
                                 ,Contents
                                 ,Context
                                 ,Opts
                                ).

-spec do_post(cb_context:context(), path_token()) -> cb_context:context().
do_post(Context, Id) ->
    Context1 = crossbar_doc:save(update_port_request_for_save(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_port_comment_notification(Context1, Id),
            Doc = cb_context:doc(Context1),
            cb_context:set_resp_data(Context1, knm_port_request:public_fields(Doc));
        _Status ->
            Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) ->
                    cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) ->
                    cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).
delete(Context, Id, ?PORT_ATTACHMENT, AttachmentName) ->
    crossbar_doc:delete_attachment(Id, AttachmentName, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_port_request(cb_context:context(), ne_binary()) -> cb_context:context().
load_port_request(Context, Id) ->
    crossbar_doc:load(Id
                      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                      ,?TYPE_CHECK_OPTION(<<"port_request">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_load_summary(cb_context:context(), ne_binary()) ->
                                    cb_context:context().
validate_load_summary(Context, ?PORT_COMPLETED = Type) ->
    case cb_modules_util:range_view_options(Context, ?MAX_RANGE, <<"modified">>) of
        {From, To} -> load_summary_by_range(Context, Type, From, To);
        Context1 -> Context1
    end;
validate_load_summary(Context, ?PORT_CANCELED = Type) ->
    case cb_modules_util:range_view_options(Context, ?MAX_RANGE, <<"modified">>) of
        {From, To} -> load_summary_by_range(Context, Type, From, To);
        Context1 -> Context1
    end;
validate_load_summary(Context, <<_/binary>> = Type) ->
    lager:debug("loading summary for ~s", [Type]),
    load_summary(cb_context:set_should_paginate(Context, 'false')
                  ,[{'startkey', [cb_context:account_id(Context), Type, kz_json:new()]}
                    ,{'endkey', [cb_context:account_id(Context), Type]}
                   ]
                 ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_port_request(cb_context:context(), http_method()) ->
                                    cb_context:context().
-spec validate_port_request(cb_context:context(), ne_binary(), http_method()) ->
                                   cb_context:context().
-spec validate_port_request(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                   cb_context:context().
validate_port_request(Context, ?HTTP_GET) ->
    summary(Context);
validate_port_request(Context, ?HTTP_PUT) ->
    create(Context).

validate_port_request(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_port_request(Context, Id, ?HTTP_POST) ->
    update(Context, Id);
validate_port_request(Context, Id, ?HTTP_DELETE) ->
    is_deletable(load_port_request(Context, Id)).

validate_port_request(Context, Id, ?PORT_SUBMITTED, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_SUBMITTED);
validate_port_request(Context, Id, ?PORT_PENDING, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_PENDING);
validate_port_request(Context, Id, ?PORT_SCHEDULED, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_SCHEDULED);
validate_port_request(Context, Id, ?PORT_COMPLETED, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_COMPLETED);
validate_port_request(Context, Id, ?PORT_REJECTED, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_REJECTED);
validate_port_request(Context, Id, ?PORT_CANCELED, ?HTTP_PATCH) ->
    maybe_move_state(Context, Id, ?PORT_CANCELED).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_attachments(cb_context:context(), ne_binary(), http_method()) ->
                                 cb_context:context().
validate_attachments(Context, Id, ?HTTP_GET) ->
    summary_attachments(Context, Id);
validate_attachments(Context, Id, ?HTTP_PUT) ->
    read(Context, Id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_attachment(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                 cb_context:context().
validate_attachment(Context, Id, AttachmentId, ?HTTP_GET) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_POST) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_DELETE) ->
    is_deletable(load_attachment(Id, AttachmentId, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_deletable(cb_context:context()) -> cb_context:context().
-spec is_deletable(cb_context:context(), ne_binary()) -> cb_context:context().
is_deletable(Context) ->
    is_deletable(Context, knm_port_request:current_state(cb_context:doc(Context))).
is_deletable(Context, ?PORT_UNCONFIRMED) -> Context;
is_deletable(Context, ?PORT_REJECTED) -> Context;
is_deletable(Context, ?PORT_CANCELED) -> Context;
is_deletable(Context, _PortState) ->
    lager:debug("port is in state ~s, can't modify", [_PortState]),
    cb_context:add_system_error('invalid_method'
                                ,<<"port request is not modifiable in this state">>
                                ,Context
                               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C, 'undefined') end,
    cb_context:validate_request_data(<<"port_requests">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 = load_port_request(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            PubDoc = knm_port_request:public_fields(cb_context:doc(Context1)),
            cb_context:set_resp_data(cb_context:set_doc(Context1, PubDoc)
                                     ,PubDoc
                                    );
        _ -> Context1
    end.

-spec authority(ne_binary()) -> api_binary().
authority(AccountId) ->
  case kz_whitelabel:fetch(AccountId) of
      {'error', _R} -> 'undefined';
      {'ok', JObj} ->
          kz_whitelabel:port_authority(JObj)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(C, Id) end,
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
    case cb_context:req_value(Context, <<"by_number">>) of
        'undefined' -> load_summary_by_range(Context);
        Number -> load_summary_by_number(Context, Number)
    end.

-spec load_summary_by_range(cb_context:context()) -> cb_context:context().
-spec load_summary_by_range(cb_context:context(), gregorian_seconds(), gregorian_seconds()) -> cb_context:context().
-spec load_summary_by_range(cb_context:context(), ne_binary(), gregorian_seconds(), gregorian_seconds()) -> cb_context:context().
load_summary_by_range(Context) ->
    case cb_modules_util:range_view_options(Context, ?MAX_RANGE, <<"modified">>) of
        {From, To} -> load_summary_by_range(Context, From, To);
        Context1 -> Context1
    end.

load_summary_by_range(Context, From, To) ->
    lager:debug("loading summary for all port requests from ~p to ~p", [From, To]),
    normalize_summary_results(
      lists:foldl(fun(?PORT_SUBMITTED=Type, C) -> load_summary_fold(C, Type);
                     (?PORT_PENDING=Type, C) -> load_summary_fold(C, Type);
                     (?PORT_SCHEDULED=Type, C) -> load_summary_fold(C, Type);
                     (?PORT_REJECTED=Type, C) -> load_summary_fold(C, Type);
                     (Type, C) ->
                          load_summary_by_range_fold(C, Type, From, To)
                  end,
                  cb_context:setters(
                    Context,
                    [{fun cb_context:set_resp_data/2, []}
                    ,{fun cb_context:set_resp_status/2, 'success'}
                    ]
                   ),
                  ?PORT_STATES
                 )
     ).

load_summary_by_range(Context, Type, From, To) ->
    load_summary_by_range(Context, Type, From, To, 'true').

load_summary_by_range(Context, Type, From, To, Normalize) ->
    lager:debug("loading summary for ~s from ~p to ~p", [Type, From, To]),
    load_summary(Context
                ,[{'startkey', [cb_context:account_id(Context), Type, To]}
                 ,{'endkey', [cb_context:account_id(Context), Type, From]}
                 ,{'normalize', Normalize}
                 ]
                ).

-spec load_summary_fold(cb_context:context(), ne_binary()) -> cb_context:context().
load_summary_fold(Context, Type) ->
    Summary = cb_context:resp_data(Context),
    Props =
        [{'startkey', [cb_context:account_id(Context), Type, kz_json:new()]}
        ,{'endkey', [cb_context:account_id(Context), Type]}
        ,{'normalize', 'false'}
        ],
    case cb_context:resp_data(
           load_summary(cb_context:set_should_paginate(Context, 'false'), Props)
          )
    of
        TypeSummary when is_list(TypeSummary) ->
            cb_context:set_resp_data(Context, Summary ++ TypeSummary);
        _Else -> Context
    end.

-spec load_summary_by_range_fold(cb_context:context(), ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                                        cb_context:context().
load_summary_by_range_fold(Context, Type, From, To) ->
    Summary = cb_context:resp_data(Context),
    case cb_context:resp_data(
           load_summary_by_range(Context, Type, From, To, 'false')
          )
    of
        TypeSummary when is_list(TypeSummary) ->
            cb_context:set_resp_data(Context, Summary ++ TypeSummary);
        _Else -> Context
    end.

-spec load_summary_by_number(cb_context:context(), ne_binary()) -> cb_context:context().
load_summary_by_number(Context, Number) ->
    case should_summarize_descendant_requests(Context) of
        'true' -> summary_descendants_by_number(Context, Number);
        'false' -> summary_by_number(Context, Number)
    end.

-spec load_summary(cb_context:context(), crossbar_doc:view_options()) ->
                           cb_context:context().
load_summary(Context, ViewOptions) ->
    View = case should_summarize_descendant_requests(Context) of
               'true' -> ?DESCENDANT_LISTING_BY_STATE;
               'false' -> ?LISTING_BY_STATE
           end,
    maybe_normalize_summary_results(
      crossbar_doc:load_view(View
                             ,['include_docs'
                               ,'descending'
                               | ViewOptions
                              ]
                             ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                             ,fun normalize_view_results/2
                            )
      ,props:get_value('normalize', ViewOptions, 'true')
     ).

-spec maybe_normalize_summary_results(cb_context:context(), boolean()) -> cb_context:context().
maybe_normalize_summary_results(Context, 'false') -> Context;
maybe_normalize_summary_results(Context, 'true') ->
    case cb_context:resp_status(Context) of
        'success' -> normalize_summary_results(Context);
        _Else -> Context
    end.

-spec normalize_summary_results(cb_context:context()) -> cb_context:context().
normalize_summary_results(Context) ->
    Dict = lists:foldl(
             fun(JObj, D) ->
                     AccountId = kz_json:get_value(<<"account_id">>, JObj),
                     dict:append_list(AccountId, [JObj], D)
             end, dict:new(), cb_context:resp_data(Context)),
    Names = get_account_names(dict:fetch_keys(Dict)),
    cb_context:set_resp_data(
      Context,
      [kz_json:from_list(
         [{<<"account_id">>, AccountId}
          ,{<<"account_name">>, props:get_value(AccountId, Names, <<"unknown">>)}
          ,{<<"port_requests">>, JObjs}
         ]
        )
       || {AccountId, JObjs} <- dict:to_list(Dict)
      ]
     ).

-spec get_account_names(ne_binaries()) -> kz_proplist().
get_account_names(Keys) ->
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACCOUNTS_BY_SIMPLE_ID, Keys) of
        {'ok', JObjs} ->
            [{kz_json:get_value(<<"id">>, JObj)
              ,kz_json:get_value([<<"value">>, <<"name">>], JObj)
             }
             || JObj <- JObjs
            ];
        {'error', _} -> []
    end.

-spec should_summarize_descendant_requests(cb_context:context()) -> boolean().
should_summarize_descendant_requests(Context) ->
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        [_AccountId, ?DESCENDANTS] -> 'true';
        _Params -> 'false'
    end.

-spec summary_by_number(cb_context:context(), ne_binary()) ->
                               cb_context:context().
summary_by_number(Context, Number) ->
    ViewOptions = [{'keys', build_keys(Context, Number)}
                   ,'include_docs'
                  ],
    crossbar_doc:load_view(
      ?ALL_PORT_REQ_NUMBERS
      ,ViewOptions
      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
      ,fun normalize_view_results/2
     ).

-spec summary_descendants_by_number(cb_context:context(), ne_binary()) ->
                                           cb_context:context().
summary_descendants_by_number(Context, Number) ->
    ViewOptions = [{'keys', build_keys(Context, Number)}
                   ,'include_docs'
                  ],
    crossbar_doc:load_view(
      ?ALL_PORT_REQ_NUMBERS
      ,ViewOptions
      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
      ,fun normalize_view_results/2
     ).

-type descendant_keys() :: [ne_binaries()].

-spec build_keys(cb_context:context(), ne_binary()) ->
                        descendant_keys().
build_keys(Context, Number) ->
    build_keys_from_account(
      knm_converters:normalize(Number)
      ,props:get_value(<<"accounts">>, cb_context:req_nouns(Context))
     ).

-spec build_keys_from_account(ne_binary(), ne_binaries()) ->
                                     descendant_keys().
build_keys_from_account(E164, [AccountId]) ->
    [[AccountId, E164]];
build_keys_from_account(E164, [AccountId, ?PORT_DESCENDANTS]) ->
    ViewOptions = [{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(
           ?KZ_ACCOUNTS_DB
           ,?AGG_VIEW_DESCENDANTS
           ,ViewOptions
          )
    of
        {'error', _R} ->
            lager:error("failed to query view ~p", [_R]),
            [];
        {'ok', JObjs} ->
            lists:foldl(
              fun(JObj, Acc) ->
                      build_descendant_key(JObj, Acc, E164)
              end
              ,[]
              ,JObjs
             )
    end.

-spec build_descendant_key(kz_json:object(), descendant_keys(), ne_binary()) ->
                                  descendant_keys().
build_descendant_key(JObj, Acc, E164) ->
    [[kz_doc:id(JObj), E164]
     |Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(Res, Acc) ->
    [leak_pvt_fields(
       Res
       ,knm_port_request:public_fields(kz_json:get_value(<<"doc">>, Res))
      )
     | Acc
    ].

-spec leak_pvt_fields(kz_json:object(), kz_json:object()) -> kz_json:object().
leak_pvt_fields(Res, JObj) ->
    Fields = [{[<<"doc">>, <<"pvt_account_id">>], <<"account_id">>}],
    lists:foldl(
      fun({Field, Key}, J) ->
              case kz_json:get_ne_value(Field, Res) of
                  'undefined' -> J;
                  Value -> kz_json:set_value(Key, Value, J)
              end
      end
      ,JObj
      ,Fields
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec summary_attachments(cb_context:context(), ne_binary()) -> cb_context:context().
summary_attachments(Context, Id) ->
    Context1 = load_port_request(Context, Id),
    As = kz_doc:attachments(cb_context:doc(Context1), kz_json:new()),
    cb_context:set_resp_data(Context1
                             ,knm_port_request:normalize_attachments(As)
                            ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(cb_context:context(), api_binary()) -> cb_context:context().
-spec on_successful_validation(cb_context:context(), api_binary(), boolean()) -> cb_context:context().
on_successful_validation(Context, 'undefined') ->
    on_successful_validation(Context, 'undefined', 'true');
on_successful_validation(Context, Id) ->
    Context1 = crossbar_doc:load_merge(Id
                                       ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                       ,?TYPE_CHECK_OPTION(<<"port_request">>)),
    on_successful_validation(Context1, Id, can_update_port_request(Context1)).

on_successful_validation(Context, Id, 'true') ->
    JObj = cb_context:doc(Context),
    Numbers = kz_json:get_keys(kz_json:get_value(<<"numbers">>, JObj)),

    Context1 = lists:foldl(fun(Number, ContextAcc) ->
                                   check_number_portability(Id, Number, ContextAcc)
                           end
                           ,Context
                           ,Numbers
                          ),

    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("number(s) checked out for ~s", [Id]),
            successful_validation(Context, Id);
        _ -> Context1
    end;
on_successful_validation(Context, _Id, 'false') ->
    PortState = kz_json:get_value(?PORT_PVT_STATE, cb_context:doc(Context)),
    lager:debug(
      "port state ~s is not valid for updating a port request"
      ,[PortState]
     ),

    cb_context:add_validation_error(
      PortState
      ,<<"type">>
      ,kz_json:from_list(
         [{<<"message">>, <<"Updating port requests not allowed in current port state">>}
          ,{<<"cause">>, PortState}
         ])
      ,Context
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec can_update_port_request(cb_context:context()) -> boolean().
-spec can_update_port_request(cb_context:context(), ne_binary()) -> boolean().
can_update_port_request(Context) ->
    lager:debug("port request: ~p", [cb_context:doc(Context)]),
    can_update_port_request(Context, knm_port_request:current_state(cb_context:doc(Context))).

can_update_port_request(_Context, ?PORT_UNCONFIRMED) ->
    'true';
can_update_port_request(_Context, ?PORT_REJECTED) ->
    'true';
can_update_port_request(Context, _) ->
    cb_modules_util:is_superduper_admin(cb_context:auth_account_id(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec successful_validation(cb_context:context(), api_binary()) -> cb_context:context().
successful_validation(Context, 'undefined') ->
    Normalized = knm_port_request:normalize_numbers(cb_context:doc(Context)),
    Unconf = [{<<"pvt_type">>, <<"port_request">>}
              ,{?PORT_PVT_STATE, ?PORT_UNCONFIRMED}
             ],
    cb_context:set_doc(Context, kz_json:set_values(Unconf, Normalized));
successful_validation(Context, _Id) ->
    Normalized = knm_port_request:normalize_numbers(cb_context:doc(Context)),
    cb_context:set_doc(Context, Normalized).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_number_portability(api_binary(), ne_binary(), cb_context:context()) ->
                                      cb_context:context().
-spec check_number_portability(api_binary(), ne_binary(), cb_context:context(), ne_binary(), kz_json:object()) ->
                                      cb_context:context().
check_number_portability(PortId, Number, Context) ->
    E164 = knm_converters:normalize(Number),
    lager:debug("checking ~s(~s) for portability", [E164, Number]),
    PortOptions = [{'key', E164}],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?PORT_REQ_NUMBERS, PortOptions) of
        {'ok', []} -> check_number_existence(E164, Number, Context);
        {'ok', [PortReq]} ->
            check_number_portability(PortId, Number, Context, E164, PortReq);
        {'ok', [_|_]=_PortReqs} ->
            Message = <<"Number is currently on multiple port requests. Contact a system admin to rectify">>,
            lager:debug("number ~s(~s) exists on multiple port request docs. That's bad!", [E164, Number]),
            number_validation_error(Context, Number, Message);
        {'error', _E} ->
            Message = <<"Failed to query back-end services, cannot port at this time">>,
            lager:debug("failed to query the port request view: ~p", [_E]),
            number_validation_error(Context, Number, Message)
    end.

check_number_portability(PortId, Number, Context, E164, PortReq) ->
    case {kz_json:get_value(<<"value">>, PortReq) =:= cb_context:account_id(Context)
          ,kz_doc:id(PortReq) =:= PortId
         }
    of
        {'true', 'true'} ->
            lager:debug(
              "number ~s(~s) is on this existing port request for this account(~s)"
              ,[E164, Number, cb_context:account_id(Context)]
             ),
            cb_context:set_resp_status(Context, 'success');
        {'true', 'false'} ->
            lager:debug(
              "number ~s(~s) is on a different port request in this account(~s): ~s"
              ,[E164, Number, cb_context:account_id(Context), kz_doc:id(PortReq)]
             ),
            Message = <<"Number is on a port request already: ", (kz_doc:id(PortReq))/binary>>,
            number_validation_error(Context, Number, Message);
        {'false', _} ->
            lager:debug(
              "number ~s(~s) is on existing port request for other account(~s)"
              ,[E164, Number, kz_json:get_value(<<"value">>, PortReq)]
             ),
            number_validation_error(Context, Number, <<"Number is being ported for a different account">>)
    end.

-spec number_validation_error(cb_context:context(), ne_binary(), ne_binary()) ->
                                     cb_context:context().
number_validation_error(Context, Number, Message) ->
    JObj = kz_json:from_list([{<<"message">>, Message}
                              ,{<<"cause">>, Number}
                             ]),
    cb_context:add_validation_error(Number, <<"type">>, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_number_existence(ne_binary(), ne_binary(), cb_context:context()) ->
                                    cb_context:context().
check_number_existence(E164, Number, Context) ->
    case knm_number:lookup_account(E164) of
        {'ok', _AccountId, _} ->
            lager:debug("number ~s exists and belongs to ~s", [E164, _AccountId]),
            number_validation_error(Context, Number, <<"Number exists on the system already">>);
        {'error', 'not_found'} ->
            lager:debug("number ~s not found in numbers db (portable!)", [E164]),
            cb_context:set_resp_status(Context, 'success');
        {'error', 'unassigned'} ->
            lager:debug("number ~s not assigned to an account (portable!)", [E164]),
            cb_context:set_resp_status(Context, 'success');
        {'error', E} ->
            lager:debug("number ~s error-ed when looking up: ~p", [E164, E]),
            number_validation_error(Context, Number, kz_util:to_binary(E))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_attachment(ne_binary(), ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(Id, AttachmentId, Context) ->
    Context1 = read(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> load_attachment(AttachmentId, Context1);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_attachment(ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(AttachmentId, Context) ->
    cb_context:add_resp_headers(
      crossbar_doc:load_attachment(cb_context:doc(Context)
                                   ,AttachmentId
                                   ,?TYPE_CHECK_OPTION(<<"port_request">>)
                                   ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                  )
      ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
        ,{<<"Content-Type">>, kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)}
        ,{<<"Content-Length">>, kz_doc:attachment_length(cb_context:doc(Context), AttachmentId)}
       ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_move_state(cb_context:context(), ne_binary(), ne_binary()) ->
                              cb_context:context().
maybe_move_state(Context, Id, PortState) ->
    Context1 = load_port_request(Context, Id),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso knm_port_request:maybe_transition(cb_context:doc(Context1), PortState)
    of
        'false' -> Context1;
        {'ok', PortRequest} ->
            lager:debug("loaded new port request state ~s", [PortState]),
            cb_context:set_doc(Context1, PortRequest);
        {'error', 'invalid_state_transition'} ->
            cb_context:add_validation_error(
              <<"port_state">>
              ,<<"enum">>
              ,kz_json:from_list(
                 [{<<"message">>, <<"Cannot move to new state from current state">>}
                  ,{<<"cause">>, PortState}
                 ])
              ,Context
             );
        {'error', 'failed_to_charge'} ->
            cb_context:add_system_error('no_credit', Context);
        {'errors', Errors} ->
            JObj = kz_json:from_list([{<<"message">>, kz_json:from_list(Errors)}]),
            cb_context:add_system_error('transition_errors', JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec generate_loa(cb_context:context()) ->
                          cb_context:context().
-spec generate_loa(cb_context:context(), crossbar_status()) ->
                          cb_context:context().
generate_loa(Context) ->
    generate_loa(Context, cb_context:resp_status(Context)).
generate_loa(Context, 'success') ->
    generate_loa_from_port(Context, cb_context:doc(Context));
generate_loa(Context, _RespStatus) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_template(ne_binary()) -> ne_binary().
-spec find_template(ne_binary(), api_binary()) -> ne_binary().
find_template(ResellerId) ->
    {'ok', Template} = kz_pdf:find_template(ResellerId, <<"loa">>),
    Template.

find_template(ResellerId, 'undefined') ->
    find_template(ResellerId);
find_template(ResellerId, CarrierName) ->
    TemplateName = <<(kz_util:to_lower_binary(kz_util:uri_encode(CarrierName)))/binary, ".tmpl">>,
    lager:debug("looking for carrier template ~s or plain template for reseller ~s"
                ,[TemplateName, ResellerId]
               ),
    case kz_pdf:find_template(ResellerId, <<"loa">>, TemplateName) of
        {'error', _} -> find_template(ResellerId);
        {'ok', Template} -> Template
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_port_comment_notification(cb_context:context(), ne_binary()) -> 'ok'.
maybe_send_port_comment_notification(Context, Id) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    ReqData = cb_context:req_data(Context),
    DbDocComments = kz_json:get_value(<<"comments">>, DbDoc),
    ReqDataComments = kz_json:get_value(<<"comments">>, ReqData),
    case has_new_comment(DbDocComments, ReqDataComments) of
        'false' -> lager:debug("no new comments in ~s, ignoring", [Id]);
        'true' ->
            try send_port_comment_notification(Context, Id) of
                _ -> lager:debug("port comment notification sent")
            catch
                _E:_R ->
                    lager:error("failed to send the port comment notification: ~s:~p", [_E, _R])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_new_comment(api_objects(), api_objects()) -> boolean().
has_new_comment('undefined', [_|_]) -> 'true';
has_new_comment([], [_|_]) -> 'true';
has_new_comment(_, 'undefined') -> 'false';
has_new_comment(_, []) -> 'false';
has_new_comment(OldComments, NewComments) ->
    OldTime = kz_json:get_value(<<"timestamp">>, lists:last(OldComments)),
    NewTime = kz_json:get_value(<<"timestamp">>, lists:last(NewComments)),

    OldTime < NewTime.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_notification(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec send_port_notification(cb_context:context(), path_token(), path_token(), function()) -> cb_context:context().
send_port_notification(Context, Id, ?PORT_SUBMITTED=State) ->
     _  = add_to_phone_numbers_doc(Context),
    send_port_notification(Context, Id, State, fun send_port_request_notification/2);
send_port_notification(Context, Id, ?PORT_PENDING=State) ->
    send_port_notification(Context, Id, State, fun send_port_pending_notification/2);
send_port_notification(Context, Id, ?PORT_SCHEDULED=State) ->
    send_port_notification(Context, Id, State, fun send_port_scheduled_notification/2);
send_port_notification(Context, Id, ?PORT_COMPLETED=State) ->
    send_port_notification(Context, Id, State, fun send_ported_notification/2);
send_port_notification(Context, Id, ?PORT_REJECTED=State) ->
    send_port_notification(Context, Id, State, fun send_port_rejected_notification/2);
send_port_notification(Context, Id, ?PORT_CANCELED=State) ->
    _ = remove_from_phone_numbers_doc(Context),
    send_port_notification(Context, Id, State, fun send_port_cancel_notification/2).

send_port_notification(Context, Id, State, Fun) ->
    try Fun(Context, Id) of
        _ ->
            lager:debug("port ~s notification sent", [State]),
            Context
    catch
        _E:_R ->
            lager:debug("failed to send the  port ~s notification: ~s:~p", [State, _E, _R]),
            _ = revert_patch(Context),
            cb_context:add_system_error(
              'bad_gateway'
              ,kz_json:from_list([{<<"message">>, <<"failed to send port ", State/binary,  " email">>}])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec revert_patch(cb_context:context()) -> cb_context:context().
revert_patch(Context) ->
    Doc = cb_context:doc(Context),
    DBDoc = cb_context:fetch(Context, 'db_doc'),

    Rev = kz_doc:revision(Doc),

    RevertedDoc = kz_doc:set_revision(DBDoc, Rev),

    crossbar_doc:save(cb_context:set_doc(Context, RevertedDoc)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_comment_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_comment_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           ,{<<"Version">>, cb_context:api_version(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_comment/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_request_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_request_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           ,{<<"Version">>, cb_context:api_version(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_request/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_pending_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_pending_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           ,{<<"Version">>, cb_context:api_version(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_pending/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_rejected_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_rejected_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_rejected/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_cancel_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_cancel_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_cancel/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_ported_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_ported_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_ported/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_scheduled_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_scheduled_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
           ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
           ,{<<"Port-Request-ID">>, Id}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_port_scheduled/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_to_phone_numbers_doc(cb_context:context()) -> 'ok' | 'error'.
-spec add_to_phone_numbers_doc(cb_context:context(), kz_json:object()) -> 'ok' | 'error'.
add_to_phone_numbers_doc(Context) ->
    case get_phone_numbers_doc(Context) of
        {'error', _R} -> 'error';
        {'ok', JObj} ->
            add_to_phone_numbers_doc(Context, JObj)
    end.

add_to_phone_numbers_doc(Context, JObj) ->
    AccountId = cb_context:account_id(Context),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    PhoneNumbersJObj =
        kz_json:foldl(
          fun(Number, _, Acc) ->
                  NumberJObj = build_number_properties(AccountId, Now),
                  kz_json:set_value(Number, NumberJObj, Acc)
          end
          ,JObj
          ,kz_json:get_value(<<"numbers">>, cb_context:doc(Context), kz_json:new())
         ),
    save_phone_numbers_doc(Context, PhoneNumbersJObj).

-spec build_number_properties(ne_binary(), gregorian_seconds()) -> kz_json:object().
build_number_properties(AccountId, Now) ->
    kz_json:from_list(
      [{<<"state">>, ?NUMBER_STATE_PORT_IN}
       ,{<<"features">>, kz_json:new()}
       ,{<<"assigned_to">>, AccountId}
       ,{<<"created">>, Now}
       ,{<<"updated">>, Now}
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_from_phone_numbers_doc(cb_context:context()) -> 'ok' | 'error'.
-spec remove_from_phone_numbers_doc(cb_context:context(), kz_json:object()) -> 'ok' | 'error'.
remove_from_phone_numbers_doc(Context) ->
    case get_phone_numbers_doc(Context) of
        {'error', _R}-> 'ok';
        {'ok', JObj} ->
            remove_from_phone_numbers_doc(Context, JObj)
    end.

remove_from_phone_numbers_doc(Context, JObj) ->
    {Updated, PhoneNumbersJObj} =
        kz_json:foldl(fun remove_phone_number/3
                      ,{'false', JObj}
                      ,kz_json:get_value(<<"numbers">>, cb_context:doc(Context), kz_json:new())
                     ),
    case Updated of
        'true' ->
            save_phone_numbers_doc(Context, PhoneNumbersJObj);
        'false' ->
            lager:debug("no numbers removed, not updating")
    end.

-spec remove_phone_number(kz_json:key(), kz_json:json_term(), {boolean(), kz_json:object()}) ->
                                 {'true', kz_json:object()}.
remove_phone_number(Number, _, {_, Acc}) ->
    {'true', kz_json:delete_key(Number, Acc)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_phone_numbers_doc(cb_context:context()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
get_phone_numbers_doc(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Context1 = crossbar_doc:load(?KNM_PHONE_NUMBERS_DOC
                                 ,cb_context:set_account_db(Context, AccountDb)
                                 ,?TYPE_CHECK_OPTION(<<"phone_numbers">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            {'ok', cb_context:doc(Context1)};
        Status ->
            lager:error("failed to open ~s doc in ~s : ~p", [?KNM_PHONE_NUMBERS_DOC, AccountId, Status]),
            {'error', Status}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_phone_numbers_doc(cb_context:context(), kz_json:object()) -> 'ok' | 'error'.
save_phone_numbers_doc(Context, JObj) ->
    AccountId = cb_context:account_id(Context),
    Context1 =
        cb_context:setters(
          Context
          ,[{fun cb_context:set_doc/2, JObj}
            ,{fun cb_context:set_account_db/2, kz_util:format_account_id(AccountId, 'encoded')}
           ]
         ),

    case cb_context:resp_status(crossbar_doc:save(Context1)) of
        'success' -> 'ok';
        _Status ->
            lager:error("failed to save ~s doc in ~s : ~p"
                        ,[?KNM_PHONE_NUMBERS_DOC, AccountId, _Status]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec generate_loa_from_port(cb_context:context(), kz_json:object()) ->
                                    cb_context:context().
generate_loa_from_port(Context, PortRequest) ->
    AccountId = cb_context:account_id(Context),

    ResellerId = kz_services:find_reseller_id(AccountId),
    ResellerDoc = cb_context:account_doc(cb_context:set_account_id(Context, ResellerId)),

    AccountDoc = cb_context:account_doc(Context),

    Numbers = [knm_util:pretty_print(N) || N <- kz_json:get_keys(<<"numbers">>, PortRequest)],

    QRCode = create_qr_code(cb_context:account_id(Context), kz_doc:id(PortRequest)),

    generate_loa_from_template(Context
                               ,props:filter_undefined(
                                  [{<<"reseller">>, kz_json:to_proplist(ResellerDoc)}
                                   ,{<<"account">>, kz_json:to_proplist(AccountDoc)}
                                   ,{<<"numbers">>, Numbers}
                                   ,{<<"bill">>, kz_json:to_proplist(kz_json:get_value(<<"bill">>, PortRequest, kz_json:new()))}
                                   ,{<<"request">>, kz_json:to_proplist(PortRequest)}
                                   ,{<<"qr_code">>, QRCode}
                                   ,{<<"type">>, <<"loa">>}
                                  ])
                               ,ResellerId
                               ,kz_json:get_value(<<"carrier">>, PortRequest)
                              ).

-spec generate_loa_from_template(cb_context:context(), kz_proplist(), ne_binary(), api_binary()) ->
                                        cb_context:context().
generate_loa_from_template(Context, TemplateData, ResellerId, Carrier) ->
    Template = find_template(ResellerId, Carrier),
    case kz_pdf:generate(ResellerId, TemplateData, Template) of
        {'error', _R} -> cb_context:set_resp_status(Context, 'error');
        {'ok', PDF} ->
            cb_context:set_resp_status(
              cb_context:set_resp_data(Context, PDF)
              ,'success'
             )
    end.

-spec create_qr_code(api_binary(), api_binary()) -> kz_proplist() | 'undefined'.
create_qr_code('undefined', _) -> 'undefined';
create_qr_code(_, 'undefined') -> 'undefined';
create_qr_code(AccountId, PortRequestId) ->
    lager:debug("create qr code for ~s - ~s", [AccountId, PortRequestId]),
    CHL = <<AccountId/binary, "-", PortRequestId/binary>>,
    Url = <<"https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=", CHL/binary, "&choe=UTF-8">>,

    case kz_http:get(kz_util:to_list(Url)) of
        {'ok', 200, _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s: ~s", [Url, RespBody]),
            [{<<"image">>, base64:encode(RespBody)}];
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            'undefined'
    end.
