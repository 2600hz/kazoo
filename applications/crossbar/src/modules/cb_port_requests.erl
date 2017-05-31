%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz INC
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
        ,authority/1

        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(TEMPLATE_DOC_ID, <<"notify.loa">>).
-define(TEMPLATE_ATTACHMENT_ID, <<"template">>).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"octet-stream">>}
                               ,{<<"text">>, <<"plain">>}
                                | ?PDF_CONTENT_TYPES
                               ]).

-define(ACCOUNTS_BY_SIMPLE_ID, <<"accounts/listing_by_simple_id">>).
-define(PORT_REQ_NUMBERS, <<"port_requests/port_in_numbers">>).
-define(ALL_PORT_REQ_NUMBERS, <<"port_requests/all_port_in_numbers">>).
-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).
-define(DESCENDANT_LISTING_BY_STATE, <<"port_requests/listing_by_descendant_state">>).

-define(DESCENDANTS, <<"descendants">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.port_requests">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.port_requests">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.port_requests">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.port_requests">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.port_requests">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.port_requests">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.port_requests">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.port_requests">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.post.port_requests">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.port_requests">>, ?MODULE, 'delete').

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
allowed_methods(?PORT_UNCONFIRMED) ->
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
    cb_context:add_content_types_provided(Context, [{'to_binary', ?PDF_CONTENT_TYPES}]);
content_types_provided(Context, _Id, _Path) ->
    Context.

content_types_provided(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET -> content_types_provided_get(Context, _Id, _AttachmentId);
        _Verb -> Context
    end.

-spec content_types_provided_get(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
content_types_provided_get(Context, Id, AttachmentId) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    cb_context:add_attachment_content_type(Context1, Id, AttachmentId).

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
    AName = cb_modules_util:attachment_name(Filename, CT),
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:save_attachment(Id, AName, Contents, Context1, Opts).

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
    maybe_patch_to_scheduled(Context, Id);
patch(Context, Id, ?PORT_COMPLETED) ->
    patch_then_notify(Context, Id, ?PORT_COMPLETED);
patch(Context, Id, ?PORT_REJECTED) ->
    patch_then_notify(Context, Id, ?PORT_REJECTED);
patch(Context, Id, ?PORT_CANCELED) ->
    patch_then_notify(Context, Id, ?PORT_CANCELED).

-spec maybe_patch_to_scheduled(cb_context:context(), path_token()) -> cb_context:context().
maybe_patch_to_scheduled(Context, Id) ->
    JObj = cb_context:req_data(Context),
    case kz_json:get_value(<<"scheduled_date">>, JObj) of
        'undefined' ->
            cb_context:add_validation_error(<<"error">>
                                           ,<<"type">>
                                           ,kz_json:from_list([{<<"message">>, <<"Schedule update missing parameters">>}
                                                              ,{<<"missing">>, <<"scheduled_date">>}
                                                              ])
                                           ,Context);

        _Scheduled ->
            patch_then_notify(Context, Id, ?PORT_SCHEDULED)
    end.

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
        kz_json:merge(cb_context:doc(Context)
                     ,kz_doc:public_fields(cb_context:req_data(Context))
                     ),

    Context1  = crossbar_doc:save(update_port_request_for_save(Context, UpdatedDoc)),
    RespData1 = knm_port_request:public_fields(cb_context:doc(Context1)),
    RespData2 = filter_private_comments(Context1, RespData1),

    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1, RespData2);

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
    Context1 = crossbar_doc:save(update_port_request_for_save(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_port_comment_notification(Context1, Id),
            Doc = cb_context:doc(Context1),
            cb_context:set_resp_data(Context1, knm_port_request:public_fields(Doc));
        _Status ->
            Context1
    end.

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

-spec load_port_request(cb_context:context(), ne_binary()) -> cb_context:context().
load_port_request(Context, Id) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(<<"port_request">>)).

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
    Msg = <<"port request is not modifiable in this state">>,
    cb_context:add_system_error('invalid_method', Msg, Context).

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
            UseDoc = filter_private_comments(Context1, PubDoc),
            cb_context:set_resp_data(cb_context:set_doc(Context1, UseDoc)
                                    ,UseDoc
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
                     (Type, C) -> load_summary_by_range_fold(C, Type, From, To)
                  end
                 ,cb_context:setters(Context
                                    ,[{fun cb_context:set_resp_data/2, []}
                                     ,{fun cb_context:set_resp_status/2, 'success'}
                                     ]
                                    )
                 ,?PORT_STATES
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
    ViewOptions = [{'keys', build_keys(Context, Number)}
                  ,'include_docs'
                  ],
    crossbar_doc:load_view(?ALL_PORT_REQ_NUMBERS
                          ,ViewOptions
                          ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                          ,fun normalize_view_results/2
                          ).

-spec build_keys(cb_context:context(), ne_binary()) -> [ne_binaries()].
build_keys(Context, Number) ->
    E164 = knm_converters:normalize(Number),
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        [AccountId] ->
            [[AccountId, E164]];
        [AccountId, ?PORT_DESCENDANTS] ->
            lists:reverse(
              [[AnAccountId, E164]
               || AnAccountId <- kapps_util:account_descendants(AccountId)
              ])
    end.

-spec load_summary(cb_context:context(), crossbar_doc:view_options() | [{'normalize', boolean()}]) ->
                          cb_context:context().
load_summary(Context, ViewOptions) ->
    View = case should_summarize_descendant_requests(Context) of
               'true' -> ?DESCENDANT_LISTING_BY_STATE;
               'false' -> ?LISTING_BY_STATE
           end,
    Context1 =
        crossbar_doc:load_view(View
                              ,['include_docs'
                               ,'descending'
                                | props:delete('normalize', ViewOptions)
                               ]
                              ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                              ,fun normalize_view_results/2
                              ),
    ShouldNormalize = props:get_value('normalize', ViewOptions, 'true'),
    maybe_normalize_summary_results(Context1, ShouldNormalize).

-spec maybe_normalize_summary_results(cb_context:context(), boolean()) -> cb_context:context().
maybe_normalize_summary_results(Context, 'false') -> Context;
maybe_normalize_summary_results(Context, 'true') ->
    case cb_context:resp_status(Context) of
        'success' -> normalize_summary_results(Context);
        _Else -> Context
    end.

-spec private_comment_filter(kz_json:object(), list()) -> list().
private_comment_filter(Comment, Acc) ->
    case kz_json:get_value(<<"superduper_comment">>, Comment, false) of
        true  -> Acc;
        false -> [Comment|Acc]
    end.

-spec filter_private_comments(cb_context:context(), kz_json:object()) -> kz_json:object().
filter_private_comments(Context, JObj) ->
    case cb_context:is_superduper_admin(Context) of
        'false' -> run_comment_filter(JObj);
        'true'  -> JObj
    end.

-spec run_comment_filter(kz_json:object()) -> kz_json:object().
run_comment_filter(JObj) ->
    Comments = kz_json:get_value(<<"comments">>, JObj, []),
    Filtered = lists:foldl(fun private_comment_filter/2, [], Comments),

    kz_json:set_value(<<"comments">>, Filtered, JObj).

-spec normalize_summary_results(cb_context:context()) -> cb_context:context().
normalize_summary_results(Context) ->
    Dict = lists:foldl(
             fun(JObj, D) ->
                     AccountId = kz_json:get_value(<<"account_id">>, JObj),
                     NewJObj   = filter_private_comments(Context, JObj),
                     dict:append_list(AccountId, [NewJObj], D)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(Res, Acc) ->
    [leak_pvt_fields(Res
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
    cb_context:set_resp_data(Context1, knm_port_request:normalize_attachments(As)).

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
    lager:debug("port state ~s is not valid for updating a port request"
               ,[PortState]),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Updating port requests not allowed in current port state">>}
            ,{<<"cause">>, PortState}
            ]),
    cb_context:add_validation_error(PortState, <<"type">>, Msg, Context).

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
    cb_context:is_superduper_admin(cb_context:auth_account_id(Context)).

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
            lager:debug("number ~s(~s) exists on multiple port request docs. That's bad!",
                        [E164, Number]),
            Msg = <<"Number is currently on multiple port requests. Contact a system admin to rectify">>,
            number_validation_error(Context, Number, Msg);
        {'error', _E} ->
            lager:debug("failed to query the port request view: ~p", [_E]),
            Message = <<"Failed to query back-end services, cannot port at this time">>,
            number_validation_error(Context, Number, Message)
    end.

check_number_portability(PortId, Number, Context, E164, PortReq) ->
    case {kz_json:get_value(<<"value">>, PortReq) =:= cb_context:account_id(Context)
         ,kz_doc:id(PortReq) =:= PortId
         }
    of
        {'true', 'true'} ->
            lager:debug("number ~s(~s) is on this existing port request for this account(~s)"
                       ,[E164, Number, cb_context:account_id(Context)]),
            cb_context:set_resp_status(Context, 'success');
        {'true', 'false'} ->
            lager:debug("number ~s(~s) is on a different port request in this account(~s): ~s"
                       ,[E164, Number, cb_context:account_id(Context), kz_doc:id(PortReq)]),
            Message = <<"Number is on a port request already: ", (kz_doc:id(PortReq))/binary>>,
            number_validation_error(Context, Number, Message);
        {'false', _} ->
            lager:debug("number ~s(~s) is on existing port request for other account(~s)"
                       ,[E164, Number, kz_json:get_value(<<"value">>, PortReq)]),
            Message = <<"Number is being ported for a different account">>,
            number_validation_error(Context, Number, Message)
    end.

-spec number_validation_error(cb_context:context(), ne_binary(), ne_binary()) ->
                                     cb_context:context().
number_validation_error(Context, Number, Message) ->
    Msg = kz_json:from_list([{<<"message">>, Message}
                            ,{<<"cause">>, Number}
                            ]),
    cb_context:add_validation_error(Number, <<"type">>, Msg, Context).

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
        {'error', {'not_in_service', _AccountId}} ->
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
            number_validation_error(Context, Number, kz_term:to_binary(E))
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
    Headers =
        [{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
        ,{<<"Content-Type">>, kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)}
        ,{<<"Content-Length">>, kz_doc:attachment_length(cb_context:doc(Context), AttachmentId)}
        ],
    cb_context:add_resp_headers(
      crossbar_doc:load_attachment(cb_context:doc(Context)
                                  ,AttachmentId
                                  ,?TYPE_CHECK_OPTION(<<"port_request">>)
                                  ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                  )
                               ,Headers
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_move_state(cb_context:context(), ne_binary(), ne_binary()) ->
                              cb_context:context().
maybe_move_state(Context, Id, PortState) ->
    Context1 = load_port_request(Context, Id),
    try cb_context:resp_status(Context1) =:= 'success'
             andalso knm_port_request:maybe_transition(cb_context:doc(Context1), PortState)
    of
        'false' -> Context1;
        {'ok', PortRequest} ->
            lager:debug("loaded new port request state ~s", [PortState]),
            cb_context:set_doc(Context1, PortRequest);
        {'error', 'invalid_state_transition'} ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Cannot move to new state from current state">>}
                    ,{<<"cause">>, PortState}
                    ]),
            cb_context:add_validation_error(<<"port_state">>, <<"enum">>, Msg, Context);
        {'error', Error} ->
            JObj = kz_json:from_list([{<<"message">>, Error}]),
            cb_context:add_system_error('transition_errors', JObj, Context)
    catch
        'throw':{'error', 'failed_to_charge'} ->
            cb_context:add_system_error('no_credit', Context)
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
-spec find_template(ne_binary(), api_binary()) -> ne_binary().
find_template(ResellerId, 'undefined') ->
    {'ok', Template} = kz_pdf:find_template(ResellerId, <<"loa">>),
    Template;
find_template(ResellerId, CarrierName) ->
    EncodedCarrierName = kz_term:to_lower_binary(kz_util:uri_encode(CarrierName)),
    TemplateName = <<EncodedCarrierName/binary, ".tmpl">>,
    lager:debug("looking for carrier template ~s or plain template for reseller ~s"
               ,[TemplateName, ResellerId]),
    case kz_pdf:find_template(ResellerId, <<"loa">>, TemplateName) of
        {'error', _} -> find_template(ResellerId, 'undefined');
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
send_port_notification(Context, Id, ?PORT_UNCONFIRMED=State) ->
    send_port_notification(Context, Id, State, fun send_port_unconfirmed_notification/2);
send_port_notification(Context, Id, ?PORT_SUBMITTED=State) ->
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
    send_port_notification(Context, Id, State, fun send_port_cancel_notification/2).

-spec send_port_notification(cb_context:context(), path_token(), path_token(), function()) ->
                                    cb_context:context().
send_port_notification(Context, Id, State, Fun) ->
    try
        Fun(Context, Id),
        lager:debug("port ~s notification sent", [State]),
        Context
    catch
        _E:_R ->
            lager:debug("failed to send the  port ~s notification: ~s:~p", [State, _E, _R]),
            _ = revert_patch(Context),
            Msg = <<"failed to send port ", State/binary, " email">>,
            cb_context:add_system_error('bad_gateway'
                                       ,kz_json:from_list([{<<"message">>, Msg}])
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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_comment/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_port_unconfirmed_notification(cb_context:context(), ne_binary()) -> 'ok'.
send_port_unconfirmed_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
          ,{<<"Version">>, cb_context:api_version(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_unconfirmed/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_request/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_pending/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_rejected/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_cancel/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_ported/1).

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
    kapi_notify_publisher:cast(Req, fun kapi_notifications:publish_port_scheduled/1).

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
    TemplateData =
        props:filter_undefined(
          [{<<"reseller">>, kz_json:to_proplist(ResellerDoc)}
          ,{<<"account">>, kz_json:to_proplist(cb_context:account_doc(Context))}
          ,{<<"numbers">>, [knm_util:pretty_print(N) || N <- kz_json:get_keys(<<"numbers">>, PortRequest)]}
          ,{<<"bill">>, kz_json:to_proplist(kz_json:get_value(<<"bill">>, PortRequest, kz_json:new()))}
          ,{<<"request">>, kz_json:to_proplist(PortRequest)}
          ,{<<"qr_code">>, create_QR_code(AccountId, kz_doc:id(PortRequest))}
          ,{<<"type">>, <<"loa">>}
          ]),
    Carrier = kz_json:get_value(<<"carrier">>, PortRequest),

    Template = find_template(ResellerId, Carrier),
    case kz_pdf:generate(ResellerId, TemplateData, Template) of
        {'error', _R} -> cb_context:set_resp_status(Context, 'error');
        {'ok', PDF} ->
            cb_context:set_resp_status(cb_context:set_resp_data(Context, PDF), 'success')
    end.

-spec create_QR_code(api_binary(), api_binary()) -> kz_proplist() | 'undefined'.
create_QR_code('undefined', _) -> 'undefined';
create_QR_code(_, 'undefined') -> 'undefined';
create_QR_code(AccountId, PortRequestId) ->
    lager:debug("create qr code for ~s - ~s", [AccountId, PortRequestId]),
    CHL = [binary_to_list(AccountId), "-", binary_to_list(PortRequestId)],
    Url = ["https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=", CHL, "&choe=UTF-8"],

    case kz_http:get(lists:flatten(Url)) of
        {'ok', 200, _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s", [Url]),
            lager:debug("QR code size: ~p, head: ~w"
                       ,[byte_size(RespBody), binary:part(RespBody, 0, min(50, byte_size(RespBody)))]),
            [{<<"image">>, base64:encode(RespBody)}];
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            'undefined'
    end.
