%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc Handles port request life cycles
%%% See doc/port_requests.md
%%%
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_port_requests).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,content_types_provided/3, content_types_provided/4
        ,content_types_accepted/3, content_types_accepted/4
        ,validate/1, validate/2, validate/3, validate/4
        ,get/3
        ,put/1, put/3
        ,patch/3, patch/2
        ,post/2, post/4
        ,delete/2, delete/4
        ,authority/1
        ,acceptable_content_types/0
        ,validate_request/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(SCHEMA, <<"port_requests">>).
-define(SCHEMA_TO_SCHEDULED, <<(?SCHEMA)/binary, ".to_scheduled">>).

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
-define(PATH_TOKEN_TIMELINE, <<"timeline">>).
-define(PATH_TOKEN_LAST_SUBMITTED, <<"last_submitted">>).

-define(REQ_TRANSITION, <<"reason">>).

-define(SUBMIT_TO_CARRIER, <<"submit">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize.port_requests">>, ?MODULE, 'authorize'),
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

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
                       boolean() |
                       {'stop', cb_context:context()}.
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), req_nouns(), req_verb()) -> 'true'.
authorize(Context, [{<<"port_requests">>, []}], ?HTTP_GET) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' ->
            {'stop', cb_context:add_system_error('forbidden', Context)}
    end;
authorize(Context, [{<<"port_requests">>, []}], _) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize(_Context, _, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PATH_TOKEN_LAST_SUBMITTED) ->
    [?HTTP_GET];
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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
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
    [?HTTP_GET];
allowed_methods(_PortRequestId, ?PATH_TOKEN_TIMELINE) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_PortRequestId, ?PORT_ATTACHMENT, _AttachmentId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /port_requests => []
%%    /port_requests/foo => [<<"foo">>]
%%    /port_requests/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_PortRequestId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_PortRequestId, ?PORT_SUBMITTED) -> 'true';
resource_exists(_PortRequestId, ?PORT_PENDING) -> 'true';
resource_exists(_PortRequestId, ?PORT_SCHEDULED) -> 'true';
resource_exists(_PortRequestId, ?PORT_COMPLETED) -> 'true';
resource_exists(_PortRequestId, ?PORT_REJECTED) -> 'true';
resource_exists(_PortRequestId, ?PORT_CANCELED) -> 'true';
resource_exists(_PortRequestId, ?PORT_ATTACHMENT) -> 'true';
resource_exists(_PortRequestId, ?PATH_TOKEN_LOA) -> 'true';
resource_exists(_PortRequestId, ?PATH_TOKEN_TIMELINE) -> 'true';
resource_exists(_PortRequestId, _) -> 'false'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_PortRequestId, ?PORT_ATTACHMENT, _AttachmentId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's accept header).
%% Of the form `{atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec acceptable_content_types() -> kz_term:proplist().
acceptable_content_types() -> ?ATTACHMENT_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _Id, ?PATH_TOKEN_LOA) ->
    cb_context:add_content_types_provided(Context, [{'to_binary', ?PDF_CONTENT_TYPES}]);
content_types_provided(Context, _Id, _Path) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET -> content_types_provided_get(Context, _Id, _AttachmentId);
        _Verb -> Context
    end.

-spec content_types_provided_get(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
content_types_provided_get(Context, Id, AttachmentId) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    cb_context:add_attachment_content_type(Context1, Id, AttachmentId).

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be requiring (matched to the client's
%% Content-Type header.
%% Of the form `{atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _Id, ?PORT_ATTACHMENT) ->
    CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
    cb_context:add_content_types_accepted(Context, CTA);
content_types_accepted(Context, _Id, _) ->
    Context.

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _Id, ?PORT_ATTACHMENT, _AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_POST ->
            CTA = [{'from_binary', ?ATTACHMENT_MIME_TYPES}],
            cb_context:add_content_types_accepted(Context, CTA);
        _Verb ->
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /port_requests might load a list of port_request objects
%% /port_requests/123 might load the port_request object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) ->
                      cb_context:context().
validate(Context) ->
    validate_port_requests(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
validate(Context, ?PATH_TOKEN_LAST_SUBMITTED) ->
    last_submitted(Context);
validate(Context, ?PORT_UNCONFIRMED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_SUBMITTED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_PENDING = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_SCHEDULED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_COMPLETED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_REJECTED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, ?PORT_CANCELED = Type) ->
    load_summary_by_type(Context, Type);
validate(Context, Id) ->
    validate_port_request(Context, Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
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
    generate_loa(read(Context, Id));
validate(Context, Id, ?PATH_TOKEN_TIMELINE) ->
    maybe_prepare_timeline(load_port_request(Context, Id)).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) ->
                      cb_context:context().
validate(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    validate_attachment(Context, Id, AttachmentId, cb_context:req_verb(Context)).

-spec validate_patch(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(PortId, Context) ->
    ValidateFun = fun (_PortId, C) -> cb_context:validate_request_data(?SCHEMA, C) end,
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:patch_and_validate(PortId, Context1, ValidateFun).

-spec superduper(cb_context:context()) -> cb_context:context().
superduper(Context) ->
    cb_context:store(Context, 'is_superduper_admin', cb_context:is_superduper_admin(Context)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, Id, ?PATH_TOKEN_LOA) ->
    lager:debug("load LOA for ~s", [Id]),
    cb_context:set_resp_data(Context, kz_json:encode(cb_context:doc(Context))).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------

-spec put(cb_context:context()) -> cb_context:context().
put(Context) -> save(Context).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, Id, ?PORT_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)],
    AName = cb_modules_util:attachment_name(Filename, CT),
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:save_attachment(Id, AName, Contents, Context1, Opts).

-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
    NewDoc1 = maybe_set_scheduled_date_from_schedule_on(cb_context:doc(Context)),
    NewDoc = kzd_accounts:set_tree(NewDoc1, kzd_accounts:tree(cb_context:account_doc(Context))),
    Context1 = cb_context:setters(Context
                                 ,[{fun cb_context:set_account_db/2, ?KZ_PORT_REQUESTS_DB}
                                  ,{fun cb_context:set_doc/2, NewDoc}
                                  ]
                                 ),
    crossbar_doc:save(Context1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context, Id, NewState=?PORT_SUBMITTED) ->
    case phonebook:maybe_create_port_in(Context) of
        'disabled' ->
            save_then_maybe_notify(Context, Id, NewState);
        'ok' ->
            Context1 = crossbar_doc:load(Id, Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    Context2 = maybe_move_state(Context1, NewState),
                    save_then_maybe_notify(Context2, Id, NewState);
                Error ->
                    cb_context:add_system_error('datastore_fault', Error, Context1)
            end;
        {'error', Reason} ->
            cb_context:add_system_error('datastore_fault', Reason, Context)
    end;
patch(Context, Id, NewState=?PORT_PENDING) ->
    save_then_maybe_notify(Context, Id, NewState);
patch(Context, Id, NewState=?PORT_SCHEDULED) ->
    OnSuccess = fun (C) -> save_then_maybe_notify(C, Id, NewState) end,
    cb_context:validate_request_data(?SCHEMA_TO_SCHEDULED, Context, OnSuccess);
patch(Context, Id, NewState=?PORT_COMPLETED) ->
    save_then_maybe_notify(Context, Id, NewState);
patch(Context, Id, NewState=?PORT_REJECTED) ->
    save_then_maybe_notify(Context, Id, NewState);
patch(Context, Id, NewState=?PORT_CANCELED) ->
    save_then_maybe_notify(Context, Id, NewState).

-spec save_then_maybe_notify(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
save_then_maybe_notify(Context, PortId, NewState) ->
    Context1 = save(superduper(Context)),
    case 'success' =:= cb_context:resp_status(Context1) of
        'false' -> Context1;
        'true' ->
            RespData1 = knm_port_request:public_fields(cb_context:doc(Context1)),
            RespData = filter_private_comments(Context1, RespData1),
            Context2 = cb_context:set_resp_data(Context1, RespData),
            send_port_notification(Context2, PortId, NewState)
    end.

-spec maybe_set_scheduled_date_from_schedule_on(kz_json:object()) -> kz_json:object().
maybe_set_scheduled_date_from_schedule_on(Doc) ->
    case kz_json:get_ne_value(<<"schedule_on">>, Doc) of
        undefined -> Doc;
        DateJObj ->
            TZ = kz_json:get_ne_binary_value(<<"timezone">>, DateJObj),
            Datetime = kz_json:get_ne_binary_value(<<"date_time">>, DateJObj),
            Scheduled = date_as_configured_timezone(Datetime, TZ),
            lager:debug("date ~s (~s) translated to ~p", [Datetime, TZ, Scheduled]),
            kz_json:set_value(<<"scheduled_date">>, Scheduled, Doc)
    end.

-spec date_as_configured_timezone(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_time:gregorian_seconds().
date_as_configured_timezone(<<YYYY:4/binary, $-, MM:2/binary, $-, DD:2/binary, $\s,
                              HH:2/binary, $:, Mm:2/binary>>
                           ,FromTimezone
                           ) ->
    Date = {kz_term:to_integer(YYYY), kz_term:to_integer(MM), kz_term:to_integer(DD)},
    Time = {kz_term:to_integer(HH), kz_term:to_integer(Mm), 0},

    date_as_configured_timezone(Date, Time, FromTimezone).

date_as_configured_timezone(Date, Time, 'undefined') ->
    date_as_configured_timezone(Date, Time, kzd_accounts:default_timezone());
date_as_configured_timezone(Date, Time, FromTimezone) ->
    kz_time:to_gregorian_seconds({Date, Time}, FromTimezone).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    Context1 = save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_port_comment_notification(Context1, Id),
            Doc = cb_context:doc(Context1),
            cb_context:set_resp_data(Context1, knm_port_request:public_fields(Doc));
        _Status ->
            Context1
    end.

-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Options = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)],
    _ = case kz_doc:attachment(cb_context:doc(Context), AttachmentId) of
            'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
            _AttachmentMeta ->
                lager:debug("deleting old attachment ~s", [AttachmentId]),
                kz_datamgr:delete_attachment(cb_context:account_db(Context), Id, AttachmentId)
        end,
    crossbar_doc:save_attachment(Id, AttachmentId, Contents, Context, Options).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------

-spec delete(cb_context:context(), path_token()) ->
                    cb_context:context().
delete(Context, _Id) ->
    Context2 = crossbar_doc:delete(Context),
    phonebook:maybe_cancel_port_in(Context2).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) ->
                    cb_context:context().
delete(Context, Id, ?PORT_ATTACHMENT, AttachmentName) ->
    crossbar_doc:delete_attachment(Id, AttachmentName, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_port_request(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_port_request(Context, Id) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_port_requests(cb_context:context(), req_nouns(), http_method()) ->
                                    cb_context:context().
validate_port_requests(Context, _, ?HTTP_GET) ->
    summary(Context);
validate_port_requests(Context, _, ?HTTP_PUT) ->
    create(Context).

-spec validate_port_request(cb_context:context(), kz_term:ne_binary(), http_method()) ->
                                   cb_context:context().
validate_port_request(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_port_request(Context, Id, ?HTTP_POST) ->
    update(Context, Id);
validate_port_request(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_port_request(Context, Id, ?HTTP_DELETE) ->
    is_deletable(load_port_request(Context, Id)).

-spec validate_port_request(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), http_method()) ->
                                   cb_context:context().
validate_port_request(Context, Id, ToState=?PORT_SUBMITTED, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate_port_request(Context, Id, ToState=?PORT_PENDING, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate_port_request(Context, Id, ToState=?PORT_SCHEDULED, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate_port_request(Context, Id, ToState=?PORT_COMPLETED, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate_port_request(Context, Id, ToState=?PORT_REJECTED, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate_port_request(Context, Id, ToState=?PORT_CANCELED, ?HTTP_PATCH) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState).

-spec patch_then_validate_then_maybe_transition(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                                       cb_context:context().
patch_then_validate_then_maybe_transition(Context, PortId, ToState) ->
    ValidateFun = fun (_PortId, C) ->
                          OnSuccess = fun (OtherC) -> maybe_move_state(OtherC, ToState) end,
                          cb_context:validate_request_data(?SCHEMA, C, OnSuccess)
                  end,
    Context1 = cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB),
    LoadOptions = ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST),
    crossbar_doc:patch_and_validate(PortId, Context1, ValidateFun, LoadOptions).

-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(_PortId, Context) -> Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_attachments(cb_context:context(), kz_term:ne_binary(), http_method()) ->
                                  cb_context:context().
validate_attachments(Context, Id, ?HTTP_GET) ->
    summary_attachments(Context, Id);
validate_attachments(Context, Id, ?HTTP_PUT) ->
    read(Context, Id).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_attachment(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), http_method()) ->
                                 cb_context:context().
validate_attachment(Context, Id, AttachmentId, ?HTTP_GET) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_POST) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_DELETE) ->
    is_deletable(load_attachment(Id, AttachmentId, Context)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec is_deletable(cb_context:context()) -> cb_context:context().
is_deletable(Context) ->
    is_deletable(Context, knm_port_request:current_state(cb_context:doc(Context))).

-spec is_deletable(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
is_deletable(Context, ?PORT_UNCONFIRMED) -> Context;
is_deletable(Context, ?PORT_REJECTED) -> Context;
is_deletable(Context, ?PORT_CANCELED) -> Context;
is_deletable(Context, _PortState) ->
    lager:debug("port is in state ~s, can't modify", [_PortState]),
    Msg = <<"port request is not modifiable in this state">>,
    cb_context:add_system_error('invalid_method', Msg, Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C, 'undefined') end,
    cb_context:validate_request_data(?SCHEMA, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 = load_port_request(superduper(Context), Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            PubDoc = knm_port_request:public_fields(cb_context:doc(Context1)),
            UseDoc = filter_private_comments(Context1, PubDoc),
            cb_context:set_resp_data(cb_context:set_doc(Context1, UseDoc)
                                    ,UseDoc
                                    );
        _ -> Context1
    end.

-spec authority(kz_term:ne_binary()) -> kz_term:api_binary().
authority(AccountId) ->
    case kzd_whitelabel:fetch(AccountId) of
        {'error', _R} -> 'undefined';
        {'ok', JObj} ->
            kzd_whitelabel:port_authority(JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
update(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(C, Id) end,
    cb_context:validate_request_data(?SCHEMA, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:req_value(Context, <<"by_number">>) of
        'undefined' -> summarize_by_types(Context);
        Number -> load_summary_by_number(Context, Number)
    end.

-spec summarize_by_types(cb_context:context()) -> cb_context:context().
summarize_by_types(Context) ->
    case cb_context:req_value(Context, <<"by_types">>) of
        'undefined' -> load_summary_by_types(Context, ?PORT_ACTIVE_STATES);
        <<"all">> -> load_summary_by_types(Context, ?PORT_STATES);
        <<"progressing">> -> load_summary_by_types(Context, ?PORT_PROGRESSING_STATES);
        <<"suspended">> -> load_summary_by_types(Context, ?PORT_SUSPENDED_STATES);
        <<"completed">> -> load_summary_by_types(Context, ?PORT_COMPLETED_STATES);
        Types -> summarize_by_types(Context, Types)
    end.

-spec summarize_by_types(cb_context:context(), any()) -> cb_context:context().
summarize_by_types(Context, Types) when is_list(Types) ->
    case lists:partition(fun(Type) -> lists:member(Type, ?PORT_STATES) end
                        ,[kz_binary:strip(Type) || Type <- Types]
                        )
    of
        {Satisfying, []} ->
            load_summary_by_types(Context, Satisfying);
        {_, Unsatisfying} ->
            Message = <<"query string by_types contained invalid values: "
                       ,(kz_binary:join(Unsatisfying))/binary
                      >>,
            cb_context:add_system_error('invalid_request', Message, Context)
    end;
summarize_by_types(Context, Types) ->
    summarize_by_types(Context, binary:split(kz_term:to_binary(Types), <<",">>)).

%%%=============================================================================
%%% Load Summary By Range
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_summary_by_type(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_summary_by_type(Context, Type) ->
    lager:debug("loading summary for ~s", [Type]),
    IsRanged = should_range_summary(Type),
    maybe_normalize_summary_results(
      load_summary(superduper(Context), view_key_options(Context, Type, IsRanged))
     ).

-spec load_summary_by_types(cb_context:context(), kz_term:ne_binaries()) -> cb_context:context().
load_summary_by_types(Context, Types) ->
    lager:debug("loading summary for port requests with types ~s"
               ,[kz_binary:join(Types)]
               ),
    Funs = fun(Type, C) ->
                   IsRanged = should_range_summary(Type),
                   load_summary_fold(C, Type, IsRanged)
           end,
    Context1 = cb_context:setters(Context
                                 ,[{fun cb_context:set_resp_data/2, []}
                                  ,{fun cb_context:set_resp_status/2, 'success'}
                                  ]
                                 ),
    maybe_normalize_summary_results(lists:foldl(Funs, Context1, Types)).

-spec should_range_summary(kz_term:ne_binary()) -> boolean().
should_range_summary(Type) ->
    lists:member(Type, ?PORT_COMPLETED_STATES).

-spec load_summary_fold(cb_context:context(), kz_term:ne_binary(), boolean()) -> cb_context:context().
load_summary_fold(Context, Type, IsRanged) ->
    Summary = cb_context:resp_data(Context),
    case cb_context:resp_data(load_summary(Context, view_key_options(Context, Type, IsRanged))) of
        TypeSummary when is_list(TypeSummary) ->
            cb_context:set_resp_data(Context, Summary ++ TypeSummary);
        _Else -> Context
    end.

-spec load_summary(cb_context:context(), {boolean(), crossbar_view:options()}) -> cb_context:context().
load_summary(Context, {IsRanged, Opts}) ->
    View = get_summarize_view_name(Context),
    Options = [{'mapper', fun normalize_view_results/2}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,{'unchunkable', 'true'}
              ,{'should_paginate', 'false'}
              ,'include_docs'
               | Opts
              ],
    case IsRanged of
        'true' -> crossbar_view:load_range(Context, View, Options);
        'false' -> crossbar_view:load(Context, View, Options)
    end.

-spec view_key_options(cb_context:context(), kz_term:ne_binary(), boolean()) -> {boolean(), crossbar_view:options()}.
view_key_options(Context, Type, 'true') ->
    {'true'
    ,[{'range_start_keymap', [cb_context:account_id(Context), Type]}
     ,{'range_end_keymap', [cb_context:account_id(Context), Type]}
     ,{'range_key_name', <<"modified">>}
     ]
    };
view_key_options(Context, Type, 'false') ->
    {'false'
    ,[{'startkey', [cb_context:account_id(Context), Type]}
     ,{'endkey', [cb_context:account_id(Context), Type, kz_json:new()]}
     ]
    }.


-spec get_summarize_view_name(cb_context:context()) -> kz_term:ne_binary().
get_summarize_view_name(Context) ->
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        [_AccountId, ?DESCENDANTS] -> ?DESCENDANT_LISTING_BY_STATE;
        _Params -> ?LISTING_BY_STATE
    end.

-spec maybe_normalize_summary_results(cb_context:context()) -> cb_context:context().
maybe_normalize_summary_results(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> normalize_summary_results(Context);
        _Else -> Context
    end.

-spec normalize_summary_results(cb_context:context()) -> cb_context:context().
normalize_summary_results(Context) ->
    Dict = lists:foldl(fun(JObj, D) ->
                               AccountId = kz_json:get_value(<<"account_id">>, JObj),
                               NewJObj   = filter_private_comments(Context, JObj),
                               dict:append_list(AccountId, [NewJObj], D)
                       end
                      ,dict:new()
                      ,cb_context:resp_data(Context)
                      ),
    Names = get_account_names(dict:fetch_keys(Dict)),
    JObj = [kz_json:from_list(
              [{<<"account_id">>, AccountId}
              ,{<<"account_name">>, props:get_value(AccountId, Names, <<"unknown">>)}
              ,{<<"port_requests">>, JObjs}
              ])
            || {AccountId, JObjs} <- dict:to_list(Dict)
           ],
    cb_context:set_resp_data(Context, JObj).

-spec get_account_names(kz_term:ne_binaries()) -> kz_term:proplist().
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

%%%=============================================================================
%%% Load Summary By Number
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_summary_by_number(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_summary_by_number(Context, Number) ->
    E164 = knm_converters:normalize(Number),
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        [AccountId] ->
            load_account_summary_by_number(Context, [[AccountId, E164]]);
        [AccountId, ?PORT_DESCENDANTS] ->
            load_account_summary_by_number(Context, lists:reverse(
                                                      [[AnAccountId, E164]
                                                       || AnAccountId <- kapps_util:account_descendants(AccountId)
                                                      ]));
        _ -> load_global_summary_by_number(Context, E164)
    end.

-spec load_global_summary_by_number(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_global_summary_by_number(Context, Number) ->
    load_summary(Context, {'keymap', Number}, ?PORT_REQ_NUMBERS).

-spec load_account_summary_by_number(cb_context:context(), [kz_term:ne_binaries()]) ->
                                            cb_context:context().
load_account_summary_by_number(Context, Keys) ->
    load_summary(Context, {'keys', Keys}, ?ALL_PORT_REQ_NUMBERS).

-spec load_summary(cb_context:context()
                  ,{'keymap', kz_term:ne_binary()} |
                   {'keys', [kz_term:ne_binaries()]}
                  ,kz_term:ne_binary()
                  ) -> cb_context:context().
load_summary(Context, Key, View) ->
    Options = [Key
              ,{'mapper', fun normalize_view_results/2}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,'include_docs'
              ],
    crossbar_view:load(Context, View, Options).

%%%=============================================================================
%%% Load Last Submitted
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec last_submitted(cb_context:context()) -> cb_context:context().
last_submitted(Context) ->
    AccountId = cb_context:account_id(Context),
    Options = [{'startkey', [AccountId]}
              ,{'endkey', [AccountId, kz_json:new()]}
              ,{'mapper', fun  normalize_last_submitted/3}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,'include_docs'
              ],
    crossbar_view:load(superduper(Context), <<"port_requests/listing_submitted">>, Options).

-spec normalize_last_submitted(cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_last_submitted(Context, ResultJObj, Acc) ->
    Doc = kz_json:get_value(<<"doc">>, ResultJObj),
    Timeline = prepare_timeline(Context, Doc),
    case lists:reverse(transitions_to_submitted(Timeline)) of
        [] -> Acc;
        [LastSubmitted|_] ->
            JObj = kz_json:from_list(
                     [{<<"id">>, kz_doc:id(Doc)}
                     ,{<<"transition">>, LastSubmitted}
                     ]),
            [JObj|Acc]
    end.

-spec transitions_to_submitted(kz_json:objects()) -> kz_json:objects().
transitions_to_submitted(Timeline) ->
    [JObj || JObj <- Timeline,
             kz_json:get_ne_binary_value([?PORT_TRANSITION, <<"new">>], JObj) =:= ?PORT_SUBMITTED
    ].

-spec prepare_timeline(cb_context:context(), kz_json:object()) -> kz_json:objects().
prepare_timeline(Context, Doc) ->
    Comments = kz_json:get_list_value(<<"comments">>, filter_private_comments(Context, Doc), []),
    Transitions = kz_json:get_list_value(?PORT_PVT_TRANSITIONS, Doc, []),
    Indexed = [{kz_json:get_integer_value(?TRANSITION_TIMESTAMP, JObj), JObj}
               || JObj <- Comments ++ Transitions
              ],
    {_, NewDocs} = lists:unzip(lists:keysort(1, Indexed)),
    NewDocs.

%% calls by `validate(Context, Id, ?PATH_TOKEN_TIMELINE)'
-spec maybe_prepare_timeline(cb_context:context()) -> cb_context:context().
maybe_prepare_timeline(Context) ->
    case success =:= cb_context:resp_status(Context) of
        false -> Context;
        true ->
            NewDoc = prepare_timeline(Context, cb_context:doc(Context)),
            cb_context:set_resp_data(Context, NewDoc)
    end.

-spec filter_private_comments(cb_context:context(), kz_json:object()) -> kz_json:object().
filter_private_comments(Context, JObj) ->
    case cb_context:fetch(Context, 'is_superduper_admin', 'false') of
        'false' -> run_comment_filter(JObj);
        'true'  -> JObj
    end.

-spec run_comment_filter(kz_json:object()) -> kz_json:object().
run_comment_filter(JObj) ->
    Filtered = [Comment
                || Comment <- kz_json:get_list_value(<<"comments">>, JObj, []),
                   not kz_json:is_true(<<"superduper_comment">>, Comment)
               ],
    kz_json:set_value(<<"comments">>, Filtered, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(Res, Acc) ->
    [leak_pvt_fields(Res, knm_port_request:public_fields(kz_json:get_value(<<"doc">>, Res)))
     | Acc
    ].

-spec leak_pvt_fields(kz_json:object(), kz_json:object()) -> kz_json:object().
leak_pvt_fields(Res, JObj) ->
    Fields = [{[<<"doc">>, <<"pvt_account_id">>], <<"account_id">>}],
    lists:foldl(fun({Field, Key}, J) ->
                        case kz_json:get_ne_value(Field, Res) of
                            'undefined' -> J;
                            Value -> kz_json:set_value(Key, Value, J)
                        end
                end
               ,JObj
               ,Fields
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summary_attachments(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
summary_attachments(Context, Id) ->
    Context1 = load_port_request(Context, Id),
    As = kz_doc:attachments(cb_context:doc(Context1), kz_json:new()),
    cb_context:set_resp_data(Context1, knm_port_request:normalize_attachments(As)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
on_successful_validation(Context, 'undefined') ->
    on_successful_validation(Context, 'undefined', 'true');
on_successful_validation(Context, Id) ->
    Context1 = crossbar_doc:load_merge(Id
                                      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                      ,?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)
                                      ),
    on_successful_validation(Context1, Id, can_update_port_request(Context1)).

-spec on_successful_validation(cb_context:context(), kz_term:api_binary(), boolean()) -> cb_context:context().
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
    lager:debug("port state ~s is not valid for updating a port request", [PortState]),
    Msg = kz_json:from_list(
            [{<<"message">>, <<"Updating port requests not allowed in current port state">>}
            ,{<<"cause">>, PortState}
            ]),
    cb_context:add_validation_error(PortState, <<"type">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec can_update_port_request(cb_context:context()) -> boolean().
can_update_port_request(Context) ->
    lager:debug("port request: ~p", [cb_context:doc(Context)]),
    can_update_port_request(Context, knm_port_request:current_state(cb_context:doc(Context))).

-spec can_update_port_request(cb_context:context(), kz_term:ne_binary()) -> boolean().
can_update_port_request(_Context, ?PORT_UNCONFIRMED) ->
    'true';
can_update_port_request(_Context, ?PORT_REJECTED) ->
    'true';
can_update_port_request(Context, _) ->
    cb_context:is_superduper_admin(cb_context:auth_account_id(Context)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec successful_validation(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
successful_validation(Context, 'undefined') ->
    PortReq = knm_port_request:new(cb_context:doc(Context)
                                  ,cb_context:auth_account_id(Context)
                                  ,cb_context:auth_user_id(Context)
                                  ),
    cb_context:set_doc(Context, PortReq);
successful_validation(Context, _Id) ->
    Normalized = knm_port_request:normalize_numbers(cb_context:doc(Context)),
    cb_context:set_doc(Context, Normalized).


-spec fetch_by_number(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
fetch_by_number(Context, Number) ->
    Options = [{'keymap', Number}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,{'unchunkable', 'true'}
              ,{'should_paginate', 'false'}
              ],
    crossbar_view:load(Context, ?PORT_REQ_NUMBERS, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_number_portability(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) ->
                                      cb_context:context().
check_number_portability(PortId, Number, Context) ->
    E164 = knm_converters:normalize(Number),
    lager:debug("checking ~s(~s) for portability", [E164, Number]),
    Context1 = fetch_by_number(Context, E164),
    case cb_context:resp_status(Context1) of
        'success' ->
            DataResp = cb_context:resp_data(Context1),
            check_number_portability(PortId, Number, Context1, E164, DataResp);
        _ -> Context1
    end.

-spec check_number_portability(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), kz_json:objects()) ->
                                      cb_context:context().
check_number_portability(_PortId, Number, Context, E164, []) ->
    check_number_existence(E164, Number, Context);
check_number_portability(PortId, Number, Context, E164, [PortReq]) ->
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
    end;
check_number_portability(_PortId, Number, Context, E164, [_|_]) ->
    lager:debug("number ~s(~s) exists on multiple port request docs. That's bad!",
                [E164, Number]),
    Msg = <<"Number is currently on multiple port requests. Contact a system admin to rectify">>,
    number_validation_error(Context, Number, Msg).

-spec number_validation_error(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     cb_context:context().
number_validation_error(Context, Number, Message) ->
    Msg = kz_json:from_list([{<<"message">>, Message}
                            ,{<<"cause">>, Number}
                            ]),
    cb_context:add_validation_error(Number, <<"type">>, Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_number_existence(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_attachment(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(Id, AttachmentId, Context) ->
    Context1 = read(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' -> load_attachment(AttachmentId, Context1);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_attachment(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_attachment(AttachmentId, Context) ->
    Context1 = crossbar_doc:load_attachment(cb_context:doc(Context)
                                           ,AttachmentId
                                           ,?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)
                                           ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                           ),
    Headers =
        #{<<"content-disposition">> => <<"attachment; filename=", AttachmentId/binary>>
         ,<<"content-type">> => kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)
         },
    cb_context:add_resp_headers(Context1, Headers).

-spec maybe_move_state(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_move_state(Context, PortState) ->
    Metadata = knm_port_request:transition_metadata(cb_context:auth_account_id(Context)
                                                   ,cb_context:auth_user_id(Context)
                                                   ,cb_context:req_value(Context, ?REQ_TRANSITION)
                                                   ),
    try cb_context:resp_status(Context) =:= 'success'
             andalso knm_port_request:maybe_transition(cb_context:doc(Context), Metadata, PortState)
    of
        'false' -> Context;
        {'ok', PortRequest} ->
            lager:debug("loaded new port request state ~s", [PortState]),
            cb_context:set_doc(Context, PortRequest);
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_loa(cb_context:context()) ->
                          cb_context:context().
generate_loa(Context) ->
    generate_loa(Context, cb_context:resp_status(Context)).

-spec generate_loa(cb_context:context(), crossbar_status()) ->
                          cb_context:context().
generate_loa(Context, 'success') ->
    generate_loa_from_port(Context, cb_context:doc(Context));
generate_loa(Context, _RespStatus) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_template(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_port_comment_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
maybe_send_port_comment_notification(Context, Id) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    ReqData = cb_context:req_data(Context),
    DbDocComments = kz_json:get_list_value(<<"comments">>, DbDoc, []),
    ReqDataComments = kz_json:get_list_value(<<"comments">>, ReqData, []),
    case has_new_comment(DbDocComments, ReqDataComments) of
        'false' -> lager:debug("no new comments in ~s, ignoring", [Id]);
        'true' ->
            _ = phonebook:maybe_add_comment(Context, lists:last(ReqDataComments)),
            try send_port_comment_notification(Context, Id, lists:last(ReqDataComments)) of
                _ -> lager:debug("port comment notification sent")
            catch
                _E:_R ->
                    lager:error("failed to send the port comment notification: ~s:~p", [_E, _R])
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_new_comment(kz_term:api_objects(), kz_term:api_objects()) -> boolean().
has_new_comment([], [_|_]) -> 'true';
has_new_comment(_, []) -> 'false';
has_new_comment(OldComments, NewComments) ->
    OldTime = kz_json:get_integer_value(<<"timestamp">>, lists:last(OldComments)),
    NewTime = kz_json:get_integer_value(<<"timestamp">>, lists:last(NewComments)),
    OldTime < NewTime.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
        lager:debug("port ~s notification sent for ~s", [State, Id]),
        Context
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            lager:debug("failed to send the  port ~s notification: ~s:~p", [State, _E, _R]),
            _ = revert_patch(Context),
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"failed to send port ", State/binary, " email">>}
                    ]),
            cb_context:add_system_error('bad_gateway', Msg, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec revert_patch(cb_context:context()) -> cb_context:context().
revert_patch(Context) ->
    Doc = cb_context:doc(Context),
    DBDoc = cb_context:fetch(Context, 'db_doc'),
    Rev = kz_doc:revision(Doc),
    RevertedDoc = kz_doc:set_revision(DBDoc, Rev),
    crossbar_doc:save(cb_context:set_doc(Context, RevertedDoc)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_comment_notification(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_port_comment_notification(Context, Id, NewComment) ->
    Props = [{<<"user_id">>, cb_context:auth_user_id(Context)}
            ,{<<"account_id">>, cb_context:auth_account_id(Context)}
            ],
    Comment = kz_json:set_values(Props, NewComment),
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
          ,{<<"Comment">>, Comment}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending port request notification for new comment by user ~s in account ~s"
               ,[cb_context:auth_user_id(Context), cb_context:auth_account_id(Context)]
               ),
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_comment/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_unconfirmed_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_unconfirmed_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_unconfirmed/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_request_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_request_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
          ,{<<"Version">>, cb_context:api_version(Context)}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_request/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_pending_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_pending_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_pending/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_rejected_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_rejected_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_rejected/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_cancel_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_cancel_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_cancel/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_ported_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_ported_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_ported/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_port_scheduled_notification(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_scheduled_notification(Context, Id) ->
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
           | common_patch_notification_props(Context, cb_context:req_value(Context, ?REQ_TRANSITION))
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_scheduled/1).

-spec common_patch_notification_props(cb_context:context(), kz_term:api_ne_binary()) -> kz_term:proplist().
common_patch_notification_props(Context, ?NE_BINARY=Reason) ->
    Props = [{<<"user_id">>, cb_context:auth_user_id(Context)}
            ,{<<"account_id">>, cb_context:auth_account_id(Context)}
            ,{<<"timestamp">>, kz_doc:modified(cb_context:doc(Context))}
            ,{<<"content">>, Reason}
            ],
    [{<<"Reason">>, kz_json:from_list(Props)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ];
common_patch_notification_props(_, _) ->
    kz_api:default_headers(?APP_NAME, ?APP_VERSION).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_loa_from_port(cb_context:context(), kz_json:object()) ->
                                    cb_context:context().
generate_loa_from_port(Context, PortRequest) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services_reseller:get_id(AccountId),
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
    Carrier = kz_json:get_ne_binary_value(<<"carrier">>, PortRequest),
    Template = find_template(ResellerId, Carrier),
    case kz_pdf:generate(ResellerId, TemplateData, Template) of
        {'error', _R} ->
            lager:error("generating LOA failed: ~p", [_R]),
            cb_context:set_resp_status(Context, 'error');
        {'ok', PDF} ->
            cb_context:set_resp_status(cb_context:set_resp_data(Context, PDF), 'success')
    end.

-spec create_QR_code(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:proplist() | 'undefined'.
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
