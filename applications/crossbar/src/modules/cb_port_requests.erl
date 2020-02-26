%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Handles port request life cycles
%%% See doc/port_requests.md
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
        ,acceptable_content_types/0

        ,load_port_request/2
        ,set_port_authority/1
        ,validate_port_comments/2
        ,filter_private_comments/2
        ,send_port_comment_notifications/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_numbers/include/knm_port_request.hrl").

-define(SCHEMA, <<"port_requests">>).
-define(SCHEMA_TO_SCHEDULED, <<(?SCHEMA)/binary, ".to_scheduled">>).

-define(TEMPLATE_DOC_ID, <<"notify.loa">>).
-define(TEMPLATE_ATTACHMENT_ID, <<"template">>).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"octet-stream">>, '*'}
                               ,{<<"text">>, <<"plain">>, '*'}
                                | ?PDF_CONTENT_TYPES
                               ]).

-define(ACCOUNTS_BY_SIMPLE_ID, <<"accounts/listing_by_simple_id">>).
-define(ALL_PORT_REQ_NUMBERS, <<"port_requests/all_port_in_numbers">>).
-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).
-define(LISTING_BY_NUMBER, <<"port_requests/listing_by_number">>).
-define(DESCENDANT_LISTING_BY_STATE, <<"port_requests/listing_by_descendant_state">>).
-define(AGENT_LISTING_BY_STATE, <<"port_requests/agent_listing_by_state">>).

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
          'false' |
          {'true', cb_context:context()} |
          {'stop', cb_context:context()}.
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), req_nouns(), req_verb()) ->
          boolean() |
          {'true', cb_context:context()} |
          {'stop', cb_context:context()}.
authorize(Context, [{<<"port_requests">>, []}], ?HTTP_GET) ->
    C1 = set_port_authority(Context),
    case cb_context:fetch(C1, 'is_port_authority', 'false')
        orelse cb_context:is_superduper_admin(C1)
    of
        'true' -> {'true', C1};
        'false' ->
            {'stop', cb_context:add_system_error('forbidden', C1)}
    end;
authorize(Context, [{<<"port_requests">>, []}], _) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize(_Context, _, _) ->
    'false'.

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
-spec acceptable_content_types() -> cowboy_content_types().
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
    Context1 = cb_context:set_db_name(Context, ?KZ_PORT_REQUESTS_DB),
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
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_port_requests(set_port_authority(Context), cb_context:req_verb(Context)).

-spec validate_port_requests(cb_context:context(), http_method()) -> cb_context:context().
validate_port_requests(Context, ?HTTP_GET) ->
    summary(Context);
validate_port_requests(Context, ?HTTP_PUT) ->
    create(Context).

%%------------------------------------------------------------------------------
%% @doc Validate operations on port doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    Comments = kzd_port_requests:comments(cb_context:req_data(Context), []),
    Setters = [{fun cb_context:store/3, 'req_comments', Comments}
              ,{fun cb_context:set_db_name/2, ?KZ_PORT_REQUESTS_DB}
              ],
    C1 = cb_context:setters(Context, Setters),
    validate_port_request(set_port_authority(C1), Id, cb_context:req_verb(C1)).

-spec validate_port_request(cb_context:context(), kz_term:ne_binary(), http_method()) ->
          cb_context:context().
validate_port_request(Context, ?PATH_TOKEN_LAST_SUBMITTED, ?HTTP_GET) ->
    C1 = cb_context:set_req_data(Context, kz_json:from_list([{<<"by_types">>, ?PATH_TOKEN_LAST_SUBMITTED}])),
    summary(set_port_authority(C1));
validate_port_request(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_port_request(Context, Id, ?HTTP_POST) ->
    update(Context, Id);
validate_port_request(Context, Id, ?HTTP_PATCH) ->
    ValidateFun = fun (_PortId, C) ->
                          OnSuccess = fun(InnerC) -> validate_port_comments(InnerC, fun kz_term:identity/1) end,
                          cb_context:validate_request_data(?SCHEMA, C, OnSuccess)
                  end,
    crossbar_doc:patch_and_validate(Id, Context, ValidateFun);
validate_port_request(Context, Id, ?HTTP_DELETE) ->
    is_deletable(load_port_request(Context, Id)).

-spec validate(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
validate(Context, Id, ToState=?PORT_SUBMITTED) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ToState=?PORT_PENDING) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ToState=?PORT_SCHEDULED) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ToState=?PORT_COMPLETED) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ToState=?PORT_REJECTED) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ToState=?PORT_CANCELED) ->
    patch_then_validate_then_maybe_transition(Context, Id, ToState);
validate(Context, Id, ?PORT_ATTACHMENT) ->
    validate_attachments(set_port_authority(Context), Id, cb_context:req_verb(Context));
validate(Context, Id, ?PATH_TOKEN_LOA) ->
    generate_loa(read(set_port_authority(Context), Id));
validate(Context, Id, ?PATH_TOKEN_TIMELINE) ->
    C1 = load_port_request(set_port_authority(Context), Id),
    case 'success' =:= cb_context:resp_status(C1) of
        'false' -> C1;
        'true' ->
            NewDoc = prepare_timeline(C1, cb_context:doc(C1)),
            cb_context:set_resp_data(C1, NewDoc)
    end.

-spec validate_attachments(cb_context:context(), kz_term:ne_binary(), http_method()) ->
          cb_context:context().
validate_attachments(Context, Id, ?HTTP_GET) ->
    summary_attachments(Context, Id);
validate_attachments(Context, Id, ?HTTP_PUT) ->
    read(Context, Id).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) ->
          cb_context:context().
validate(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    validate_attachment(set_port_authority(Context), Id, AttachmentId, cb_context:req_verb(Context)).

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
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, Id, ?PATH_TOKEN_LOA) ->
    lager:debug("load LOA for ~s", [Id]),
    cb_context:set_resp_data(Context, kz_json:encode(cb_context:doc(Context))).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Doc = cb_context:doc(Context1),
            cb_context:set_resp_data(Context1, knm_port_request:public_fields(Doc));
        _ ->
            Context1
    end.

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, Id, ?PORT_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)],
    AName = cb_modules_util:attachment_name(Filename, CT),
    Context1 = cb_context:set_db_name(Context, ?KZ_PORT_REQUESTS_DB),
    crossbar_doc:save_attachment(Id, AName, Contents, Context1, Opts).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context, Id, NewState=?PORT_SUBMITTED) ->
    case cb_modules_util:phonebook_port_in(Context) of
        {'ok', 'disabled'} ->
            save_then_maybe_notify(Context, Id, NewState);
        {'ok', Response} ->
            C1 = handle_phonebook_response(Context, Response),
            save_then_maybe_notify(C1, Id, NewState);
        {'error', Error} ->
            handle_phonebook_error(Context, Error)
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

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    save_and_send_comments(Context, Id, get_new_comments(Context)).

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
                kz_datamgr:delete_attachment(cb_context:db_name(Context), Id, AttachmentId)
        end,
    crossbar_doc:save_attachment(Id, AttachmentId, Contents, Context, Options).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    crossbar_doc:delete(Context).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) ->
          cb_context:context().
delete(Context, Id, ?PORT_ATTACHMENT, AttachmentName) ->
    crossbar_doc:delete_attachment(Id, AttachmentName, Context).

%%%=============================================================================
%%% Save functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
    NewDoc1 = maybe_set_scheduled_date_from_schedule_on(cb_context:doc(Context)),
    NewDoc = kzd_accounts:set_tree(NewDoc1, kzd_accounts:tree(cb_context:account_doc(Context))),
    NewerDoc = kz_json:delete_keys([<<"reason">> | knm_port_request:public_fields()], NewDoc),
    Context1 = cb_context:setters(Context
                                 ,[{fun cb_context:set_db_name/2, ?KZ_PORT_REQUESTS_DB}
                                  ,{fun cb_context:set_doc/2, NewerDoc}
                                  ]
                                 ),
    crossbar_doc:save(Context1).

-spec save_and_send_comments(cb_context:context(), kz_term:ne_binary(), kz_json:objects()) -> cb_context:context().
save_and_send_comments(Context, _, []) ->
    Context1 = save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Doc = cb_context:doc(Context1),
            cb_context:set_resp_data(Context1, knm_port_request:public_fields(Doc));
        _ ->
            Context1
    end;
save_and_send_comments(Context, Id, NewComments) ->
    case cb_modules_util:phonebook_comment(Context, NewComments) of
        {'ok', _} ->
            Context1 = save(Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    Doc = cb_context:doc(Context1),
                    send_port_comment_notifications(Context, Id, NewComments),
                    RespData = filter_private_comments(Context1, knm_port_request:public_fields(Doc)),
                    cb_context:set_resp_data(Context1, RespData);
                _ ->
                    Context1
            end;
        {'error', Error} ->
            handle_phonebook_error(Context, Error)
    end.

-spec save_then_maybe_notify(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
save_then_maybe_notify(Context, PortId, NewState) ->
    Context1 = save(Context),
    case 'success' =:= cb_context:resp_status(Context1) of
        'false' -> Context1;
        'true' ->
            Doc = cb_context:doc(Context1),
            send_port_comment_notifications(Context1, PortId),
            RespData = filter_private_comments(Context1, knm_port_request:public_fields(Doc)),
            Context2 = cb_context:set_resp_data(Context1, RespData),
            port_state_change_notify(Context2, PortId, NewState)
    end.

-spec maybe_set_scheduled_date_from_schedule_on(kz_json:object()) -> kz_json:object().
maybe_set_scheduled_date_from_schedule_on(Doc) ->
    case kz_json:get_ne_value(<<"schedule_on">>, Doc) of
        'undefined' -> Doc;
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

%%%=============================================================================
%%% Phonebook functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_phonebook_error(cb_context:context(), knm_phonebook:errors()) -> cb_context:context().
handle_phonebook_error(Context, #{code := Code
                                 ,payload := Payload
                                 ,reason := 'bad_phonebook'
                                 }=Error) ->
    Env = cb_context:resp_envelope(Context),
    Env1 = kz_json:set_values([{<<"passthrough">>, 'true'}
                              ,{<<"error_format">>, <<"phonebook">>}
                              ], Env),
    Setters = [{fun cb_context:set_resp_error_code/2, Code}
              ,{fun cb_context:set_resp_status/2, 'error'}
              ,{fun cb_context:set_resp_error_msg/2, <<"request is rejected by port automation system">>}
              ,{fun cb_context:set_validation_errors/2, kz_json:get_value(<<"data">>, Payload, kz_json:new())}
              ,{fun cb_context:set_resp_envelope/2, Env1}
              ],
    Context1 = maybe_reject_port(Context, Error),
    case cb_context:resp_status(Context1) of
        'success' ->
            %% returning successful failure
            Context1;
        _ ->
            %% for port_in: even rejecting the port is failed, shame!
            %% for add_comment: to let client know why comment did not added
            cb_context:setters(Context, Setters)
    end;
handle_phonebook_error(Context, #{payload := Payload}) ->
    cb_context:add_system_error('datastore_fault', kz_json:get_value(<<"message">>, Payload), Context).

%% Adding a second to transition timestamp, because the port is just transitioned
%% to submitted and it is immediately transitioning to rejected so if the seconds are same
%% this could cause the UI to not showing the 'Fix' button.
-define(IMMEDIATELY_REJECTION_FUDGE_SECONDS, 1).

-spec maybe_reject_port(cb_context:context(), knm_phonebook:errors()) -> cb_context:context().
maybe_reject_port(Context, #{code := Code
                            ,req_type := 'port_in'
                            ,payload := Payload
                            }) ->
    lager:debug("phonebook submission failed, rejecting the port request"),
    Reason = <<"There was some issue with your port request. Please wait while we investigate the issue.">>,
    PhonebookPayload = kz_json:from_list(
                         [{<<"http_code">>, kz_term:to_binary(Code)}
                         ,{<<"payload">>, Payload}
                         ]),
    CommentText = kz_term:to_binary(
                    ["Phonebook/carrier reject to submit port request.\n "
                    ,kz_json:encode(Payload, ['pretty'])
                    ]),
    Comment = kz_doc:setters(
                [{fun kzd_comment:set_account_id/2, cb_context:auth_account_id(Context)}
                ,{fun kzd_comment:set_action_required/2, 'false'}
                ,{fun kzd_comment:set_author/2, <<"Port Automation">>}
                ,{fun kzd_comment:set_content/2, CommentText}
                ,{fun kzd_comment:set_is_private/2, 'true'}
                ,{fun kzd_comment:set_timestamp/2, kz_time:now_s()}
                ]),
    Metadata = knm_port_request:transition_metadata(cb_context:auth_account_id(Context)
                                                   ,cb_context:auth_user_id(Context)
                                                   ,Reason
                                                   ,kz_time:now_s() + ?IMMEDIATELY_REJECTION_FUDGE_SECONDS
                                                   ),
    Context1 = move_state(Context, ?PORT_REJECTED, Metadata),
    case cb_context:resp_status(Context1) of
        'success' ->
            send_port_comment_notifications(Context, kz_doc:id(cb_context:doc(Context)), [Comment]),
            Doc = cb_context:doc(Context1),
            DocSetters = [{fun kzd_port_requests:set_comments/2, kzd_port_requests:comments(Doc, []) ++ [Comment]}
                         ,{fun kzd_port_requests:set_pvt_last_phonebook_error/2, PhonebookPayload}
                         ],
            CtxSetters = [{fun cb_context:set_doc/2, kz_doc:setters(Doc, DocSetters)}
                         ,{fun cb_context:store/3, 'req_comments', []}
                         ],
            Context2 = save(cb_context:setters(Context1, CtxSetters)),
            case cb_context:resp_status(Context2) of
                'success' ->
                    lager:debug("congratulation, port submission has been successfully rejected"),
                    Context2;
                _ ->
                    lager:warning("failed to save rejected port request, halp"),
                    Context2
            end;
        _ ->
            lager:warning("failed to reject port request, halp"),
            Context1
    end;
maybe_reject_port(Context, _) ->
    %% changing resp_status so we return phonebook error above why adding comment is failed
    Context1 = cb_context:store(Context, 'req_comments', []),
    cb_context:add_system_error('datastore_fault', <<"unable to submit comment to carrier">>, Context1).

-spec handle_phonebook_response(cb_context:context(), kz_json:object()) -> cb_context:context().
handle_phonebook_response(Context, Response) ->
    Data = kz_json:get_value(<<"data">>, Response, kz_json:new()),
    PhonebookComments = [fix_phonebook_comments(Comment)
                         || Comment <- kzd_port_requests:comments(Data, [])
                        ],

    Doc = cb_context:doc(Context),
    UpdatedComments = cb_comments:sort(kzd_port_requests:comments(Doc, []) ++ PhonebookComments),
    Merged = kz_json:merge([kz_json:delete_key(<<"comments">>, Doc)
                           ,kz_json:delete_key(<<"comments">>, Data)
                           ]
                          ),

    cb_context:set_doc(Context, kzd_port_requests:set_comments(Merged, UpdatedComments)).

-spec fix_phonebook_comments(kz_json:object()) -> kz_json:object().
fix_phonebook_comments(Comment) ->
    Setters = [{fun kzd_comment:set_action_required/2, kzd_comment:action_required(Comment, 'false')}
              ,{fun kzd_comment:set_author/2, kzd_comment:author(Comment, <<"phonebook">>)}
              ,{fun kzd_comment:set_content/2, kzd_comment:content(Comment, <<"phonebook comment">>)}
              ,{fun kzd_comment:set_is_private/2, kzd_comment:is_private(Comment, 'false')}
              ,{fun kzd_comment:set_timestamp/2, kzd_comment:timestamp(Comment, kz_time:now_s())}
              ],
    kz_doc:setters(Comment, Setters).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_port_request(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_port_request(Context, Id) ->
    Context1 = cb_context:set_db_name(Context, ?KZ_PORT_REQUESTS_DB),
    Context2 = crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST)),
    case cb_context:resp_status(Context2) =:= 'success' of
        'false' -> Context2;
        'true' ->
            Doc = cb_context:doc(Context2),
            Comments =
                [kzd_comment:migrate_to_is_private(Comment)
                 || Comment <- kzd_port_requests:comments(Doc, [])
                ],
            cb_context:set_doc(Context2, kzd_port_requests:set_comments(Doc, Comments))
    end.

-spec patch_then_validate_then_maybe_transition(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          cb_context:context().
patch_then_validate_then_maybe_transition(Context, PortId, ToState) ->
    OnSuccess = fun(C) ->
                        validate_port_comments(C, fun(OtherC) -> maybe_move_state(OtherC, ToState) end)
                end,
    ValidateFun = fun (_PortId, C) ->
                          cb_context:validate_request_data(?SCHEMA, C, OnSuccess)
                  end,
    Comments = kzd_port_requests:comments(cb_context:req_data(Context), []),
    Setters = [{fun cb_context:store/3, 'req_comments', Comments}
              ,{fun cb_context:set_db_name/2, ?KZ_PORT_REQUESTS_DB}
              ],
    Context1 = cb_context:setters(Context, Setters),
    LoadOptions = ?TYPE_CHECK_OPTION(?TYPE_PORT_REQUEST),
    crossbar_doc:patch_and_validate(PortId, set_port_authority(Context1), ValidateFun, LoadOptions).

%%%=============================================================================
%%% Internal Validate functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_port_comments(cb_context:context(), fun((cb_context:context()) -> cb_context:context())) ->
          cb_context:context().
validate_port_comments(Context, OnSuccess) ->
    validate_port_comments(Context, OnSuccess, get_new_comments(Context)).

-spec validate_port_comments(cb_context:context(), fun((cb_context:context()) -> cb_context:context()), kz_json:objects()) ->
          cb_context:context().
validate_port_comments(Context, OnSuccess, []) ->
    Doc = cb_context:doc(Context),
    Comments = kzd_port_requests:comments(Doc, []),
    NewDoc = kzd_port_requests:set_comments(Doc, cb_comments:sort(Comments)),
    OnSuccess(cb_context:set_doc(Context, NewDoc));
validate_port_comments(Context, OnSuccess, NewComments) ->
    C1 = validate_comments(Context, NewComments),
    case cb_context:resp_status(C1) =:= 'success' of
        'true' -> OnSuccess(C1);
        'false' -> C1
    end.

-spec validate_comments(cb_context:context(), kz_json:objects()) -> cb_context:context().
validate_comments(Context, NewComments) ->
    IsPortAuthority = cb_context:fetch(Context, 'is_port_authority', 'false')
        orelse cb_context:is_superduper_admin(Context),
    ContextToValidate = cb_context:set_req_data(Context, kzd_port_requests:set_comments(kz_json:new(), NewComments)),
    ValidatedContext = cb_context:validate_request_data(<<"comments">>, ContextToValidate),
    case cb_context:resp_status(ValidatedContext) =:= 'success' of
        'true' ->
            ValidatedComments = kzd_port_requests:comments(cb_context:doc(ValidatedContext)),
            validate_comments(Context, ValidatedComments, IsPortAuthority);
        'false' -> ValidatedContext
    end.

-spec validate_comments(cb_context:context(), kz_json:objects(), boolean()) -> cb_context:context().
validate_comments(Context, NewComments, 'false') ->
    Filters = [{kzd_comment:action_required_path(), fun kz_term:is_true/1}
              ,{kzd_comment:superduper_comment_path(), fun kz_term:is_true/1} %% old key
              ,{kzd_comment:is_private_path(), fun kz_term:is_true/1}
              ],
    case run_comment_filter(NewComments, Filters) of
        [] ->
            add_commentors_info(Context, NewComments);
        [_|_] ->
            Msg = kz_json:from_list_recursive(
                    [{<<"comments">>
                     ,[{<<"forbidden">>
                       ,[{<<"message">>, <<"you're not allowed to make private comment or set action_required">>}
                        ,{<<"cause">>, <<"comments">>}
                        ]
                       }
                      ]
                     }
                    ]),
            cb_context:add_system_error('forbidden', Msg, Context)
    end;
validate_comments(Context, NewComments, 'true') ->
    Filters = [{kzd_comment:superduper_comment_path(), fun kz_term:is_true/1} %% old key
              ,{kzd_comment:is_private_path(), fun kz_term:is_true/1}
              ],
    case [Comment
          || Comment <- run_comment_filter(NewComments, Filters),
             kz_json:is_true(<<"action_required">>, Comment, 'false')
         ]
    of
        [] ->
            add_commentors_info(Context, NewComments);
        [_|_] ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"setting action_required on private comment is not allowed">>}
                    ,{<<"cause">>, <<"comments">>}
                    ]),
            cb_context:add_validation_error(<<"comments">>, <<"rejected">>, Msg, Context)
    end.

-spec add_commentors_info(cb_context:context(), kz_json:objects()) -> cb_context:context().
add_commentors_info(Context, NewComments) ->
    Doc = cb_context:doc(Context),
    DbComments = kzd_port_requests:comments(cb_context:fetch(Context, 'db_doc'), []),
    Updated = [update_comment(Context, Comment)
               || Comment <- NewComments
              ],
    Setters = [{fun cb_context:set_doc/2, kzd_port_requests:set_comments(Doc, cb_comments:sort(DbComments ++ Updated))}
              ,{fun cb_context:store/3, 'req_comments', Updated}
              ],
    cb_context:setters(Context, Setters).

-spec update_comment(cb_context:context(), kz_json:object()) -> kz_json:object().
update_comment(Context, Comment) ->
    Setters = [{fun kzd_comment:set_is_private/2, kzd_comment:is_private_legacy(Comment)}
               | get_commentor_info(Context, cb_context:auth_doc(Context))
              ],
    kz_doc:setters(kz_json:delete_key(kzd_comment:superduper_comment_path(), Comment), Setters).

-spec get_commentor_info(cb_context:context(), kz_term:api_object()) -> kz_term:proplist().
get_commentor_info(Context, 'undefined') ->
    [{fun kzd_comment:set_account_id/2, cb_context:auth_account_id(Context)}];
get_commentor_info(Context, AuthDoc) ->
    case kz_doc:type(AuthDoc) of
        <<"user">> ->
            [{fun kzd_comment:set_account_id/2, cb_context:auth_account_id(Context)}
            ,{fun kzd_comment:set_user_id/2, kz_doc:id(AuthDoc)}
            ,{fun kzd_comment:set_author/2, kzd_users:full_name(AuthDoc)}
            ];
        _LOL ->
            AuthAccountId = cb_context:auth_account_id(Context),
            UserId = kz_json:get_value(<<"owner_id">>, AuthDoc),
            case kzd_users:fetch(AuthAccountId, UserId) of
                {'ok', UserDoc} ->
                    [{fun kzd_comment:set_account_id/2, AuthAccountId}
                    ,{fun kzd_comment:set_user_id/2, UserId}
                    ,{fun kzd_comment:set_author/2, kzd_users:full_name(UserDoc)}
                    ];
                {'error', _} ->
                    [{fun kzd_comment:set_account_id/2, cb_context:auth_account_id(Context)}
                    ,{fun kzd_comment:set_user_id/2, UserId}
                    ]
            end
    end.

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

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
update(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(C, Id) end,
    cb_context:validate_request_data(?SCHEMA, Context, OnSuccess).

%%%=============================================================================
%%% Summary functions
%%%=============================================================================

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

%%%=============================================================================
%%% Summary by Types functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summarize_by_types(cb_context:context()) -> cb_context:context().
summarize_by_types(Context) ->
    case cb_context:req_value(Context, <<"by_types">>) of
        'undefined' -> load_summary_by_types(Context, ?PORT_ACTIVE_STATES);
        <<"active">> -> load_summary_by_types(Context, ?PORT_ACTIVE_STATES);
        <<"all">> -> load_summary_by_types(Context, ?PORT_STATES);
        <<"progressing">> -> load_summary_by_types(Context, ?PORT_PROGRESSING_STATES);
        <<"suspended">> -> load_summary_by_types(Context, ?PORT_SUSPENDED_STATES);
        <<"completed">> -> load_summary_by_types(Context, ?PORT_COMPLETED_STATES);
        ?PATH_TOKEN_LAST_SUBMITTED -> last_submitted(Context);
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_summary_by_types(cb_context:context(), kz_term:ne_binaries()) -> cb_context:context().
load_summary_by_types(Context, Types) ->
    lager:debug("loading summary for port requests with types ~s"
               ,[kz_binary:join(Types)]
               ),
    load_summary_by_types(Context, Types, []).

-spec load_summary_by_types(cb_context:context(), kz_term:ne_binaries(), kz_json:objects()) -> cb_context:context().
load_summary_by_types(Context, [], JObjs) ->
    Setters = [{fun cb_context:set_resp_data/2, JObjs}
              ,{fun cb_context:set_resp_status/2, 'success'}
              ],
    normalize_type_summarize_results(cb_context:setters(Context, Setters));
load_summary_by_types(Context, [Type | Types], JObjs) ->
    C1 = load_summary_by_type(Context, Type),
    case cb_context:resp_status(C1) =:= 'success' of
        'true' ->
            RespData = cb_context:resp_data(C1),
            load_summary_by_types(Context, Types, JObjs ++ RespData);
        'false' -> C1
    end.

load_summary_by_type(Context, Type) ->
    lager:debug("loading summary for port requests with type ~s"
               ,[Type]
               ),
    IsRanged = lists:member(Type, ?PORT_COMPLETED_STATES),
    AuthorityType = cb_context:fetch(Context, 'authority_type'),
    View = type_summarize_view_name(AuthorityType),
    Options = [{'mapper', fun normalize_view_results/2}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,{'unchunkable', 'true'}
              ,{'should_paginate', 'false'}
              ,'include_docs'
               | by_types_view_options(Context, Type, IsRanged, AuthorityType)
              ],
    case IsRanged of
        'true' -> crossbar_view:load_time_range(Context, View, Options);
        'false' -> crossbar_view:load(Context, View, Options)
    end.

-spec by_types_view_options(cb_context:context(), kz_term:ne_binary(), boolean(), authority_type()) -> crossbar_view:options().
by_types_view_options(Context, Type, 'true', 'agent') ->
    [{'range_start_keymap', [cb_context:fetch(Context, 'port_authority_id'), Type]}
    ,{'range_end_keymap', [cb_context:fetch(Context, 'port_authority_id'), Type]}
    ,{'range_key_name', <<"modified">>}
    ];
by_types_view_options(Context, Type, 'false', 'agent') ->
    [{'startkey', [cb_context:fetch(Context, 'port_authority_id'), Type]}
    ,{'endkey', [cb_context:fetch(Context, 'port_authority_id'), Type, kz_json:new()]}
    ];
by_types_view_options(Context, Type, 'true', _) ->
    [{'range_start_keymap', [cb_context:account_id(Context), Type]}
    ,{'range_end_keymap', [cb_context:account_id(Context), Type]}
    ,{'range_key_name', <<"modified">>}
    ];
by_types_view_options(Context, Type, 'false', _) ->
    [{'startkey', [cb_context:account_id(Context), Type]}
    ,{'endkey', [cb_context:account_id(Context), Type, kz_json:new()]}
    ].

-spec type_summarize_view_name(authority_type()) -> kz_term:ne_binary().
type_summarize_view_name('account') ->
    ?LISTING_BY_STATE;
type_summarize_view_name('agent') ->
    ?AGENT_LISTING_BY_STATE;
type_summarize_view_name('descendants') ->
    ?DESCENDANT_LISTING_BY_STATE.

-spec normalize_type_summarize_results(cb_context:context()) -> cb_context:context().
normalize_type_summarize_results(Context) ->
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
    Options = [{'keymap', knm_converters:normalize(Number)}
              ,{'mapper', fun normalize_number_summarize_results/3}
              ,{'databases', [?KZ_PORT_REQUESTS_DB]}
              ,'include_docs'
              ],
    crossbar_view:load(Context, ?LISTING_BY_NUMBER, Options).

-spec normalize_number_summarize_results(cb_context:context(), kz_json:object(), kz_json:objects()) ->
          kz_json:objects().
normalize_number_summarize_results(Context, JObj, Acc) ->
    AccountId = case cb_context:account_id(Context) of
                    'undefined' -> cb_context:auth_account_id(Context);
                    Id -> Id
                end,
    case lists:member(AccountId, kzd_accounts:tree(kz_json:get_value(<<"doc">>, JObj)))
        orelse cb_context:is_superduper_admin(Context)
    of
        'true' -> normalize_view_results(JObj, Acc);
        'false' -> Acc
    end.

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
    crossbar_view:load(Context, <<"port_requests/listing_submitted">>, Options).

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
    Comments = kzd_port_requests:comments(filter_private_comments(Context, Doc), []),
    Transitions = kzd_port_requests:pvt_transitions(Doc, []),
    Indexed = [{kz_json:get_integer_value(?TRANSITION_TIMESTAMP, JObj), JObj}
               || JObj <- Comments ++ Transitions
              ],
    {_, NewDocs} = lists:unzip(lists:keysort(1, Indexed)),
    NewDocs.

-spec filter_private_comments(cb_context:context(), kz_json:object()) -> kz_json:object().
filter_private_comments(Context, JObj) ->
    Filters = [{[<<"is_private">>], fun kz_term:is_false/1}
              ],
    Comments =
        [kzd_comment:migrate_to_is_private(Comment)
         || Comment <- kzd_port_requests:comments(JObj, [])
        ],
    case kzd_port_requests:pvt_port_authority(JObj) =:= cb_context:auth_account_id(Context)
        orelse cb_context:fetch(Context, 'is_port_authority')
        orelse cb_context:is_superduper_admin(Context)
    of
        'false' ->
            kzd_port_requests:set_comments(JObj, run_comment_filter(Comments, Filters));
        'true'  ->
            kzd_port_requests:set_comments(JObj, Comments)
    end.

-spec run_comment_filter([kzd_comment:doc()], [{kz_json:get_key(), fun((kz_json:json_term()) -> boolean())}]) ->
          [kzd_comment:doc()].
run_comment_filter(Comments, Filters) ->
    [Comment
     || Comment <- Comments,
        lists:any(fun({Key, Filter}) ->
                          Filter(kz_json:get_value(Key, Comment))
                  end
                 ,Filters
                 )
    ].

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
                                      ,cb_context:set_db_name(Context, ?KZ_PORT_REQUESTS_DB)
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
            validate_port_comments(Context, fun(C) -> successful_validation(C, Id) end);
        _ -> Context1
    end;
on_successful_validation(Context, _Id, 'false') ->
    PortState = kzd_port_requests:pvt_port_state(cb_context:doc(Context)),
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
    lager:debug("check can update port request: ~p", [kz_doc:id(cb_context:doc(Context))]),
    can_update_port_request(Context, knm_port_request:current_state(cb_context:doc(Context))).

-spec can_update_port_request(cb_context:context(), kz_term:ne_binary()) -> boolean().
can_update_port_request(_Context, ?PORT_UNCONFIRMED) ->
    lager:debug("port is in unconfirmed state, allowing update"),
    'true';
can_update_port_request(_Context, ?PORT_REJECTED) ->
    lager:debug("port is in rejected state, allowing update"),
    'true';
can_update_port_request(Context, _State) ->
    lager:debug("port is in ~s state, check if the requestor is superduper admin", [_State]),
    cb_context:is_superduper_admin(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec successful_validation(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
successful_validation(Context, 'undefined') ->
    PortReq = knm_port_request:new(cb_context:doc(Context)
                                  ,[{'auth_by', cb_context:auth_account_id(Context)}
                                   ,{'auth_user_id', cb_context:auth_user_id(Context)}
                                   ,{'port_authority_id', cb_context:fetch(Context, 'port_authority_id')}
                                   ,{'port_authority_name', kzd_accounts:fetch_name(cb_context:fetch(Context, 'port_authority_id'))}
                                   ,{'account_name', cb_context:account_name(Context)}
                                   ]
                                  ),
    cb_context:set_doc(Context, PortReq);
successful_validation(Context, _Id) ->
    Normalized = knm_port_request:normalize_numbers(cb_context:doc(Context)),
    cb_context:set_doc(Context, Normalized).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_number_portability(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) ->
          cb_context:context().
check_number_portability(PortId, Number, Context) ->
    E164 = knm_converters:normalize(Number),
    lager:debug("checking ~s(~s) for portability", [E164, Number]),
    case knm_port_request:get_portin_number(cb_context:account_id(Context), E164) of
        {'error', Reason}->
            lager:debug("failed to check ports for number portability: ~p", [Reason]),
            crossbar_doc:handle_datamgr_errors(Reason, PortId, Context);
        {'ok', Result} ->
            check_number_portability(PortId, Number, Context, E164, Result)
    end.

-spec check_number_portability(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), kz_json:objects()) ->
          cb_context:context().
check_number_portability(_PortId, Number, Context, E164, []) ->
    check_number_existence(E164, Number, Context);
check_number_portability(PortId, Number, Context, E164, [PortReq]) ->
    case {kz_doc:account_id(PortReq) =:= cb_context:account_id(Context)
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
                       ,[E164, Number, kz_doc:account_id(PortReq)]),
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
    case knm_numbers:lookup_account(E164) of
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
                                           ,cb_context:set_db_name(Context, ?KZ_PORT_REQUESTS_DB)
                                           ),
    Headers =
        #{<<"content-disposition">> => <<"attachment; filename=", AttachmentId/binary>>
         ,<<"content-type">> => kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)
         },
    cb_context:add_resp_headers(Context1, Headers).

-spec maybe_move_state(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_move_state(Context, PortState) ->
    maybe_move_state(Context, PortState, cb_context:resp_status(Context)).

maybe_move_state(Context, PortState, 'success') ->
    Metadata = knm_port_request:transition_metadata(cb_context:auth_account_id(Context)
                                                   ,cb_context:auth_user_id(Context)
                                                   ,cb_context:req_value(Context, ?REQ_TRANSITION)
                                                   ),
    move_state(Context, PortState, Metadata);
maybe_move_state(Context, _, _) ->
    Context.

-spec move_state(cb_context:context(), kz_term:ne_binary(), knm_port_request:transition_metadata()) -> cb_context:context().
move_state(Context, PortState, Metadata) ->
    try knm_port_request:attempt_transition(cb_context:doc(Context), Metadata, PortState) of
        {'ok', PortRequest} ->
            lager:debug("loaded new port request state ~s", [PortState]),
            cb_context:set_doc(Context, PortRequest);
        {'error', 'user_not_allowed'} ->
            Msg = kz_json:from_list(
                    [{<<"message">>
                     ,<<"you're not allowed to change to new state from current state, please contact your port authority">>
                     }
                    ,{<<"cause">>, PortState}
                    ]),
            cb_context:add_validation_error(<<"port_state">>, <<"enum">>, Msg, Context);
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
    EncodedCarrierName = kz_term:to_lower_binary(kz_http_util:urlencode(CarrierName)),
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
-spec port_state_change_notify(cb_context:context(), path_token(), path_token()) -> cb_context:context().
port_state_change_notify(Context, Id, State) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
            ,{<<"Port-Request-ID">>, Id}
            ,{<<"Reason">>, state_change_reason(Context, cb_context:req_value(Context, ?REQ_TRANSITION))}
            ,{<<"Version">>, cb_context:api_version(Context)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    try
        lager:debug("sending port ~s notification for port request ~s", [State, Id]),
        kapps_notify_publisher:cast(Req, state_change_notify_fun(State)),
        Context
    catch
        _E:_R ->
            lager:debug("failed to send the  port ~s notification, maybe reverting the state change: ~s:~p"
                       ,[State, _E, _R]
                       ),
            maybe_revert_patch(Context, cb_modules_util:should_send_to_phonebook(Context))
    end.

-spec state_change_notify_fun(kz_term:ne_binary()) -> function().
state_change_notify_fun(?PORT_SUBMITTED) ->
    fun kapi_notifications:publish_port_request/1;
state_change_notify_fun(?PORT_PENDING) ->
    fun kapi_notifications:publish_port_pending/1;
state_change_notify_fun(?PORT_SCHEDULED) ->
    fun kapi_notifications:publish_port_scheduled/1;
state_change_notify_fun(?PORT_COMPLETED) ->
    fun kapi_notifications:publish_ported/1;
state_change_notify_fun(?PORT_REJECTED) ->
    fun kapi_notifications:publish_port_rejected/1;
state_change_notify_fun(?PORT_CANCELED) ->
    fun kapi_notifications:publish_port_cancel/1.

-spec state_change_reason(cb_context:context(), kz_term:api_ne_binary()) -> kz_term:api_object().
state_change_reason(Context, ?NE_BINARY=Reason) ->
    kz_json:from_list(
      [{<<"user_id">>, cb_context:auth_user_id(Context)}
      ,{<<"account_id">>, cb_context:auth_account_id(Context)}
      ,{<<"timestamp">>, kz_doc:modified(cb_context:doc(Context))}
      ,{<<"content">>, Reason}
      ]
     );
state_change_reason(_, _) ->
    'undefined'.

-spec maybe_revert_patch(cb_context:context(), boolean()) -> cb_context:context().
maybe_revert_patch(Context, 'true') ->
    lager:debug("port is managed by phonebook, can not revert"),
    Context;
maybe_revert_patch(Context, 'false') ->
    lager:debug("port is not managed by phonebook, reverting"),
    Doc = cb_context:doc(Context),
    DBDoc = cb_context:fetch(Context, 'db_doc'),
    Rev = kz_doc:revision(Doc),
    RevertedDoc = kz_doc:set_revision(DBDoc, Rev),
    _ = crossbar_doc:save(cb_context:set_doc(Context, RevertedDoc)),
    Msg = kz_json:from_list([{<<"message">>, <<"failed to send port state change email notification">>}]),
    cb_context:add_system_error('bad_gateway', Msg, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_new_comments(cb_context:context()) -> kz_json:objects().
get_new_comments(Context) ->
    ReqComments = cb_comments:sort(cb_context:fetch(Context, 'req_comments', [])),
    case cb_comments:sort(kz_json:get_list_value(<<"comments">>, cb_context:fetch(Context, 'db_doc'), [])) of
        [] ->
            ReqComments;
        Comments ->
            OldTime = kz_json:get_integer_value(<<"timestamp">>, lists:last(Comments)),
            [Comment
             || Comment <- ReqComments,
                kz_json:get_integer_value(<<"timestamp">>, Comment) > OldTime
            ]
    end.

-spec send_port_comment_notifications(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
send_port_comment_notifications(Context, Id) ->
    send_port_comment_notifications(Context, Id, get_new_comments(Context)).

-spec send_port_comment_notifications(cb_context:context(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
send_port_comment_notifications(Context, Id, NewComments) ->
    _ = lists:foldl(fun send_port_comment_notification/2, {Context, Id, length(NewComments), 1}, NewComments),
    'ok'.

-spec send_port_comment_notification(kz_json:object(), {cb_context:context(), kz_term:ne_binary(), non_neg_integer(), non_neg_integer()}) ->
          {cb_context:context(), kz_term:ne_binary(), non_neg_integer(), non_neg_integer()}.
send_port_comment_notification(NewComment, {Context, Id, TotalNew, Index}) ->
    Setters = [{fun kzd_comment:set_user_id/2, kzd_comment:user_id(NewComment, cb_context:auth_user_id(Context))}
              ,{fun kzd_comment:set_account_id/2, kzd_comment:account_id(NewComment, cb_context:auth_account_id(Context))}
              ],
    Comment = kz_doc:setters(NewComment, Setters),
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"Authorized-By">>, cb_context:auth_account_id(Context)}
          ,{<<"Port-Request-ID">>, Id}
          ,{<<"Comment">>, Comment}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending port comment notification ~b/~b", [Index, TotalNew]),
    try kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_port_comment/1) of
        _ ->
            lager:debug("port comment notification sent ~b/~b", [Index, TotalNew]),
            {Context, Id, TotalNew, Index+1}
    catch
        _E:_R ->
            lager:error("failed to send the port comment notification ~b/~b: ~s:~p", [Index, TotalNew, _E, _R]),
            {Context, Id, TotalNew, Index+1}
    end.

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_port_authority(cb_context:context()) -> cb_context:context().
set_port_authority(Context) ->
    {Type, AccountId} = authority_type(Context, cb_context:req_nouns(Context)),
    set_port_authority(Context, Type, find_port_authority(Type, AccountId)).

-type authority_type() :: 'agent' | 'account' | 'descendants'.

-spec authority_type(cb_context:context(), req_nouns()) -> {authority_type() , kz_term:ne_binary()}.
authority_type(Context, Nouns) ->
    Accounts = props:get_value(<<"accounts">>, Nouns),
    authority_type(Context, Nouns, Accounts).

-spec authority_type(cb_context:context(), req_nouns(), path_tokens() | 'undefined') -> {authority_type(), kz_term:ne_binary()}.
authority_type(Context, _Nouns, 'undefined') ->
    {'agent', cb_context:auth_account_id(Context)};
authority_type(Context, _Nouns, [_AccountId, ?DESCENDANTS]) ->
    {'descendants', cb_context:account_id(Context)};
authority_type(Context, _Nouns, [_AccountId | _]) ->
    {'account', cb_context:account_id(Context)}.

-spec find_port_authority(authority_type(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
find_port_authority('agent', AccountId) ->
    case kzd_whitelabel:fetch_port_authority(AccountId, 'undefined') of
        AccountId -> AccountId;
        _ -> kzd_port_requests:find_port_authority(AccountId)
    end;
find_port_authority(_, AccountId) ->
    kzd_port_requests:find_port_authority(AccountId).

-spec set_port_authority(cb_context:context(), authority_type(), kz_term:api_ne_binary()) -> cb_context:context().
set_port_authority(Context, Type, 'undefined') ->
    lager:warning("set undefined port authority to master"),
    {'ok', MasterId} = kapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    Setters = [{fun cb_context:store/3, 'port_authority_id', MasterId}
              ,{fun cb_context:store/3, 'is_port_authority', AuthAccountId =:= MasterId}
              ,{fun cb_context:store/3, 'authority_type', Type}
              ],
    cb_context:setters(Context, Setters);
set_port_authority(Context, Type, PortAuthority) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    Setters = [{fun cb_context:store/3, 'port_authority_id', PortAuthority}
              ,{fun cb_context:store/3, 'is_port_authority', AuthAccountId =:= PortAuthority}
              ,{fun cb_context:store/3, 'authority_type', Type}
              ],
    cb_context:setters(Context, Setters).
