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
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3, content_types_provided/4
         ,content_types_accepted/3, content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,get/3
         ,put/1, put/3
         ,post/2, post/3, post/4
         ,delete/2, delete/4
         ,cleanup/1

         ,update_default_template/0
         ,find_template/1, find_template/2
         ,authority/1
        ]).

-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle_number_manager/include/wh_port_request.hrl").
-include("../crossbar.hrl").

-define(MY_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".port_requests">>).

-define(TEMPLATE_DOC_ID, <<"notify.loa">>).
-define(TEMPLATE_ATTACHMENT_ID, <<"template">>).

-define(LOA_BUILDER, whapps_config:get(?MY_CONFIG_CAT, <<"loa_builder">>, <<"htmldoc">>)).

-define(ATTACHMENT_MIME_TYPES, [{<<"application">>, <<"pdf">>}
                                ,{<<"application">>, <<"octet-stream">>}
                                ,{<<"text">>, <<"plain">>}
                               ]).

-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(PORT_REQ_NUMBERS, <<"port_requests/port_in_numbers">>).
-define(ALL_PORT_REQ_NUMBERS, <<"port_requests/all_port_in_numbers">>).
-define(LISTING_BY_STATE, <<"port_requests/listing_by_state">>).

-define(UNFINISHED_PORT_REQUEST_LIFETIME
        ,whapps_config:get_integer(?MY_CONFIG_CAT, <<"unfinished_port_request_lifetime_s">>, ?SECONDS_IN_DAY * 30)
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
    wh_port_request:init(),

    Bindings = [{crossbar_cleanup:binding_system(), 'cleanup'}
                ,{<<"*.allowed_methods.port_requests">>, 'allowed_methods'}
                ,{<<"*.resource_exists.port_requests">>, 'resource_exists'}
                ,{<<"*.content_types_provided.port_requests">>, 'content_types_provided'}
                ,{<<"*.content_types_accepted.port_requests">>, 'content_types_accepted'}
                ,{<<"*.validate.port_requests">>, 'validate'}
                ,{<<"*.execute.get.port_requests">>, 'get'}
                ,{<<"*.execute.put.port_requests">>, 'put'}
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
-spec cleanup(ne_binary(), wh_json:objects()) -> 'ok'.

cleanup(?KZ_PORT_REQUESTS_DB = Db) ->
    ModifiedBefore = wh_util:current_tstamp() - ?UNFINISHED_PORT_REQUEST_LIFETIME,
    ViewOpts = [{'startkey', [0]}
                ,{'endkey', [ModifiedBefore]}
                ,{'limit', couch_util:max_bulk_insert()}
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
allowed_methods(?PORT_COMPLETE) ->
    [?HTTP_GET];
allowed_methods(?PORT_REJECT) ->
    [?HTTP_GET];
allowed_methods(?PORT_CANCELED) ->
    [?HTTP_GET];
allowed_methods(_Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_Id, ?PORT_SUBMITTED) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_PENDING) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_SCHEDULED) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_COMPLETE) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_REJECT) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_CANCELED) ->
    [?HTTP_POST];
allowed_methods(_Id, ?PORT_ATTACHMENT) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(_Id, ?PATH_TOKEN_LOA) ->
    [?HTTP_GET].

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

resource_exists(_Id, ?PORT_SUBMITTED) -> 'true';
resource_exists(_Id, ?PORT_PENDING) -> 'true';
resource_exists(_Id, ?PORT_SCHEDULED) -> 'true';
resource_exists(_Id, ?PORT_COMPLETE) -> 'true';
resource_exists(_Id, ?PORT_REJECT) -> 'true';
resource_exists(_Id, ?PORT_CANCELED) -> 'true';
resource_exists(_Id, ?PORT_ATTACHMENT) -> 'true';
resource_exists(_Id, ?PATH_TOKEN_LOA) -> 'true';
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
content_types_provided(Context) ->
    Context.

content_types_provided(Context, _Id) ->
    Context.

content_types_provided(Context, _Id, ?PATH_TOKEN_LOA) ->
    cb_context:add_content_types_provided(Context, [{'to_binary', [{<<"application">>, <<"x-pdf">>}]}]);
content_types_provided(Context, _Id, _) ->
    Context.

content_types_provided(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET -> content_types_provided_get(Context, Id, AttachmentId);
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
    validate_port_requests(Context, cb_context:req_verb(Context)).

validate(Context, ?PORT_SUBMITTED = Type) ->
    validate_load_requests(Context, Type);
validate(Context, ?PORT_PENDING = Type) ->
    validate_load_requests(Context, Type);
validate(Context, ?PORT_SCHEDULED = Type) ->
    validate_load_requests(Context, Type);
validate(Context, ?PORT_COMPLETE = Type) ->
    validate_load_requests(Context, Type);
validate(Context, ?PORT_REJECT = Type) ->
    validate_load_requests(Context, Type);
validate(Context, ?PORT_CANCELED = Type) ->
    validate_load_requests(Context, Type);
validate(Context, Id) ->
    validate_port_request(Context, Id, cb_context:req_verb(Context)).

validate(Context, Id, ?PORT_SUBMITTED) ->
    validate_port_request(Context, Id, ?PORT_SUBMITTED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_PENDING) ->
    validate_port_request(Context, Id, ?PORT_PENDING, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_SCHEDULED) ->
    validate_port_request(Context, Id, ?PORT_SCHEDULED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_COMPLETE) ->
    validate_port_request(Context, Id, ?PORT_COMPLETE, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_REJECT) ->
    validate_port_request(Context, Id, ?PORT_REJECT, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_CANCELED) ->
    validate_port_request(Context, Id, ?PORT_CANCELED, cb_context:req_verb(Context));
validate(Context, Id, ?PORT_ATTACHMENT) ->
    validate_attachments(Context, Id, cb_context:req_verb(Context));
validate(Context, Id, ?PATH_TOKEN_LOA) ->
    generate_loa(read(Context, Id)).

validate(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    validate_attachment(Context, Id, AttachmentId, cb_context:req_verb(Context)).

-spec validate_port_requests(cb_context:context(), http_method()) ->
                                    cb_context:context().
validate_port_requests(Context, ?HTTP_GET) ->
    validate_get_port_requests(
      Context
      ,props:get_value(<<"accounts">>, cb_context:req_nouns(Context))
      ,cb_context:req_value(Context, <<"by_number">>)
     );
validate_port_requests(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_load_requests(cb_context:context(), ne_binary()) ->
                                    cb_context:context().
validate_load_requests(Context, ?PORT_COMPLETE = Type) ->
    case cb_modules_util:range_view_options(Context) of
        {From, To} ->
            lager:debug("loading requests for ~s from ~p to ~p", [Type, From, To]),
            load_requests(Context
                          ,[{'startkey', [cb_context:account_id(Context), Type, To]}
                            ,{'endkey', [cb_context:account_id(Context), Type, From]}
                           ]
                         );
        Context1 -> Context1
    end;
validate_load_requests(Context, <<_/binary>> = Type) ->
    lager:debug("loading requests for ~s", [Type]),
    load_requests(cb_context:set_should_paginate(Context, 'false')
                  ,[{'startkey', [cb_context:account_id(Context), Type, wh_json:new()]}
                    ,{'endkey', [cb_context:account_id(Context), Type]}
                   ]
                 ).

-spec load_requests(cb_context:context(), crossbar_doc:view_options()) ->
                           cb_context:context().
load_requests(Context, ViewOptions) ->
    crossbar_doc:load_view(?LISTING_BY_STATE
                           ,['include_docs'
                             ,'descending'
                             | ViewOptions
                            ]
                           ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                           ,fun normalize_view_results/2
                          ).

-spec validate_get_port_requests(cb_context:context(), path_tokens(), api_binary()) ->
                                        cb_context:context().
validate_get_port_requests(Context, [_AccountId], 'undefined') ->
    summary(Context);
validate_get_port_requests(Context, [_AccountId], ByNumber) ->
    summary_by_number(Context, ByNumber);
validate_get_port_requests(Context, [_AccountId, ?PORT_DESCENDANTS], 'undefined') ->
    read_descendants(Context);
validate_get_port_requests(Context, [_AccountId, ?PORT_DESCENDANTS], ByNumber) ->
    read_descendants_by_number(Context, ByNumber).

-spec validate_port_request(cb_context:context(), ne_binary(), http_method()) ->
                                   cb_context:context().
validate_port_request(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_port_request(Context, Id, ?HTTP_POST) ->
    update(Context, Id);
validate_port_request(Context, Id, ?HTTP_DELETE) ->
    is_deletable(crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB))).

-spec validate_port_request(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                   cb_context:context().
validate_port_request(Context, Id, ?PORT_SUBMITTED, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_SUBMITTED);
validate_port_request(Context, Id, ?PORT_PENDING, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_PENDING);
validate_port_request(Context, Id, ?PORT_SCHEDULED, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_SCHEDULED);
validate_port_request(Context, Id, ?PORT_COMPLETE, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_COMPLETE);
validate_port_request(Context, Id, ?PORT_REJECT, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_REJECT);
validate_port_request(Context, Id, ?PORT_CANCELED, ?HTTP_POST) ->
    maybe_move_state(Context, Id, ?PORT_CANCELED).

-spec validate_attachments(cb_context:context(), ne_binary(), http_method()) ->
                                 cb_context:context().
validate_attachments(Context, Id, ?HTTP_GET) ->
    summary_attachments(Context, Id);
validate_attachments(Context, Id, ?HTTP_PUT) ->
    read(Context, Id).

-spec validate_attachment(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                 cb_context:context().
validate_attachment(Context, Id, AttachmentId, ?HTTP_GET) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_POST) ->
    load_attachment(Id, AttachmentId, Context);
validate_attachment(Context, Id, AttachmentId, ?HTTP_DELETE) ->
    is_deletable(load_attachment(Id, AttachmentId, Context)).

-spec is_deletable(cb_context:context()) -> cb_context:context().
-spec is_deletable(cb_context:context(), ne_binary()) -> cb_context:context().
is_deletable(Context) ->
    is_deletable(Context, wh_port_request:current_state(cb_context:doc(Context))).
is_deletable(Context, ?PORT_WAITING) -> Context;
is_deletable(Context, ?PORT_REJECT) -> Context;
is_deletable(Context, ?PORT_CANCELED) -> Context;
is_deletable(Context, _PortState) ->
    lager:debug("port is in state ~s, can't modify", [_PortState]),
    cb_context:add_system_error('invalid_method'
                                ,<<"port request is not modifiable in this state">>
                                ,Context
                               ).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, Id, ?PATH_TOKEN_LOA) ->
    lager:debug("load LOA for ~s", [Id]),
    cb_context:set_resp_data(Context, wh_json:encode(cb_context:doc(Context))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)).

put(Context, Id, ?PORT_ATTACHMENT) ->
    [{Filename, FileJObj}] = cb_context:req_files(Context),

    Contents = wh_json:get_value(<<"contents">>, FileJObj),

    CT = wh_json:get_string_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'headers', [{'content_type', CT}]}],

    crossbar_doc:save_attachment(Id
                                 ,cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents
                                 ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                 ,Opts
                                ).

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

post(Context, Id, ?PORT_SUBMITTED) ->
    Callback =
        fun() ->
            _  = add_to_phone_numbers_doc(Context),
            try send_port_request_notification(Context, Id) of
                _ ->
                    lager:debug("port request notification sent"),
                    do_post(Context, Id)
            catch
                _E:_R ->
                    lager:debug("failed to send the port request notification: ~s:~p", [_E, _R]),
                    cb_context:add_system_error(
                      'bad_gateway'
                      ,<<"failed to send port request email">>
                      ,Context
                     )
            end
        end,
    crossbar_services:maybe_dry_run(Context, Callback);
post(Context, Id, ?PORT_PENDING) ->
    try send_port_pending_notification(Context, Id) of
        _ ->
            lager:debug("port pending notification sent"),
            do_post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the port pending notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(
              'bad_gateway'
              ,<<"failed to send port pending email">>
              ,Context
             )
    end;
post(Context, Id, ?PORT_SCHEDULED) ->
    try send_port_scheduled_notification(Context, Id) of
        _ ->
            lager:debug("port scheduled notification sent"),
            do_post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the port scheduled notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(
              'bad_gateway'
              ,<<"failed to send port scheduled email">>
              ,Context
             )
    end;
post(Context, Id, ?PORT_COMPLETE) ->
    try send_ported_notification(Context, Id) of
        _ ->
            lager:debug("ported notification sent"),
            do_post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the ported notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(
              'bad_gateway'
              ,<<"failed to send ported email">>
              ,Context
             )
    end;
post(Context, Id, ?PORT_REJECT) ->
    _ = remove_from_phone_numbers_doc(Context),
    try send_port_cancel_notification(Context, Id) of
        _ ->
            lager:debug("port cancel notification sent"),
            post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the port cancel notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(
              'bad_gateway'
              ,<<"failed to send port cancel email">>
              ,Context
             )
    end;
post(Context, Id, ?PORT_CANCELED) ->
    _ = remove_from_phone_numbers_doc(Context),
    try send_port_cancel_notification(Context, Id) of
        _ ->
            lager:debug("port cancel notification sent"),
            do_post(Context, Id)
    catch
        _E:_R ->
            lager:debug("failed to send the port cancel notification: ~s:~p", [_E, _R]),
            cb_context:add_system_error(
              'bad_gateway'
              ,<<"failed to send port cancel email">>
              ,Context
             )
    end.

post(Context, Id, ?PORT_ATTACHMENT, AttachmentId) ->
    [{_Filename, FileJObj}] = cb_context:req_files(Context),
    Contents = wh_json:get_value(<<"contents">>, FileJObj),
    CT = wh_json:get_string_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'headers', [{'content_type', CT}]}],

    case wh_doc:attachment(cb_context:doc(Context), AttachmentId) of
        'undefined' -> lager:debug("no attachment named ~s", [AttachmentId]);
        _AttachmentMeta ->
            lager:debug("deleting old attachment ~s", [AttachmentId]),
            couch_mgr:delete_attachment(cb_context:account_db(Context), Id, AttachmentId)
    end,
    crossbar_doc:save_attachment(Id
                                 ,AttachmentId
                                 ,Contents
                                 ,Context
                                 ,Opts
                                ).

-spec do_post(cb_context:context(), path_token()) -> cb_context:context().
do_post(Context, Id) ->
    Context1 = crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = maybe_send_port_comment_notification(Context, Id),
            cb_context:set_resp_data(Context1, wh_port_request:public_fields(cb_context:doc(Context1)));
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
-spec read_descendants(cb_context:context(), ne_binaries()) -> cb_context:context().
read_descendants(Context) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS
                                      ,[{<<"startkey">>, [cb_context:account_id(Context)]}
                                        ,{<<"endkey">>, [cb_context:account_id(Context), wh_json:new()]}
                                       ]
                                      ,cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> read_descendants(Context1, cb_context:doc(Context1));
        _ -> Context1
    end.

read_descendants(Context, SubAccounts) ->
    AllPortRequests =
        lists:foldl(
          fun(Account, Acc) ->
                  read_descendants_fold(Account, Acc, Context)
          end
          ,[]
          ,SubAccounts
         ),
    crossbar_doc:handle_json_success(AllPortRequests, Context).

-spec read_descendants_fold(wh_json:object(), wh_json:objects(), cb_context:context()) ->
                                   wh_json:objects().
read_descendants_fold(Account, Acc, Context) ->
    AccountId = wh_doc:id(Account),
    case maybe_read_descendant(Context, AccountId) of
        'undefined' -> Acc;
        PortRequests ->
            [wh_json:from_list(
               [{<<"account_id">>, AccountId}
                ,{<<"account_name">>, wh_json:get_value([<<"value">>, <<"name">>], Account)}
                ,{<<"port_requests">>, PortRequests}
               ]
              )
             |Acc
            ]
    end.

-spec maybe_read_descendant(cb_context:context(), ne_binary()) -> api_object().
-spec maybe_read_descendant(cb_context:context(), ne_binary(), boolean()) -> api_object().
maybe_read_descendant(Context, AccountId) ->
    maybe_read_descendant(Context, AccountId, wh_services:is_reseller(AccountId)).

maybe_read_descendant(Context, AccountId, 'true') ->
    case authority(AccountId) of
        'undefined' -> read_descendant(Context, AccountId);
        AccountId ->
            lager:debug("~s is managed by itself", [AccountId]),
            'undefined';
        _OtherAccountId ->
            lager:warning("~s is managed by unknown: ~s", [AccountId, _OtherAccountId]),
            read_descendant(Context, AccountId)
    end;
maybe_read_descendant(Context, AccountId, 'false') ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    AuthAccountId = cb_context:auth_account_id(Context),
    case authority(ResellerId) of
        'undefined' -> read_descendant(Context, AccountId);
        AuthAccountId -> read_descendant(Context, AccountId);
        _OtherAccountId ->
            lager:debug("~s is managed by reseller: ~s", [AccountId, _OtherAccountId]),
            'undefined'
    end.

-spec authority(ne_binary()) -> api_binary().
authority(AccountId) ->
  case kz_whitelabel:fetch(AccountId) of
      {'error', _R} -> 'undefined';
      {'ok', JObj} ->
          kz_whitelabel:port_authority(JObj)
  end.

-spec read_descendant(cb_context:context(), ne_binary()) -> api_object().
read_descendant(Context, Id) ->
    Context1 = summary(cb_context:set_account_id(Context, Id)),
    case cb_context:resp_status(Context1) of
        'success' -> cb_context:doc(Context1);
        _Status -> 'undefined'
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
    ViewOptions = [{'startkey', [cb_context:account_id(Context)]}
                   ,{'endkey', [cb_context:account_id(Context), wh_json:new()]}
                   ,'include_docs'
                  ],

    crossbar_doc:load_view(<<"port_requests/crossbar_listing">>
                           ,ViewOptions
                           ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                           ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read_descendants_by_number(cb_context:context(), ne_binary()) ->
                                        cb_context:context().
read_descendants_by_number(Context, Number) ->
    ViewOptions = [{'keys', build_keys(Context, Number)}
                   ,'include_docs'
                  ],
    crossbar_doc:load_view(
      ?ALL_PORT_REQ_NUMBERS
      ,ViewOptions
      ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
      ,fun normalize_view_results/2
     ).

-type descendant_keys() :: [ne_binaries(),...] | [].

-spec build_keys(cb_context:context(), ne_binary()) ->
                        descendant_keys().
build_keys(Context, Number) ->
    build_keys_from_account(
      wnm_util:to_e164(Number)
      ,props:get_value(<<"accounts">>, cb_context:req_nouns(Context))
     ).

-spec build_keys_from_account(ne_binary(), ne_binaries()) ->
                                     descendant_keys().
build_keys_from_account(E164, [AccountId]) ->
    [[AccountId, E164]];
build_keys_from_account(E164, [AccountId, ?PORT_DESCENDANTS]) ->
    ViewOptions = [{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(
           ?WH_ACCOUNTS_DB
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
              ,[[AccountId, E164]]
              ,JObjs
             )
    end.

-spec build_descendant_key(wh_json:object(), descendant_keys(), ne_binary()) ->
                                  descendant_keys().
build_descendant_key(JObj, Acc, E164) ->
    [[wh_doc:id(JObj), E164]
     |Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(Res, Acc) ->
    [wh_port_request:public_fields(wh_json:get_value(<<"doc">>, Res)) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec summary_attachments(cb_context:context(), ne_binary()) -> cb_context:context().
summary_attachments(Context, Id) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),

    As = wh_doc:attachments(cb_context:doc(Context1), wh_json:new()),
    cb_context:set_resp_data(Context1
                             ,wh_port_request:normalize_attachments(As)
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
    Context1 = crossbar_doc:load_merge(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    on_successful_validation(Context1, Id, can_update_port_request(Context1)).

on_successful_validation(Context, Id, 'true') ->
    JObj = cb_context:doc(Context),
    Numbers = wh_json:get_keys(wh_json:get_value(<<"numbers">>, JObj)),

    Context1 = lists:foldl(fun(Number, ContextAcc) ->
                                   check_number_portability(Id, Number, ContextAcc)
                           end, Context, Numbers),

    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("number(s) checked out for ~s", [Id]),
            successful_validation(Context, Id);
        _ -> Context1
    end;
on_successful_validation(Context, _Id, 'false') ->
    PortState = wh_json:get_value(?PORT_PVT_STATE, cb_context:doc(Context)),
    lager:debug(
        "port state ~s is not valid for updating a port request"
        ,[PortState]
    ),
    cb_context:add_validation_error(
        PortState
        ,<<"type">>
        ,wh_json:from_list([
            {<<"message">>, <<"Updating port requests not allowed in current port state">>}
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
    can_update_port_request(Context, wh_port_request:current_state(cb_context:doc(Context))).

can_update_port_request(_Context, ?PORT_WAITING) ->
    'true';
can_update_port_request(_Context, ?PORT_REJECT) ->
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
    JObj = cb_context:doc(Context),
    cb_context:set_doc(Context, wh_json:set_values([{<<"pvt_type">>, <<"port_request">>}
                                                    ,{?PORT_PVT_STATE, ?PORT_WAITING}
                                                   ]
                                                   ,wh_port_request:normalize_numbers(JObj)
                                                  ));
successful_validation(Context, _Id) ->
    cb_context:set_doc(Context, wh_port_request:normalize_numbers(cb_context:doc(Context))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec check_number_portability(api_binary(), ne_binary(), cb_context:context()) ->
                                      cb_context:context().
-spec check_number_portability(api_binary(), ne_binary(), cb_context:context(), ne_binary(), wh_json:object()) ->
                                      cb_context:context().
check_number_portability(PortId, Number, Context) ->
    E164 = wnm_util:to_e164(Number),
    lager:debug("checking ~s(~s) for portability", [E164, Number]),
    PortOptions = [{'key', E164}],
    case couch_mgr:get_results(?KZ_PORT_REQUESTS_DB, ?PORT_REQ_NUMBERS, PortOptions) of
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
    case {wh_json:get_value(<<"value">>, PortReq) =:= cb_context:account_id(Context)
          ,wh_doc:id(PortReq) =:= PortId
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
                ,[E164, Number, cb_context:account_id(Context), wh_doc:id(PortReq)]
            ),
            Message = <<"Number is on a port request already: ", (wh_doc:id(PortReq))/binary>>,
            number_validation_error(Context, Number, Message);
        {'false', _} ->
            lager:debug(
                "number ~s(~s) is on existing port request for other account(~s)"
                ,[E164, Number, wh_json:get_value(<<"value">>, PortReq)]
            ),
            number_validation_error(Context, Number, <<"Number is being ported for a different account">>)
    end.

-spec number_validation_error(cb_context:context(), ne_binary(), ne_binary()) ->
                                     cb_context:context().
number_validation_error(Context, Number, Message) ->
    JObj = wh_json:from_list([{<<"message">>, Message}
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
    case wh_number_manager:lookup_account_by_number(E164) of
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
            number_validation_error(Context, Number, wh_util:to_binary(E))
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
                                   ,cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)
                                  )
      ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
        ,{<<"Content-Type">>, wh_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)}
        ,{<<"Content-Length">>, wh_doc:attachment_length(cb_context:doc(Context), AttachmentId)}
       ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_move_state(cb_context:context(), ne_binary(), ne_binary()) ->
                              cb_context:context().
maybe_move_state(Context, Id, PortState) ->
    Context1 = crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_PORT_REQUESTS_DB)),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso wh_port_request:maybe_transition(cb_context:doc(Context1), PortState)
    of
        'false' -> Context1;
        {'ok', PortRequest} ->
            lager:debug("loaded new port request state ~s", [PortState]),
            cb_context:set_doc(Context1, PortRequest);
        {'error', 'invalid_state_transition'} ->
            cb_context:add_validation_error(
              <<"port_state">>
              ,<<"enum">>
                  ,wh_json:from_list(
                     [{<<"message">>, <<"Cannot move to new state from current state">>}
                      ,{<<"cause">>, PortState}
                     ])
              ,Context
             )
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
-spec generate_loa_from_port(cb_context:context(), wh_json:object()) ->
                                    cb_context:context().
-spec generate_loa_from_port(cb_context:context(), wh_json:object(), ne_binary()) ->
                                    cb_context:context().
generate_loa_from_port(Context, PortRequest) ->
    generate_loa_from_port(Context, PortRequest, ?LOA_BUILDER).

generate_loa_from_port(Context, PortRequest, <<"htmldoc">>) ->
    cb_loa_htmldoc:generate_loa(Context, PortRequest).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_template(ne_binary()) -> ne_binary().
-spec find_template(ne_binary(), api_binary()) -> ne_binary().
find_template(ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    case couch_mgr:fetch_attachment(ResellerDb, ?TEMPLATE_DOC_ID, ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', Template} -> Template;
        {'error', _} -> default_template()
    end.

find_template(ResellerId, 'undefined') ->
    find_template(ResellerId);
find_template(ResellerId, CarrierName) ->
    CarrierTemplate = list_to_binary([?TEMPLATE_DOC_ID
                                      ,<<".">>
                                      ,wh_util:to_lower_binary(wh_util:uri_encode(CarrierName))
                                     ]),
    lager:debug("looking for carrier template ~s or plain template for reseller ~s", [CarrierTemplate, ResellerId]),
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    case couch_mgr:fetch_attachment(ResellerDb, CarrierTemplate, ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', Template} -> Template;
        {'error', _} -> find_carrier_template(ResellerDb, CarrierTemplate)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_carrier_template(ne_binary(), ne_binary()) -> ne_binary().
find_carrier_template(ResellerDb, CarrierTemplate) ->
    case couch_mgr:fetch_attachment(ResellerDb, ?TEMPLATE_DOC_ID, ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', Template} -> Template;
        {'error', _} -> default_carrier_template(CarrierTemplate)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_template() -> ne_binary().
default_template() ->
    case couch_mgr:fetch_attachment(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID, ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', Template} -> Template;
        {'error', _} -> create_default_template()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_carrier_template(ne_binary()) -> ne_binary().
default_carrier_template(CarrierTemplate) ->
    case couch_mgr:fetch_attachment(?WH_CONFIG_DB, CarrierTemplate, ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', Template} -> Template;
        {'error', _} -> default_template()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_default_template() -> ne_binary().
create_default_template() ->
    {'ok', _Doc} =
        couch_mgr:save_doc(?WH_CONFIG_DB
                           ,wh_json:from_list([{<<"template_name">>, <<"loa">>}
                                               ,{<<"_id">>, ?TEMPLATE_DOC_ID}
                                              ])
                          ),
    save_default_template().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_default_template() -> ne_binary().
update_default_template() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID) of
        {'ok', _Doc} -> save_default_template();
        {'error', 'not_found'} -> create_default_template()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_default_template() -> ne_binary().
save_default_template() ->
    PrivDir = code:priv_dir('crossbar'),
    TemplateFile = filename:join([PrivDir, <<"couchdb">>, <<"templates">>, <<"loa.tmpl">>]),
    lager:debug("loading template from ~s", [TemplateFile]),

    {'ok', Template} = file:read_file(TemplateFile),

    {'ok', _} =
        couch_mgr:put_attachment(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID, ?TEMPLATE_ATTACHMENT_ID, Template),
    Template.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_port_comment_notification(cb_context:context(), ne_binary()) -> 'ok'.
maybe_send_port_comment_notification(Context, Id) ->
    DbDoc = cb_context:fetch(Context, 'db_doc'),
    ReqData = cb_context:req_data(Context),
    DbDocComments = wh_json:get_value(<<"comments">>, DbDoc),
    ReqDataComments = wh_json:get_value(<<"comments">>, ReqData),
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
    OldTime = wh_json:get_value(<<"timestamp">>, lists:last(OldComments)),
    NewTime = wh_json:get_value(<<"timestamp">>, lists:last(NewComments)),

    OldTime < NewTime.

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_comment/1).

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_request/1).

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_pending/1).

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_cancel/1).

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_ported/1).

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
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_send(Req, fun wapi_notifications:publish_port_scheduled/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_to_phone_numbers_doc(cb_context:context()) -> 'ok' | 'error'.
-spec add_to_phone_numbers_doc(cb_context:context(), wh_json:object()) -> 'ok' | 'error'.
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
        wh_json:foldl(
          fun(Number, _, Acc) ->
                  NumberJObj = build_number_properties(AccountId, Now),
                  wh_json:set_value(Number, NumberJObj, Acc)
          end
          ,JObj
          ,wh_json:get_value(<<"numbers">>, cb_context:doc(Context), wh_json:new())
         ),
    save_phone_numbers_doc(Context, PhoneNumbersJObj).

-spec build_number_properties(ne_binary(), gregorian_seconds()) -> wh_json:object().
build_number_properties(AccountId, Now) ->
    wh_json:from_list(
      [{<<"state">>, ?NUMBER_STATE_PORT_IN}
       ,{<<"features">>, []}
       ,{<<"assigned_to">>, AccountId}
       ,{<<"used_by">>, <<>>}
       ,{<<"created">>, Now}
       ,{<<"updated">>, Now}
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_from_phone_numbers_doc(cb_context:context()) -> 'ok' | 'error'.
-spec remove_from_phone_numbers_doc(cb_context:context(), wh_json:object()) -> 'ok' | 'error'.
remove_from_phone_numbers_doc(Context) ->
    case get_phone_numbers_doc(Context) of
        {'error', _R}-> 'ok';
        {'ok', JObj} ->
            remove_from_phone_numbers_doc(Context, JObj)
    end.

remove_from_phone_numbers_doc(Context, JObj) ->
    {Updated, PhoneNumbersJObj} =
        wh_json:foldl(fun remove_phone_number/3
                      ,{'false', JObj}
                      ,wh_json:get_value(<<"numbers">>, cb_context:doc(Context), wh_json:new())
                     ),
    case Updated of
        'true' ->
            save_phone_numbers_doc(Context, PhoneNumbersJObj);
        'false' ->
            lager:debug("no numbers removed, not updating")
    end.

-spec remove_phone_number(wh_json:key(), wh_json:json_term(), {boolean(), wh_json:object()}) ->
                                 {'true', wh_json:object()}.
remove_phone_number(Number, _, {_, Acc}) ->
    {'true', wh_json:delete_key(Number, Acc)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_phone_numbers_doc(cb_context:context()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', _}.
get_phone_numbers_doc(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Context1 = crossbar_doc:load(?WNM_PHONE_NUMBER_DOC, cb_context:set_account_db(Context, AccountDb)),
    case cb_context:resp_status(Context1) of
        'success' ->
            {'ok', cb_context:doc(Context1)};
        Status ->
            lager:error("failed to open phone_numbers doc in ~s : ~p", [AccountId, Status]),
            {'error', Status}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_phone_numbers_doc(cb_context:context(), wh_json:object()) -> 'ok' | 'error'.
save_phone_numbers_doc(Context, JObj) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    Context1 =
        cb_context:setters(
          Context
          ,[{fun cb_context:set_doc/2, JObj}
            ,{fun cb_context:set_account_db/2, AccountDb}
           ]
         ),
    Context2 = crossbar_doc:save(Context1),

    case cb_context:resp_status(Context2) of
        'success' -> 'ok';
        _Status ->
            lager:error("failed to save phone_numbers doc in ~s : ~p", [AccountId, _Status]),
            'error'
    end.
