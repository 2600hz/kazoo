%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% Handle CRUD operations for WebHooks
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_webhooks).

-export([init/0
        ,authorize/1
        ,authenticate/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2
        ,patch/1, patch/2
        ,delete/2, delete_account/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(CB_LIST, <<"webhooks/crossbar_listing">>).

-define(PATH_TOKEN_ATTEMPTS, <<"attempts">>).

-define(ATTEMPTS_BY_ACCOUNT, <<"webhooks/attempts_by_time_listing">>).
-define(ATTEMPTS_BY_HOOK, <<"webhooks/attempts_by_hook_listing">>).
-define(AVAILABLE_HOOKS, <<"webhooks/webhook_meta_listing">>).

-define(DESCENDANTS, <<"descendants">>).
-define(REENABLE, <<"re-enable">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = kz_datamgr:db_create(?KZ_WEBHOOKS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, ?APP, <<"views/webhooks.json">>),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, ?APP, <<"schemas/webhooks.json">>),
    init_master_account_db(),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.webhooks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.webhooks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.webhooks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.webhooks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.webhooks">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.webhooks">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.webhooks">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.accounts">>, ?MODULE, 'delete_account'),
    'ok'.

-spec init_master_account_db() -> 'ok'.
init_master_account_db() ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            _ = kz_datamgr:revise_doc_from_file(MasterAccountDb, 'webhooks', <<"webhooks.json">>),
            lager:debug("ensured view into master db"),
            maybe_revise_schema(MasterAccountDb);
        {'error', _E} ->
            lager:warning("master account not set yet, unable to load view and revise schema: ~p", [_E])
    end.

-spec maybe_revise_schema(ne_binary()) -> 'ok'.
maybe_revise_schema(MasterAccountDb) ->
    case kz_json_schema:load(<<"webhooks">>) of
        {'ok', SchemaJObj} -> maybe_revise_schema(MasterAccountDb, SchemaJObj);
        {'error', _E} ->
            lager:warning("failed to find webhooks schema: ~p", [_E])
    end.

-spec maybe_revise_schema(ne_binary(), kz_json:object()) -> 'ok'.
maybe_revise_schema(MasterDb, SchemaJObj) ->
    case kz_datamgr:get_results(MasterDb, ?AVAILABLE_HOOKS) of
        {'ok', []} ->
            lager:warning("no hooks are registered; have you started the webhooks app?");
        {'error', _E} ->
            lager:warning("failed to find registered webhooks: ~p", [_E]);
        {'ok', Hooks} ->
            revise_schema(SchemaJObj, [kz_json:get_value(<<"key">>, Hook) || Hook <- Hooks])
    end.

-spec revise_schema(kz_json:object(), ne_binaries()) -> 'ok'.
revise_schema(SchemaJObj, HNs) ->
    HookNames = [<<"all">> | lists:delete(<<"skel">>, HNs)],
    Updated = kz_json:set_value([<<"properties">>, <<"hook">>, <<"enum">>], HookNames, SchemaJObj),
    case kz_datamgr:save_doc(?KZ_SCHEMA_DB, Updated) of
        {'ok', _} -> lager:info("added hooks enum to schema: ~p", [HookNames]);
        {'error', _E} -> lager:warning("failed to add hooks enum to schema: ~p", [_E])
    end.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(http_method(), req_nouns()) -> boolean().
authorize(?HTTP_GET, [{<<"webhooks">>, []}]) ->
    lager:debug("authorizing request"),
    'true';
authorize(_Verb, _Nouns) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), http_method(), req_nouns()) ->
                          {'true', cb_context:context()} |
                          'false'.
authenticate(Context, ?HTTP_GET, [{<<"webhooks">>, []}]) ->
    lager:debug("authenticating request"),
    {'true', Context};
authenticate(_Context, _Verb, _Nouns) -> 'false'.

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

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_PATCH].

allowed_methods(?PATH_TOKEN_ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(_WebhookId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

allowed_methods(_WebhookId, ?PATH_TOKEN_ATTEMPTS) ->
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
resource_exists() -> 'true'.
resource_exists(_WebhookId) -> 'true'.
resource_exists(_WebhookId, ?PATH_TOKEN_ATTEMPTS) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_webhooks(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB), cb_context:req_verb(Context)).

-spec validate_webhooks(cb_context:context(), http_method()) -> cb_context:context().
validate_webhooks(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        [{<<"webhooks">>, []}] -> summary_available(Context);
        _Nouns -> summary(Context)
    end;
validate_webhooks(Context, ?HTTP_PUT) ->
    create(Context);
validate_webhooks(Context, ?HTTP_PATCH) ->
    validate_collection_patch(Context).

validate(Context, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, 'undefined');
validate(Context, Id) ->
    validate_webhook(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB), Id, cb_context:req_verb(Context)).

-spec validate_webhook(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_webhook(Context, WebhookId, ?HTTP_GET) ->
    read(WebhookId, Context);
validate_webhook(Context, WebhookId, ?HTTP_POST) ->
    update(WebhookId, Context);
validate_webhook(Context, WebhookId, ?HTTP_PATCH) ->
    validate_patch(read(WebhookId, Context), WebhookId);
validate_webhook(Context, WebhookId, ?HTTP_DELETE) ->
    read(WebhookId, Context).

validate(Context, WebhookId=?NE_BINARY, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, WebhookId).

-spec validate_patch(cb_context:context(), ne_binary()) -> cb_context:context().
validate_patch(Context, WebhookId) ->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = kz_doc:public_fields(cb_context:req_data(Context)),
            JObj = kz_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnSuccess = fun(C) -> crossbar_doc:load_merge(WebhookId, C, ?TYPE_CHECK_OPTION(kzd_webhook:type())) end,
            cb_context:validate_request_data(<<"webhooks">>, cb_context:set_req_data(Context, JObj), OnSuccess);
        _Status -> Context
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Context1 = maybe_update_hook(Context),
    crossbar_doc:save(cb_context:set_account_db(Context1, ?KZ_WEBHOOKS_DB)).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

-spec patch(cb_context:context()) -> cb_context:context().
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context) ->
    reenable_hooks(Context).

patch(Context, WebhookId) ->
    post(Context, WebhookId).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

-spec delete_account(cb_context:context(), ne_binary()) -> cb_context:context().
delete_account(Context, AccountId) ->
    lager:debug("account ~s deleted, removing any webhooks", [AccountId]),
    kz_util:spawn(fun delete_account_webhooks/1, [AccountId]),
    Context.

-spec delete_account_webhooks(ne_binary()) -> 'ok'.
delete_account_webhooks(AccountId) ->
    case fetch_account_hooks(AccountId) of
        {'ok', []} -> 'ok';
        {'error', _E} ->
            lager:debug("failed to fetch webhooks for account ~s: ~p", [AccountId, _E]);
        {'ok', ViewJObjs} ->
            _ = delete_account_hooks(ViewJObjs),
            lager:debug("deleted ~p hooks from account ~s", [length(ViewJObjs), AccountId])
    end.

-spec fetch_account_hooks(ne_binary()) -> kazoo_data:get_results_return().
fetch_account_hooks(AccountId) ->
    ViewOptions = [{'key', AccountId}, {'reduce', 'false'}],
    kz_datamgr:get_results(?KZ_WEBHOOKS_DB, <<"webhooks/accounts_listing">>, ViewOptions).

-spec delete_account_hooks(kz_json:objects()) -> any().
delete_account_hooks(JObjs) ->
    kz_datamgr:del_docs(?KZ_WEBHOOKS_DB, [kz_doc:id(J) || J <- JObjs]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

-spec validate_collection_patch(cb_context:context()) -> cb_context:context().
-spec validate_collection_patch(cb_context:context(), api_boolean()) ->
                                       cb_context:context().
validate_collection_patch(Context) ->
    validate_collection_patch(Context, cb_context:req_value(Context, ?REENABLE)).
validate_collection_patch(Context, 'undefined') ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"re-enable is required to patch collections">>}
            ]),
    cb_context:add_validation_error(?REENABLE, <<"required">>, Msg, Context);
validate_collection_patch(Context, ReEnable) ->
    case kz_term:is_true(ReEnable) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' -> reenable_validation_error(Context)
    end.

-spec reenable_validation_error(cb_context:context()) -> cb_context:context().
reenable_validation_error(Context) ->
    Msg = kz_json:from_list(
            [{<<"message">>, <<"value not found in enumerated list of values">>}
            ,{<<"target">>, ['true']}
            ]),
    cb_context:add_validation_error(?REENABLE, <<"enum">>, Msg, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_webhook:type())),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_leak_pvt_fields(Context1);
        _Status -> Context1
    end.

-spec maybe_leak_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_leak_pvt_fields(Context) ->
    Doc = cb_context:doc(Context),
    NewDoc = kz_json:set_value(<<"disable_reason">>, kzd_webhook:disabled_message(Doc), Doc),
    cb_context:set_doc(Context, NewDoc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Options = [{'startkey', [cb_context:account_id(Context)]}
              ,{'endkey', [cb_context:account_id(Context), kz_json:new()]}
              ,{'mapper', crossbar_view:map_value_fun()}
              ],
    crossbar_view:load(Context, ?CB_LIST, Options).

-spec summary_available(cb_context:context()) ->
                               cb_context:context().
summary_available(Context) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    Options = [{'mapper', fun normalize_available/2}
              ,'include_docs'
              ],
    crossbar_view:load(cb_context:set_account_db(Context, MasterAccountDb), ?AVAILABLE_HOOKS, Options).

-spec normalize_available(kz_json:object(), kz_json:objects()) ->
                                 kz_json:objects().
normalize_available(JObj, Acc) ->
    case kz_json:get_value(<<"key">>, JObj) of
        <<"skel">> -> Acc;
        Name -> [kz_doc:set_id(kz_json:get_value(<<"doc">>, JObj), Name) | Acc]
    end.

-spec summary_attempts(cb_context:context(), api_ne_binary()) -> cb_context:context().
summary_attempts(Context, HookId) ->
    ViewName = get_view_name(HookId),
    Options = [{'mapper', fun normalize_attempt_results/2}
              ,{'range_keymap', HookId}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec get_view_name(api_ne_binary()) -> ne_binary().
get_view_name('undefined') -> ?ATTEMPTS_BY_ACCOUNT;
get_view_name(_) -> ?ATTEMPTS_BY_HOOK.

-spec normalize_attempt_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_attempt_results(JObj, Acc) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    NewDoc = kz_json:set_value(<<"timestamp">>, kz_doc:created(Doc), Doc),
    [kz_json:delete_keys([<<"id">>, <<"_id">>], NewDoc)
     | Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) ->
                                      cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_webhook:type()}
            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
            ],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_webhook:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a hook was auto-disabled and is being re-enabled, cleanup the private
%% fields related to the auto-disabling
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_hook(cb_context:context()) -> cb_context:context().
maybe_update_hook(Context) ->
    Doc = cb_context:doc(Context),
    case kzd_webhook:is_enabled(Doc) of
        'false' -> Context;
        'true' -> cb_context:set_doc(Context, kzd_webhook:enable(Doc))
    end.

-spec reenable_hooks(cb_context:context()) ->
                            cb_context:context().
-spec reenable_hooks(cb_context:context(), ne_binaries()) ->
                            cb_context:context().
reenable_hooks(Context) ->
    reenable_hooks(Context, props:get_value(<<"accounts">>, cb_context:req_nouns(Context))).

reenable_hooks(Context, [AccountId]) ->
    handle_resp(Context, send_reenable_req(Context, AccountId, <<"account">>));
reenable_hooks(Context, [AccountId, ?DESCENDANTS]) ->
    handle_resp(Context, send_reenable_req(Context, AccountId, ?DESCENDANTS)).

-spec send_reenable_req(cb_context:context(), ne_binary(), ne_binary()) ->
                               kz_amqp_worker:request_return().
send_reenable_req(Context, AccountId, Action) ->
    Req = [{<<"Type">>, kzd_webhook:type()}
          ,{<<"Action">>, Action}
          ,{<<"Account-ID">>, AccountId}
          ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:call(Req
                       ,fun kapi_conf:publish_doc_type_update/1
                       ,fun kz_term:always_true/1
                       ).

-spec handle_resp(cb_context:context(), kz_amqp_worker:request_return()) ->
                         cb_context:context().
handle_resp(Context, {'ok', _Resp}) ->
    lager:debug("received resp from update"),
    crossbar_util:response(<<"hooks updated">>, Context);
handle_resp(Context, {'returned', _Request, BasicReturn}) ->
    lager:debug("no webhook apps running: ~p", [BasicReturn]),
    Resp = kz_json:delete_keys([<<"exchange">>, <<"routing_key">>], BasicReturn),
    crossbar_util:response('error', <<"The backend is not configured for this request">>, 500, Resp, Context);
handle_resp(Context, {'timeout', _Timeout}) ->
    lager:debug("timed out waiting for a response: ~p", [_Timeout]),
    crossbar_util:response_datastore_timeout(Context);
handle_resp(Context, {'error', 'timeout'}) ->
    lager:debug("timed out waiting for a response"),
    crossbar_util:response_datastore_timeout(Context);
handle_resp(Context, {'error', _E}) ->
    lager:debug("error with request: ~p", [_E]),
    crossbar_util:response('error', <<"Request failed on the backend">>, 500, Context).
