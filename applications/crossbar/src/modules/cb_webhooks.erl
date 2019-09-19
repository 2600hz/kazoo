%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle CRUD operations for Webhooks
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_webhooks).

-export([init/0
        ,authorize/1, authorize/2, authorize/3
        ,authenticate/1, authenticate/2, authenticate/3
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
-define(PATH_TOKEN_SAMPLES, <<"samples">>).

-define(ATTEMPTS_BY_ACCOUNT, <<"webhooks/attempts_by_time_listing">>).
-define(ATTEMPTS_BY_HOOK, <<"webhooks/attempts_by_hook_listing">>).
-define(AVAILABLE_HOOKS, <<"webhooks/webhook_meta_listing">>).

-define(DESCENDANTS, <<"descendants">>).
-define(REENABLE, <<"re-enable">>).

-define(NOTIFY_SUPER_ADMIN_ONLY, [<<"system_alert">>
                                 ,<<"webhook_disabled">>
                                 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kz_datamgr:db_create(?KZ_WEBHOOKS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, ?APP, <<"schemas/webhooks.json">>),
    init_master_account_db(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.webhooks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize.webhooks">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.authenticate.webhooks">>, ?MODULE, 'authenticate'),
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
    maybe_revise_schema(get_available_hook_ids()).

maybe_revise_schema({'error', _}) ->
    'ok';
maybe_revise_schema({'ok', []}) ->
    'ok';
maybe_revise_schema({'ok', HookIds}) ->
    case kz_json_schema:load(<<"webhooks">>) of
        {'ok', SchemaJObj} -> maybe_revise_schema(HookIds, SchemaJObj);
        {'error', _E} ->
            lager:warning("failed to find webhooks schema: ~p", [_E])
    end.

-spec maybe_revise_schema(kz_term:ne_binaries(), kz_json:object()) -> 'ok'.
maybe_revise_schema(HookIds, SchemaJObj) ->
    HookNames = lists:usort([<<"all">> | HookIds]),
    Updated = kz_json:set_value([<<"properties">>, <<"hook">>, <<"enum">>], HookNames, SchemaJObj),

    case kz_json:are_equal(kz_doc:public_fields(SchemaJObj), kz_doc:public_fields(Updated)) of
        'true' -> 'ok';
        'false' ->
            case kz_datamgr:save_doc(?KZ_SCHEMA_DB, Updated) of
                {'ok', _} -> lager:info("added hooks enum to schema: ~p", [HookNames]);
                {'error', _E} -> lager:warning("failed to add hooks enum to schema: ~p", [_E])
            end
    end.

-spec get_available_hook_ids() -> {'ok', kz_term:ne_binaries()} | kazoo_data:data_error().
get_available_hook_ids() ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            get_available_hook_ids(MasterAccountDb);
        {'error', _E}=Error ->
            lager:warning("master account not set yet: ~p", [_E]),
            Error
    end.

-spec get_available_hook_ids(kz_term:ne_binary()) -> {'ok', kz_term:ne_binaries()} | kazoo_data:data_error().
get_available_hook_ids(MasterDb) ->
    case kz_datamgr:get_results(MasterDb, ?AVAILABLE_HOOKS) of
        {'ok', []}=OK ->
            lager:warning("no hooks are registered; have you started the webhooks app?"),
            OK;
        {'error', _E}=Error ->
            lager:warning("failed to find registered webhooks: ~p", [_E]),
            Error;
        {'ok', Hooks} ->
            ToRemoveHooks = [<<"callflow">>
                            ,<<"inbound_fax">>
                            ,<<"outbound_fax">>
                            ,<<"skel">>
                            ],
            {'ok'
            ,[Id
              || <<"webhooks_", Id/binary>> <- [kz_doc:id(Hook) || Hook <- Hooks],
                 not lists:member(Id, ToRemoveHooks)
             ]
            }
    end.

-spec authorize(cb_context:context()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context) ->
    authorize_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context, _) ->
    authorize_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize(Context, _, _) ->
    authorize_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authorize_webhook(cb_context:context(), http_method(), req_nouns()) ->
          boolean() |
          {'stop', cb_context:context()}.
authorize_webhook(_, ?HTTP_GET, [{<<"webhooks">>, []}]) ->
    lager:debug("authorizing request"),
    'true';
authorize_webhook(_, ?HTTP_GET, [{<<"webhooks">>, [?PATH_TOKEN_SAMPLES | _]}]) ->
    lager:debug("authorizing fetching webhook samples"),
    'true';
authorize_webhook(Context, _, [{<<"webhooks">>, _}]) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize_webhook(_, _Verb, _Nouns) ->
    'false'.

-spec authenticate(cb_context:context()) ->
          {'true', cb_context:context()} |
          'false'.
authenticate(Context) ->
    authenticate_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token()) ->
          {'true', cb_context:context()} |
          'false'.
authenticate(Context, _) ->
    authenticate_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), path_token(), path_token()) ->
          {'true', cb_context:context()} |
          'false'.
authenticate(Context, _, _) ->
    authenticate_webhook(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate_webhook(cb_context:context(), http_method(), req_nouns()) ->
          {'true', cb_context:context()} |
          'false'.
authenticate_webhook(Context, ?HTTP_GET, [{<<"webhooks">>, []}]) ->
    lager:debug("authenticating request"),
    {'true', Context};
authenticate_webhook(Context, ?HTTP_GET, [{<<"webhooks">>, [?PATH_TOKEN_SAMPLES | _]}]) ->
    lager:debug("authenticating request for fetching webhook samples"),
    {'true', Context}.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_PATCH].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PATH_TOKEN_ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(?PATH_TOKEN_SAMPLES) ->
    [?HTTP_GET];
allowed_methods(_WebhookId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?PATH_TOKEN_SAMPLES, _SampleId) ->
    [?HTTP_GET];
allowed_methods(_WebhookId, ?PATH_TOKEN_ATTEMPTS) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_WebhookId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?PATH_TOKEN_SAMPLES, _SampleId) -> 'true';
resource_exists(_WebhookId, ?PATH_TOKEN_ATTEMPTS) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_webhooks(cb_context:set_db_name(Context, ?KZ_WEBHOOKS_DB), cb_context:req_verb(Context)).

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

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, 'undefined');
validate(Context, ?PATH_TOKEN_SAMPLES) ->
    case get_available_hook_samples() of
        [] -> cb_context:add_system_error('datastore_fault', Context);
        WebHooks ->
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_resp_data/2, WebHooks}
                      ],
            cb_context:setters(Context, Setters)
    end;
validate(Context, Id) ->
    validate_webhook(cb_context:set_db_name(Context, ?KZ_WEBHOOKS_DB), Id, cb_context:req_verb(Context)).

-spec validate_webhook(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_webhook(Context, WebhookId, ?HTTP_GET) ->
    read(WebhookId, Context);
validate_webhook(Context, WebhookId, ?HTTP_POST) ->
    update(WebhookId, Context);
validate_webhook(Context, WebhookId, ?HTTP_PATCH) ->
    validate_patch(read(WebhookId, Context), WebhookId);
validate_webhook(Context, WebhookId, ?HTTP_DELETE) ->
    read(WebhookId, Context).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?PATH_TOKEN_SAMPLES, SampleId) ->
    fetch_webhook_samples(Context, SampleId);
validate(Context, WebhookId=?NE_BINARY, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, WebhookId).

-spec validate_patch(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_patch(Context, WebhookId) ->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = kz_doc:public_fields(cb_context:req_data(Context)),
            JObj = kz_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnSuccess = fun(C) -> maybe_check_modifiers(crossbar_doc:load_merge(WebhookId, C, ?TYPE_CHECK_OPTION(kzd_webhooks:type()))) end,
            cb_context:validate_request_data(<<"webhooks">>, cb_context:set_req_data(Context, JObj), OnSuccess);
        _Status -> Context
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Context1 = maybe_update_hook(Context),
    crossbar_doc:save(cb_context:set_db_name(Context1, ?KZ_WEBHOOKS_DB)).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(cb_context:set_db_name(Context, ?KZ_WEBHOOKS_DB)).

-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    reenable_hooks(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, WebhookId) ->
    post(Context, WebhookId).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(cb_context:set_db_name(Context, ?KZ_WEBHOOKS_DB)).

-spec delete_account(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
delete_account(Context, AccountId) ->
    _P = kz_process:spawn(fun delete_account_webhooks/1, [AccountId]),
    lager:debug("account ~s deleted, removing any webhooks in ~p", [AccountId, _P]),
    cb_context:set_resp_status(Context, 'success').

-spec delete_account_webhooks(kz_term:ne_binary()) -> 'ok'.
delete_account_webhooks(AccountId) ->
    case fetch_account_hooks(AccountId) of
        {'ok', []} -> 'ok';
        {'error', _E} ->
            lager:debug("failed to fetch webhooks for account ~s: ~p", [AccountId, _E]);
        {'ok', ViewJObjs} ->
            _ = delete_account_hooks(ViewJObjs),
            lager:debug("deleted ~p hooks from account ~s", [length(ViewJObjs), AccountId])
    end.

-spec fetch_account_hooks(kz_term:ne_binary()) -> kazoo_data:get_results_return().
fetch_account_hooks(AccountId) ->
    ViewOptions = [{'key', AccountId}, {'reduce', 'false'}],
    kz_datamgr:get_results(?KZ_WEBHOOKS_DB, <<"webhooks/accounts_listing">>, ViewOptions).

-spec delete_account_hooks(kz_json:objects()) -> any().
delete_account_hooks(JObjs) ->
    kz_datamgr:del_docs(?KZ_WEBHOOKS_DB, [kz_doc:id(J) || J <- JObjs]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

-spec validate_collection_patch(cb_context:context()) -> cb_context:context().
validate_collection_patch(Context) ->
    validate_collection_patch(Context, cb_context:req_value(Context, ?REENABLE)).

-spec validate_collection_patch(cb_context:context(), kz_term:api_boolean()) ->
          cb_context:context().
validate_collection_patch(Context, 'undefined') ->
    Msg = kz_json:from_list([{<<"message">>, <<"re-enable is required to patch collections">>}]),
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

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_webhooks:type())),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_leak_pvt_fields(Context1);
        _Status -> Context1
    end.

-spec maybe_leak_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_leak_pvt_fields(Context) ->
    Doc = cb_context:doc(Context),
    NewDoc = kz_json:set_value(<<"disable_reason">>, kzd_webhooks:disabled_message(Doc), Doc),
    cb_context:set_doc(Context, NewDoc).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
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
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    C1 = cb_context:store(Context, 'is_superduper_admin', IsSuperAdmin),
    Options = [{'mapper', fun normalize_available/3}
              ,'include_docs'
              ],
    crossbar_view:load(cb_context:set_db_name(C1, MasterAccountDb), ?AVAILABLE_HOOKS, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_webhook_samples(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
fetch_webhook_samples(Context, WebhooksName) ->
    Path = filename:join(code:priv_dir(?APP), <<WebhooksName/binary, "-samples.json">>),
    case file:read_file(Path) of
        {'ok', Bin} ->
            HasFilter = crossbar_filter:is_defined(Context),
            Resp = [JObj || JObj <- kz_json:decode(Bin),
                            crossbar_filter:by_doc(JObj, Context, HasFilter)
                   ],
            crossbar_doc:handle_json_success(Resp, Context);
        {'error', 'enoent'} ->
            crossbar_util:response_bad_identifier(WebhooksName, Context);
        {'error', _Reason} ->
            lager:debug("failed to read file ~s: ~p", [Path, _Reason]),
            cb_context:add_system_error('datastore_fault', Context)
    end.

-spec get_available_hook_samples() -> kz_term:ne_binaries().
get_available_hook_samples() ->
    [hd(binary:split(filename:basename(kz_term:to_binary(Path), ".json"), <<"-samples">>))
     || Path <- filelib:wildcard(filename:join(code:priv_dir('crossbar'), "webhooks_*-samples.json"))
    ].

-spec normalize_available(cb_context:context(), kz_json:object(), kz_json:objects()) ->
          kz_json:objects().
normalize_available(Context, JObj, Acc) ->
    maybe_filter_non_admin_hooks(Context, kz_doc:id(JObj), kz_json:get_value(<<"doc">>, JObj), Acc).

-spec maybe_filter_non_admin_hooks(cb_context:context(), kz_term:ne_binary(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
maybe_filter_non_admin_hooks(_, <<"webhooks_skel">>, _, Acc) -> Acc;
maybe_filter_non_admin_hooks(Context, <<"webhooks_notifications">>, JObj, Acc) ->
    [kz_doc:set_id(maybe_filter_non_admin_notifications(Context, JObj), <<"notifications">>) | Acc];
maybe_filter_non_admin_hooks(_, <<"webhooks_", Id/binary>>, JObj, Acc) ->
    [kz_doc:set_id(JObj, Id) | Acc];
maybe_filter_non_admin_hooks(_, Id, JObj, Acc) ->
    [kz_doc:set_id(JObj, Id) | Acc].

-spec maybe_filter_non_admin_notifications(cb_context:context(), kz_json:object()) -> kz_json:object().
maybe_filter_non_admin_notifications(Context, JObj) ->
    IsSuperAdmin = cb_context:fetch(Context, 'is_superduper_admin'),
    NotifyTypes = kz_json:get_value([<<"modifiers">>, <<"type">>, <<"items">>], JObj),
    Filtered = kz_json:filter(fun(KV) -> filter_non_admin_notifications(KV, IsSuperAdmin) end, NotifyTypes),
    kz_json:set_value([<<"modifiers">>, <<"type">>, <<"items">>], Filtered, JObj).

-spec filter_non_admin_notifications({kz_json:key(), kz_json:json_term()}, boolean()) -> boolean().
filter_non_admin_notifications({_, _}, 'true') ->
    'true';
filter_non_admin_notifications({Name, _}, 'false') ->
    not lists:member(Name, ?NOTIFY_SUPER_ADMIN_ONLY).

-spec summary_attempts(cb_context:context(), kz_term:api_ne_binary()) -> cb_context:context().
summary_attempts(Context, HookId) ->
    ViewName = get_view_name(HookId),
    Options = [{'mapper', fun normalize_attempt_results/2}
              ,{'range_keymap', HookId}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec get_view_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_view_name('undefined') -> ?ATTEMPTS_BY_ACCOUNT;
get_view_name(_) -> ?ATTEMPTS_BY_HOOK.

-spec normalize_attempt_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_attempt_results(JObj, Acc) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    NewDoc = kz_json:set_value(<<"timestamp">>, kz_doc:created(Doc), Doc),
    [kz_json:delete_keys([<<"id">>, <<"_id">>], NewDoc)
     | Acc
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) ->
          cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_webhooks:type()}
            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
            ],
    maybe_check_modifiers(cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context))));
on_successful_validation(Id, Context) ->
    maybe_check_modifiers(crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_webhooks:type()))).

-spec maybe_check_modifiers(cb_context:context()) -> cb_context:context().
maybe_check_modifiers(Context) ->
    WebhookDoc = cb_context:doc(Context),
    lager:info("checking modifiers for ~s", [kzd_webhooks:hook(WebhookDoc)]),
    maybe_check_modifiers(Context, WebhookDoc, kzd_webhooks:hook(WebhookDoc)).

-spec maybe_check_modifiers(cb_context:context(), kzd_webhooks:doc(), kz_term:api_ne_binary()) -> cb_context:context().
maybe_check_modifiers(Context, _WebhookdDoc, <<"all">>) -> Context;
maybe_check_modifiers(Context, WebhookDoc, HookEvent) ->
    case get_hook_definition(HookEvent) of
        'undefined' ->
            cb_context:add_system_error('datastore_fault', Context);
        HookDefinition ->
            check_modifiers(Context, WebhookDoc, kzd_webhooks:modifiers(HookDefinition))
    end.

-spec check_modifiers(cb_context:context(), kzd_webhooks:doc(), kz_term:api_object()) -> cb_context:context().
check_modifiers(Context, _WebhookDoc_, 'undefined') ->
    Context;
check_modifiers(Context, WebhookDoc, Modifiers) ->
    kz_json:foldl(fun(K, V, Acc) -> check_modifiers(WebhookDoc, K, V, Acc) end, Context, Modifiers).

-spec check_modifiers(kzd_webhooks:doc(), kz_term:ne_binary(), kz_json:object(), cb_context:context()) -> cb_context:context().
check_modifiers(WebhookDoc, ModifierKey, ModifierValue, Context) ->
    case kz_json:get_value([ModifierKey], kzd_webhooks:custom_data(WebhookDoc, kz_json:new())) of
        'undefined' ->
            lager:info("failed to find ~s in webhook's custom data", [ModifierKey]),
            Msg = kz_json:from_list([{<<"message">>, <<"missing required modifier">>}]),
            cb_context:add_validation_error([<<"custom_data">>, ModifierKey], <<"required">>, Msg, Context);
        CustomValue ->
            lager:debug("found ~s in webhook's custom data: ~p", [ModifierKey, CustomValue]),
            Type = kz_json:get_ne_binary_value(<<"type">>, ModifierValue),
            check_modifier_values(CustomValue, ModifierKey, ModifierValue, Type, Context)
    end.

-spec check_modifier_values(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
check_modifier_values(CustomValue, ModifierKey, ModifierValue, <<"array">>, Context) ->
    Items = kz_json:get_value(<<"items">>, ModifierValue, []),
    case lists:member(CustomValue, Items) of
        'true' -> Context;
        'false' when CustomValue =:= <<"all">> ->
            lager:info("modifier ~s is for all items in ~p", [ModifierKey, Items]),
            Context;
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"value not found in enumerated list of values">>}
                                    ,{<<"cause">>, CustomValue}
                                    ,{<<"target">>, Items}
                                    ]),
            cb_context:add_validation_error([<<"custom_data">>, ModifierKey], <<"enum">>, Msg, Context)
    end;
check_modifier_values(CustomValue, ModifierKey, ModifierValue, <<"object">>, Context) ->
    case kz_json:get_value([<<"items">>, CustomValue], ModifierValue) of
        'undefined' ->
            Keys = kz_json:get_keys(kz_json:get_value(<<"items">>, ModifierValue)),
            Target = case cb_context:is_superduper_admin(Context) of
                         'true' -> Keys;
                         'false' -> Keys -- ?NOTIFY_SUPER_ADMIN_ONLY
                     end,
            Msg = kz_json:from_list([{<<"message">>, <<"value not found in enumerated list of values">>}
                                    ,{<<"cause">>, CustomValue}
                                    ,{<<"target">>, Target}
                                    ]),
            cb_context:add_validation_error([<<"custom_data">>, ModifierKey], <<"enum">>, Msg, Context);
        _ ->
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc Fetch the metadata, like modifiers available, for a hook type.
%% For instance, object webhooks have the type of object changed (device, user,
%% etc) and the type of change (created, updated, deleted) as modifiers.
%% @end
%%------------------------------------------------------------------------------
-spec get_hook_definition(kz_term:ne_binary()) -> kz_term:api_object().
get_hook_definition(HookEvent) ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterDb} -> get_hook_definition(HookEvent, MasterDb);
        {'error', _} -> 'undefined'
    end.

-spec get_hook_definition(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
get_hook_definition(HookEvent, MasterDb) ->
    case kz_datamgr:open_cache_doc(MasterDb, <<"webhooks_", HookEvent/binary>>) of
        {'ok', JObj} -> JObj;
        {'error', _Reason} ->
            lager:debug("failed to open webhook ~s definition: ~p", [HookEvent, _Reason]),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc If a hook was auto-disabled and is being re-enabled, cleanup the private
%% fields related to the auto-disabling
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_hook(cb_context:context()) -> cb_context:context().
maybe_update_hook(Context) ->
    Doc = cb_context:doc(Context),
    case kzd_webhooks:enabled(Doc) of
        'false' -> Context;
        'true' -> cb_context:set_doc(Context, kzd_webhooks:enable(Doc))
    end.

-spec reenable_hooks(cb_context:context()) ->
          cb_context:context().
reenable_hooks(Context) ->
    reenable_hooks(Context, props:get_value(<<"accounts">>, cb_context:req_nouns(Context))).

-spec reenable_hooks(cb_context:context(), kz_term:ne_binaries()) ->
          cb_context:context().
reenable_hooks(Context, [AccountId]) ->
    handle_resp(Context, send_reenable_req(Context, AccountId, <<"account">>));
reenable_hooks(Context, [AccountId, ?DESCENDANTS]) ->
    handle_resp(Context, send_reenable_req(Context, AccountId, ?DESCENDANTS)).

-spec send_reenable_req(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          kz_amqp_worker:request_return().
send_reenable_req(Context, AccountId, Action) ->
    Req = [{<<"Type">>, kzd_webhooks:type()}
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
