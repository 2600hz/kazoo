%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
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
-include_lib("kazoo_json/include/kazoo_json.hrl").

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

-spec init() -> ok.
init() ->
    _ = kz_datamgr:db_create(?KZ_WEBHOOKS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),
    init_master_account_db(),
    maybe_revise_schema(),

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
    ok.

-spec init_master_account_db() -> 'ok'.
init_master_account_db() ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            _ = kz_datamgr:revise_doc_from_file(MasterAccountDb
                                               ,'webhooks'
                                               ,<<"webhooks.json">>
                                               ),
            lager:debug("ensured view into master db");
        {'error', _E} ->
            lager:warning("master account not set yet, unable to load view: ~p", [_E])
    end.

-spec maybe_revise_schema() -> 'ok'.
-spec maybe_revise_schema(kz_json:object()) -> 'ok'.
-spec maybe_revise_schema(kz_json:object(), ne_binary()) -> 'ok'.
maybe_revise_schema() ->
    case kz_json_schema:load(<<"webhooks">>) of
        {'ok', SchemaJObj} -> maybe_revise_schema(SchemaJObj);
        {'error', _E} ->
            lager:warning("failed to find webhooks schema: ~p", [_E])
    end.

maybe_revise_schema(SchemaJObj) ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterDb} -> maybe_revise_schema(SchemaJObj, MasterDb);
        {'error', _E} ->
            lager:warning("master account not set yet, unable to revise schema: ~p", [_E])
    end.

maybe_revise_schema(SchemaJObj, MasterDb) ->
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
resource_exists(_) -> 'true'.
resource_exists(_Id, ?PATH_TOKEN_ATTEMPTS) -> 'true'.

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
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    validate_webhooks(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)
                     ,cb_context:req_verb(Context)
                     ).

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
    summary_attempts(Context);
validate(Context, Id) ->
    validate_webhook(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)
                    ,Id
                    ,cb_context:req_verb(Context)
                    ).

-spec validate_webhook(cb_context:context(), path_token(), http_method()) ->
                              cb_context:context().
validate_webhook(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_webhook(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_webhook(Context, Id, ?HTTP_PATCH) ->
    validate_patch(read(Id, Context), Id);
validate_webhook(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

validate(Context, Id=?NE_BINARY, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, Id).

-spec validate_patch(cb_context:context(), ne_binary()) ->
                            cb_context:context().
validate_patch(Context, Id) ->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = kz_doc:public_fields(cb_context:req_data(Context)),
            JObj = kz_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnValidateReqDataSuccess =
                fun(C) -> crossbar_doc:load_merge(Id, C, ?TYPE_CHECK_OPTION(kzd_webhook:type())) end,
            cb_context:validate_request_data(<<"webhooks">>
                                            ,cb_context:set_req_data(Context, JObj)
                                            ,OnValidateReqDataSuccess
                                            );
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

patch(Context, Id) ->
    post(Context, Id).

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

-spec fetch_account_hooks(ne_binary()) -> kz_datamgr:get_results_return().
fetch_account_hooks(AccountId) ->
    kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                          ,<<"webhooks/accounts_listing">>
                          ,[{'key', AccountId}
                           ,{'reduce', 'false'}
                           ,'include_docs'
                           ]
                          ).

-spec delete_account_hooks(kz_json:objects()) -> any().
delete_account_hooks(ViewJObjs) ->
    kz_datamgr:del_docs(?KZ_WEBHOOKS_DB
                       ,[kz_json:get_value(<<"doc">>, ViewJObj)
                         || ViewJObj <- ViewJObjs
                        ]
                       ).

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
    case kz_util:is_true(ReEnable) of
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
    maybe_fix_envelope(
      crossbar_doc:load_view(?CB_LIST
                            ,[{'startkey', [cb_context:account_id(Context), get_summary_start_key(Context)]}
                             ,{'endkey', [cb_context:account_id(Context), kz_json:new()]}
                             ]
                            ,Context
                            ,fun normalize_view_results/2
                            )
     ).

-spec maybe_fix_envelope(cb_context:context()) -> cb_context:context().
maybe_fix_envelope(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> fix_envelope(Context);
        _Status -> Context
    end.

-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    UpdatedEnvelope =
        case {cb_context:doc(Context)
             ,cb_context:resp_envelope(Context)
             }
        of
            {[], Envelope} ->
                kz_json:delete_keys([<<"start_key">>, <<"next_start_key">>], Envelope);
            {_, Envelope} ->
                fix_keys(Envelope)
        end,
    cb_context:set_resp_envelope(Context, UpdatedEnvelope).

-spec fix_keys(kz_json:object()) -> kz_json:object().
fix_keys(Envelope) ->
    lists:foldl(fun fix_key_fold/2
               ,Envelope
               ,[<<"start_key">>, <<"next_start_key">>]
               ).

-spec fix_key_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_key_fold(Key, Envelope) ->
    case kz_json:get_value(Key, Envelope) of
        [_AccountId, ?EMPTY_JSON_OBJECT] -> kz_json:delete_key(Key, Envelope);
        [_AccountId, 0] -> kz_json:delete_key(Key, Envelope);
        [_AccountId, Value] -> kz_json:set_value(Key, Value, Envelope);
        <<_/binary>> = _ -> Envelope;
        0 -> kz_json:delete_key(Key, Envelope);
        I when is_integer(I) -> Envelope;
        ?EMPTY_JSON_OBJECT -> kz_json:delete_key(Key, Envelope);
        'undefined' -> Envelope
    end.

-spec summary_available(cb_context:context()) ->
                               cb_context:context().
summary_available(Context) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),

    crossbar_doc:load_view(?AVAILABLE_HOOKS
                          ,['include_docs']
                          ,cb_context:set_account_db(Context, MasterAccountDb)
                          ,fun normalize_available/2
                          ).

-spec normalize_available(kz_json:object(), kz_json:objects()) ->
                                 kz_json:objects().
normalize_available(JObj, Acc) ->
    case kz_json:get_value(<<"key">>, JObj) of
        <<"skel">> -> Acc;
        _ ->
            Doc = kz_json:public_fields(kz_json:get_value(<<"doc">>, JObj)),
            Name = kz_json:get_value(<<"name">>, Doc),

            [kz_json:set_value(<<"id">>, Name, Doc) | Acc]
    end.

-spec summary_attempts(cb_context:context()) -> cb_context:context().
-spec summary_attempts(cb_context:context(), api_binary()) -> cb_context:context().
summary_attempts(Context) ->
    summary_attempts(Context, 'undefined').

summary_attempts(Context, 'undefined') ->
    ViewOptions = [{'endkey', 0}
                  ,{'startkey', get_attempts_start_key(Context)}
                  ,'include_docs'
                  ,'descending'
                  ],
    summary_attempts_fetch(Context, ViewOptions, ?ATTEMPTS_BY_ACCOUNT);
summary_attempts(Context, <<_/binary>> = HookId) ->
    ViewOptions = [{'endkey', [HookId, 0]}
                  ,{'startkey', [HookId, get_attempts_start_key(Context)]}
                  ,'include_docs'
                  ,'descending'
                  ],
    summary_attempts_fetch(Context, ViewOptions, ?ATTEMPTS_BY_HOOK).

-spec get_summary_start_key(cb_context:context()) -> ne_binary() | integer().
get_summary_start_key(Context) ->
    get_start_key(Context, 0, fun kz_util:identity/1).

-spec get_attempts_start_key(cb_context:context()) ->
                                    integer() | ?EMPTY_JSON_OBJECT.
get_attempts_start_key(Context) ->
    get_start_key(Context, kz_json:new(), fun kz_util:to_integer/1).

-spec get_start_key(cb_context:context(), any(), fun()) -> any().
get_start_key(Context, Default, Formatter) ->
    case cb_context:req_value(Context, <<"start_key">>) of
        'undefined' -> Default;
        V -> Formatter(V)
    end.

-spec summary_attempts_fetch(cb_context:context(), crossbar_doc:view_options(), ne_binary()) ->
                                    cb_context:context().
summary_attempts_fetch(Context, ViewOpts, View) ->
    case get_modb(Context) of
        {'ok', Dbs} ->
            ViewOptions = ViewOpts ++ Dbs,
            lager:debug("loading view ~s with options ~p", [View, ViewOptions]),
            maybe_fix_envelope(
              crossbar_doc:load_view(View
                                    ,ViewOptions
                                    ,Context
                                    ,fun normalize_attempt_results/2
                                    ));
        Ctx -> Ctx
    end.

-spec normalize_attempt_results(kz_json:object(), kz_json:objects()) ->
                                       kz_json:objects().
normalize_attempt_results(JObj, Acc) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    [kz_json:delete_keys([<<"id">>, <<"_id">>]
                        ,kz_json:set_value(<<"timestamp">>, kz_doc:created(Doc), Doc)
                        )
     | Acc
    ].

-spec get_modb(cb_context:context()) ->
                      {'ok', crossbar_doc:view_options()} |
                      cb_context:context().
get_modb(Context) ->
    AccountId = cb_context:account_id(Context),
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            Databases = kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo),
            {'ok', [{'databases', lists:reverse(Databases)}]};
        Ctx -> Ctx
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) ->
                                      cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_json:set_values([{<<"pvt_type">>, kzd_webhook:type()}
                                          ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
                                          ]
                                         ,cb_context:doc(Context)
                                         )
                      );
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_webhook:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

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
    reenable_hooks(Context
                  ,props:get_value(<<"accounts">>, cb_context:req_nouns(Context))
                  ).

reenable_hooks(Context, [AccountId]) ->
    handle_resp(
      Context
               ,send_reenable_req(Context, AccountId, <<"account">>)
     );
reenable_hooks(Context, [AccountId, ?DESCENDANTS]) ->
    handle_resp(
      Context
               ,send_reenable_req(Context, AccountId, ?DESCENDANTS)
     ).

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
                       ,fun kz_util:always_true/1
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
