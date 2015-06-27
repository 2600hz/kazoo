%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2
         ,patch/1, patch/2
         ,delete/2, delete_account/2
         ,cleanup/1
        ]).

-include("../crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"webhooks/crossbar_listing">>).

-define(PATH_TOKEN_ATTEMPTS, <<"attempts">>).

-define(ATTEMPTS_BY_ACCOUNT, <<"webhooks/attempts_by_time_listing">>).
-define(ATTEMPTS_BY_HOOK, <<"webhooks/attempts_by_hook_listing">>).

-define(DESCENDANTS, <<"descendants">>).
-define(REENABLE, <<"re-enable">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:db_create(?KZ_WEBHOOKS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = couch_mgr:revise_doc_from_file(?WH_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),

    Bindings = [{<<"*.allowed_methods.webhooks">>, 'allowed_methods'}
                ,{<<"*.resource_exists.webhooks">>, 'resource_exists'}
                ,{<<"*.validate.webhooks">>, 'validate'}
                ,{<<"*.execute.put.webhooks">>, 'put'}
                ,{<<"*.execute.post.webhooks">>, 'post'}
                ,{<<"*.execute.patch.webhooks">>, 'patch'}
                ,{<<"*.execute.delete.webhooks">>, 'delete'}
                ,{<<"*.execute.delete.accounts">>, 'delete_account'}
                ,{crossbar_cleanup:binding_system(), 'cleanup'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

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
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_Id, ?PATH_TOKEN_ATTEMPTS) ->
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
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_webhooks(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)
                      ,cb_context:req_verb(Context)
                     ).

-spec validate_webhooks(cb_context:context(), http_method()) -> cb_context:context().
validate_webhooks(Context, ?HTTP_GET) ->
    summary(Context);
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

-spec validate_webhook(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_webhook(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_webhook(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_webhook(Context, Id, ?HTTP_PATCH) ->
    validate_patch(read(Id, Context), Id);
validate_webhook(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

validate(Context, Id, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, Id).

-spec validate_patch(cb_context:context(), ne_binary()) -> cb_context:context().
validate_patch(Context, Id) ->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = wh_doc:public_fields(cb_context:req_data(Context)),
            JObj = wh_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnValidateReqDataSuccess = fun(C) -> crossbar_doc:load_merge(Id, C) end,
            cb_context:validate_request_data(<<"webhooks">>, cb_context:set_req_data(Context, JObj), OnValidateReqDataSuccess);
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
    wh_util:spawn(fun() -> delete_account_webhooks(AccountId) end),
    Context.

-spec delete_account_webhooks(ne_binary()) -> 'ok'.
delete_account_webhooks(AccountId) ->
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'key', AccountId}
                                 ,{'reduce', 'false'}
                                 ,'include_docs'
                                ]
                              )
    of
        {'ok', []} -> 'ok';
        {'error', _E} -> lager:debug("failed to fetch webhooks for account ~s: ~p", [AccountId, _E]);
        {'ok', ViewJObjs} ->
            _Res = couch_mgr:del_docs(?KZ_WEBHOOKS_DB
                                      ,[wh_json:get_value(<<"doc">>, ViewJObj) || ViewJObj <- ViewJObjs]
                                     ),
            lager:debug("deleted ~p hooks from account ~s", [length(ViewJObjs), AccountId])
    end.

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
    cb_context:add_validation_error(
      ?REENABLE
      ,<<"required">>
      ,wh_json:from_list(
         [{<<"message">>, <<"re-enable is required to patch collections">>}]
        )
      ,Context
     );
validate_collection_patch(Context, ReEnable) ->
    case wh_util:is_true(ReEnable) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_validation_error(
              ?REENABLE
              ,<<"enum">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"value not found in enumerated list of values">>}
                  ,{<<"target">>, ['true']}
                 ])
              ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_leak_pvt_fields(Context1);
        _Status -> Context1
    end.

-spec maybe_leak_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_leak_pvt_fields(Context) ->
    Doc = cb_context:doc(Context),
    wh_json:set_values([{<<"disable_reason">>, kzd_webhook:disabled_message(Doc)}]
                       ,Doc
                      ),
    cb_context:set_doc(Context, Doc).

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
                               ,{'endkey', [cb_context:account_id(Context), wh_json:new()]}
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
                wh_json:delete_keys([<<"start_key">>, <<"next_start_key">>], Envelope);
            {_, Envelope} ->
                fix_keys(Envelope)
        end,
    cb_context:set_resp_envelope(Context, UpdatedEnvelope).

-spec fix_keys(wh_json:object()) -> wh_json:object().
fix_keys(Envelope) ->
    lists:foldl(fun fix_key_fold/2, Envelope, [<<"start_key">>, <<"next_start_key">>]).

-spec fix_key_fold(wh_json:key(), wh_json:object()) -> wh_json:object().
fix_key_fold(Key, Envelope) ->
    case wh_json:get_value(Key, Envelope) of
        [_AccountId, ?EMPTY_JSON_OBJECT] -> wh_json:delete_key(Key, Envelope);
        [_AccountId, 0] -> wh_json:delete_key(Key, Envelope);
        [_AccountId, Value] -> wh_json:set_value(Key, Value, Envelope);
        <<_/binary>> = _ -> Envelope;
        0 -> wh_json:delete_key(Key, Envelope);
        I when is_integer(I) -> Envelope;
        ?EMPTY_JSON_OBJECT -> wh_json:delete_key(Key, Envelope);
        'undefined' -> Envelope
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
    get_start_key(Context, 0, fun wh_util:identity/1).

-spec get_attempts_start_key(cb_context:context()) -> integer() | ?EMPTY_JSON_OBJECT.
get_attempts_start_key(Context) ->
    get_start_key(Context, wh_json:new(), fun wh_util:to_integer/1).

-spec get_start_key(cb_context:context(), term(), fun()) -> term().
get_start_key(Context, Default, Formatter) ->
    case cb_context:req_value(Context, <<"start_key">>) of
        'undefined' -> Default;
        V -> Formatter(V)
    end.

-spec summary_attempts_fetch(cb_context:context(), crossbar_doc:view_options(), ne_binary()) ->
                                    cb_context:context().
summary_attempts_fetch(Context, ViewOptions, View) ->
    Db = wh_util:format_account_mod_id(cb_context:account_id(Context), wh_util:current_tstamp()),
    lager:debug("loading view ~s with options ~p", [View, ViewOptions]),
    maybe_fix_envelope(
      crossbar_doc:load_view(View
                             ,ViewOptions
                             ,cb_context:set_account_db(Context, Db)
                             ,fun normalize_attempt_results/2
                            )
     ).

-spec normalize_attempt_results(wh_json:object(), wh_json:objects()) ->
                                       wh_json:objects().
normalize_attempt_results(JObj, Acc) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Timestamp = wh_json:get_value(<<"pvt_created">>, Doc),
    [wh_json:delete_keys([<<"id">>, <<"_id">>]
                         ,wh_json:set_value(<<"timestamp">>, Timestamp, Doc)
                        )
     | Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, kzd_webhook:type()}
                                            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
                                           ]
                                           ,cb_context:doc(Context)
                                          )
                      );
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

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

-spec reenable_hooks(cb_context:context()) -> cb_context:context().
-spec reenable_hooks(cb_context:context(), ne_binaries()) ->
                            cb_context:context().
reenable_hooks(Context) ->
    reenable_hooks(Context, props:get_value(<<"accounts">>, cb_context:req_nouns(Context))).

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
                               wh_amqp_worker:request_return().
send_reenable_req(Context, AccountId, Action) ->
    Req = [{<<"Type">>, kzd_webhook:type()}
           ,{<<"Action">>, Action}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Msg-ID">>, cb_context:req_id(Context)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:call(Req
                        ,fun wapi_conf:publish_doc_type_update/1
                        ,fun wh_util:always_true/1
                       ).

-spec handle_resp(cb_context:context(), wh_amqp_worker:request_return()) ->
                         cb_context:context().
handle_resp(Context, {'ok', _Resp}) ->
    lager:debug("received resp from update"),
    crossbar_util:response(<<"hooks updated">>, Context);
handle_resp(Context, {'returned', _Request, BasicReturn}) ->
    lager:debug("no webhook apps running: ~p", [BasicReturn]),
    Resp = wh_json:delete_keys([<<"exchange">>, <<"routing_key">>], BasicReturn),
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

-spec cleanup(ne_binary()) -> 'ok'.
cleanup(?KZ_WEBHOOKS_DB) ->
    lager:debug("checking ~s for abandoned accounts", [?KZ_WEBHOOKS_DB]),
    cleanup_orphaned_hooks();
cleanup(_SystemDb) -> 'ok'.

-spec cleanup_orphaned_hooks() -> 'ok'.
cleanup_orphaned_hooks() ->
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,['group']
                              )
    of
        {'ok', []} -> lager:debug("no hooks configured");
        {'error', _E} ->
            lager:debug("failed to lookup accounts in ~s: ~p", [?KZ_WEBHOOKS_DB, _E]);
        {'ok', Accounts} ->
            _Rm = [begin
                       delete_account_webhooks(AccountId),
                       timer:sleep(5 * ?MILLISECONDS_IN_SECOND)
                   end
                   || Account <- Accounts,
                      begin
                          AccountId = wh_json:get_value(<<"key">>, Account),
                          not couch_mgr:db_exists(wh_util:format_account_id(AccountId, 'encoded'))
                      end
                  ],
            _Rm =/= [] andalso lager:debug("removed ~p accounts' webhooks", [length(_Rm)]),
            'ok'
    end.
