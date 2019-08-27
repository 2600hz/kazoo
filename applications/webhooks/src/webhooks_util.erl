%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_util).

-export([from_json/1
        ,to_json/1
        ]).
-export([find_webhooks/2]).
-export([fire_hooks/2]).
-export([init_webhooks/0
        ,init_webhook_db/0
        ,jobj_to_rec/1
        ,hook_event_lowered/1
        ,hook_event/1
        ,load_hooks/1
        ,load_hook/2
        ,hook_id/1
        ,hook_id/2

        ,account_expires_time/1
        ,system_expires_time/0
        ,account_failure_count/1
        ,system_failure_count/0

        ,reenable/2
        ,init_metadata/2
        ,available_events/0
        ]).

%% ETS Management
-export([table_id/0
        ,table_options/0
        ,gift_data/0
        ]).

-ifdef(TEST).
-export([note_failed_attempt/3]).
-endif.

-include("webhooks.hrl").

-define(TABLE, 'webhooks_listener').

-define(CONNECT_TIMEOUT_MS
       ,kapps_config:get_integer(?APP_NAME, <<"connect_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).
-define(HTTP_OPTS, [{'connect_timeout', ?CONNECT_TIMEOUT_MS}]).

-define(HTTP_TIMEOUT_MS
       ,kapps_config:get_integer(?APP_NAME, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).

-define(HTTP_REQ_HEADERS(Hook)
       ,[{"X-Hook-ID", kz_term:to_list(Hook#webhook.hook_id)}
        ,{"X-Account-ID", kz_term:to_list(Hook#webhook.account_id)}
        ]).

-define(CONF_LOG_SUCCESS, <<"log_successful_attempts">>).
-define(SHOULD_LOG_SUCCESS
       ,kapps_config:get_is_true(?APP_NAME, ?CONF_LOG_SUCCESS, 'false')
       ).

-define(AVAILABLE_EVENT_KEY, 'available_events').

-spec table_id() -> ?TABLE.
table_id() -> ?TABLE.

-spec table_options() -> list().
table_options() -> ['set'
                   ,'protected'
                   ,{'keypos', #webhook.id}
                   ,'named_table'
                   ].

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

-spec from_json(kz_json:object()) -> webhook().
from_json(Hook) ->
    jobj_to_rec(Hook).

-spec to_json(webhook()) -> kz_json:object().
to_json(#webhook{}=Hook) ->
    kz_json:from_list(
      [{<<"_id">>, Hook#webhook.id}
      ,{<<"uri">>, Hook#webhook.uri}
      ,{<<"http_verb">>, kz_term:to_binary(Hook#webhook.http_verb)}
      ,{<<"retries">>, Hook#webhook.retries}
      ,{<<"account_id">>, Hook#webhook.account_id}
      ,{<<"include_subaccounts">>, Hook#webhook.include_subaccounts}
      ,{<<"include_internal_legs">>, Hook#webhook.include_loopback}
      ,{<<"custom_data">>, Hook#webhook.custom_data}
      ,{<<"modifiers">>, Hook#webhook.modifiers}
      ]).

-spec find_webhooks(kz_term:ne_binary(), kz_term:api_binary()) -> webhooks().
find_webhooks(_HookEvent, 'undefined') -> [];
find_webhooks(HookEvent, AccountId) ->
    case kzd_accounts:fetch(AccountId, 'accounts') of
        {'ok', JObj} ->
            Accounts = kzd_accounts:tree(JObj) -- [AccountId],
            find_webhooks(HookEvent, AccountId, Accounts);
        {'error', 'not_found'} -> []
    end.

find_webhooks(HookEvent, AccountId, Accounts) ->
    match_account_webhooks(HookEvent, AccountId) ++
        lists:foldl(fun(ParentId, Acc) ->
                            Acc ++ match_subaccount_webhooks(HookEvent, ParentId)
                    end
                   ,[]
                   ,Accounts
                   ).

match_account_webhooks(HookEvent, AccountId) ->
    MatchSpec = [{#webhook{account_id = '$1'
                          ,hook_event = '$2'
                          ,_='_'
                          }
                 ,[{'andalso'
                   ,{'=:=', '$1', {'const', AccountId}}
                   ,{'orelse'
                    ,{'=:=', '$2', {'const', HookEvent}}
                    ,{'=:=', '$2', {'const', <<"all">>}}
                    }
                   }]
                 ,['$_']
                 }],
    ets:select(table_id(), MatchSpec).

match_subaccount_webhooks(HookEvent, AccountId) ->
    MatchSpec = [{#webhook{account_id = '$1'
                          ,hook_event = '$2'
                          ,include_subaccounts = 'true'
                          ,_='_'
                          }
                 ,[{'andalso'
                   ,{'=:=', '$1', {'const', AccountId}}
                   ,{'orelse'
                    ,{'=:=', '$2', {'const', HookEvent}}
                    ,{'=:=', '$2', {'const', <<"all">>}}
                    }
                   }]
                 ,['$_']
                 }],
    ets:select(table_id(), MatchSpec).

-spec fire_hooks(kz_json:object(), webhooks()) -> 'ok'.
fire_hooks(_, []) -> 'ok';
fire_hooks(JObj, [Hook | Hooks]) ->
    maybe_fire_hook(JObj, Hook),
    fire_hooks(JObj, Hooks).

-spec maybe_fire_hook(kz_json:object(), webhook()) -> 'ok'.
maybe_fire_hook(JObj, #webhook{modifiers='undefined'}=Hook) ->
    fire_hook(JObj, Hook);
maybe_fire_hook(JObj, #webhook{modifiers=Modifiers}=Hook) ->
    case kz_json:foldl(fun maybe_fire_foldl/3, {'true', JObj}, Modifiers) of
        {'false', _} -> 'ok';
        {'true', _} ->  fire_hook(JObj, Hook)
    end.

-type maybe_fire_acc() :: {boolean(), kz_json:object()}.
-spec maybe_fire_foldl(kz_term:ne_binary(), any(), maybe_fire_acc()) -> maybe_fire_acc().

maybe_fire_foldl(_Key, _Value, {'false', _}=Acc) ->
    Acc;
maybe_fire_foldl(_Key, [], Acc) -> Acc;
maybe_fire_foldl(Key, Value, {_ShouldFire, JObj}) when is_list(Value) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> {'true', JObj};
        Data ->
            {lists:member(Data, Value), JObj}
    end;
maybe_fire_foldl(Key, Value, {_ShouldFire, JObj}) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> {'true', JObj};
        Value -> {'true', JObj};
        _Else -> {'false', JObj}
    end.

-spec fire_hook(kz_json:object(), webhook()) -> 'ok'.
fire_hook(JObj, #webhook{custom_data = 'undefined'
                        } = Hook) ->
    EventId = kz_binary:rand_hex(5),
    NewJObj = kz_json:set_value(<<"cluster_id">>, kzd_cluster:id(), JObj),
    do_fire(Hook, EventId, NewJObj);
fire_hook(JObj, #webhook{custom_data = CustomData
                        } = Hook) ->
    NewJObj = kz_json:merge_jobjs(CustomData, JObj),
    fire_hook(NewJObj, Hook#webhook{custom_data = 'undefined'}).

-spec do_fire(webhook(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
do_fire(#webhook{uri = ?NE_BINARY = URI
                ,http_verb = 'get'
                ,retries = Retries
                ,hook_event = _HookEvent
                ,hook_id = _HookId
                } = Hook, EventId, JObj) ->
    lager:debug("sending hook ~s(~s) with interaction id ~s via 'get' (retries ~b): ~s", [_HookEvent, _HookId, EventId, Retries, URI]),

    Url = kz_term:to_list(
            list_to_binary([kz_term:to_binary(URI)
                           ,kz_term:to_binary([$? | kz_http_util:json_to_querystring(JObj)])
                           ])
           ),
    Headers = ?HTTP_REQ_HEADERS(Hook),
    Debug = debug_req(Hook, EventId, URI, Headers, <<>>),
    Fired = kz_http:get(Url, Headers, ?HTTP_OPTS),
    handle_resp(Hook, EventId, JObj, Debug, Fired);
do_fire(#webhook{uri = ?NE_BINARY = URI
                ,http_verb = 'post'
                ,retries = Retries
                ,hook_event = _HookEvent
                ,hook_id = _HookId
                ,format = 'form-data'
                } = Hook, EventId, JObj) ->
    lager:debug("sending hook ~s(~s) with interaction id ~s via 'post' (retries ~b): ~s", [_HookEvent, _HookId, EventId, Retries, URI]),

    Body = kz_http_util:json_to_querystring(JObj),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}
               | ?HTTP_REQ_HEADERS(Hook)
              ],
    Debug = debug_req(Hook, EventId, URI, Headers, Body),
    Fired = kz_http:post(URI, Headers, Body, ?HTTP_OPTS),

    handle_resp(Hook, EventId, JObj, Debug, Fired);
do_fire(#webhook{uri = ?NE_BINARY = URI
                ,http_verb = 'post'
                ,retries = Retries
                ,hook_event = _HookEvent
                ,hook_id = _HookId
                ,format = 'json'
                } = Hook, EventId, JObj) ->
    lager:debug("sending hook ~s(~s) with interaction id ~s via 'post' (retries ~b): ~s", [_HookEvent, _HookId, EventId, Retries, URI]),

    Body = kz_json:encode(JObj, ['pretty']),
    Headers = [{"Content-Type", "application/json"}
               | ?HTTP_REQ_HEADERS(Hook)
              ],
    Debug = debug_req(Hook, EventId, URI, Headers, Body),
    Fired = kz_http:post(URI, Headers, Body, ?HTTP_OPTS),

    handle_resp(Hook, EventId, JObj, Debug, Fired).

-spec handle_resp(webhook(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist(), kz_http:ret()) -> 'ok'.
handle_resp(#webhook{hook_event = _HookEvent
                    ,hook_id = _HookId
                    } = Hook, _EventId, _JObj, Debug, {'ok', 200, _, _} = Resp) ->
    lager:debug("sent hook call event ~s(~s) with interaction id ~s successfully", [_HookEvent, _HookId, _EventId]),
    successful_hook(Hook, Debug, Resp);
handle_resp(#webhook{hook_event = _HookEvent
                    ,hook_id = _HookId
                    } = Hook, _EventId, _JObj, Debug, {'ok', RespCode, _, _} = Resp) ->
    _ = failed_hook(Hook, Debug, Resp),
    lager:debug("non-200 response code: ~p on account ~s for event ~s(~s) with interaction id ~s"
               ,[RespCode, Hook#webhook.account_id, _HookEvent, _HookId, _EventId]
               );
handle_resp(#webhook{hook_event = _HookEvent
                    ,hook_id = _HookId
                    } = Hook, EventId, JObj, Debug, {'error', _E} = Resp) ->
    lager:debug("failed to fire hook event ~s(~s) interaction id: ~p error: ~p", [_HookEvent, _HookId, EventId, _E]),
    _ = failed_hook(Hook, Debug, Resp),
    retry_hook(Hook, EventId, JObj).

-spec retry_hook(webhook(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
retry_hook(#webhook{uri = _URI
                   ,retries = 1
                   ,hook_id = _HookId
                   ,hook_event = _HookEvent
                   }, _EventId, _JObj) ->
    lager:debug("retries exhausted for event ~s(~s) with interaction id ~s for uri (~s)", [_HookEvent, _HookId, _EventId, _URI]);
retry_hook(#webhook{retries = Retries} = Hook, EventId, JObj) ->
    timer:sleep(2000),
    do_fire(Hook#webhook{retries = Retries - 1}, EventId, JObj).

-spec successful_hook(webhook(), kz_term:proplist(), kz_http:ret()) -> 'ok'.
successful_hook(Hook, Debug, Resp) ->
    successful_hook(Hook, Debug, Resp, ?SHOULD_LOG_SUCCESS).

-spec successful_hook(webhook(), kz_term:proplist(), kz_http:ret(), boolean()) -> 'ok'.
successful_hook(_Hook, _Debug, _Resp, 'false') -> 'ok';
successful_hook(#webhook{account_id = AccountId}, Debug, Resp, 'true') ->
    DebugJObj = debug_resp(Resp, Debug, 'undefined'),
    save_attempt(AccountId, DebugJObj).

-spec failed_hook(webhook(), kz_term:proplist(), kz_http:ret()) -> 'ok'.
failed_hook(#webhook{hook_id = HookId
                    ,account_id = AccountId
                    ,retries = Retries
                    }, Debug, Resp) ->
    note_failed_attempt(AccountId, HookId),
    DebugJObj = debug_resp(Resp, Debug, Retries),
    save_attempt(AccountId, DebugJObj).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_attempt(kz_term:api_binary(), kz_json:object()) -> 'ok'.
save_attempt(AccountId, Attempt) ->
    Now = kz_time:now_s(),
    ModDb = kz_util:format_account_mod_id(AccountId, Now),

    Doc = kz_json:set_values(
            props:filter_undefined(
              [{<<"pvt_account_db">>, ModDb}
              ,{<<"pvt_account_id">>, AccountId}
              ,{<<"pvt_type">>, <<"webhook_attempt">>}
              ,{<<"pvt_created">>, Now}
              ,{<<"pvt_modified">>, Now}
              ]), Attempt),
    _ = kazoo_modb:save_doc(ModDb, Doc, [{'publish_change_notice', 'false'}]),
    'ok'.

-spec debug_req(webhook(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), iodata()) ->
                       kz_term:proplist().
debug_req(#webhook{hook_id=HookId
                  ,hook_event = HookEvent
                  ,http_verb = Method
                  ,custom_data = CustomData
                  }, EventId, URI, ReqHeaders, ReqBody) ->
    Headers = kz_json:from_list(
                [{fix_value(K), fix_value(V)}
                 || {K, V} <- ReqHeaders
                ]),
    [{<<"hook_id">>, HookId}
    ,{<<"hook_event">>, HookEvent}
    ,{<<"custom_data">>, CustomData}
    ,{<<"event_id">>, EventId}
    ,{<<"uri">>, kz_term:to_binary(URI)}
    ,{<<"method">>, kz_term:to_binary(Method)}
    ,{<<"req_headers">>, Headers}
    ,{<<"req_body">>, ReqBody}
    ].

-spec debug_resp(kz_http:ret(), kz_term:proplist(), hook_retries() | 'undefined') ->
                        kz_json:object().
debug_resp({'ok', RespCode, RespHeaders, RespBody}, Debug, Retries) ->
    Headers = kz_json:from_list(
                [{fix_value(K), fix_value(V)}
                 || {K, V} <- RespHeaders
                ]),
    Result = case RespCode of
                 200 -> [{<<"result">>, <<"success">>}];
                 _ ->
                     [{<<"result">>, <<"failure">>}
                     ,{<<"reason">>, <<"bad response code">>}
                     ]
             end,
    RetriesLeft = case Retries of
                      'undefined' -> 'undefined';
                      1 -> <<"retries exceeded">>;
                      _ -> Retries - 1
                  end,
    kz_json:from_list(
      [{<<"resp_status_code">>, kz_term:to_binary(RespCode)}
      ,{<<"resp_headers">>, Headers}
      ,{<<"resp_body">>, RespBody}
      ,{<<"try">>, Retries}
      ,{<<"retries_left">>, RetriesLeft}
       | Result ++ Debug
      ]);
debug_resp({'error', E}, Debug, Retries) ->
    Error = try fix_error_value(E) of
                Bin -> Bin
            catch
                _E:_R ->
                    lager:debug("failed to convert error ~p", [E]),
                    <<"unknown">>
            end,
    RetriesLeft = case Retries of
                      'undefined' -> 'undefined';
                      1 -> <<"retries exceeded">>;
                      _ -> Retries
                  end,
    kz_json:from_list(
      [{<<"result">>, <<"failure">>}
      ,{<<"reason">>, <<"kazoo http client error">>}
      ,{<<"retries">>, Retries}
      ,{<<"retries_left">>, RetriesLeft}
      ,{<<"client_error">>, Error}
       | Debug
      ]).

-spec fix_value(number() | list()) -> number() | kz_term:ne_binary().
fix_value(N) when is_number(N) -> N;
fix_value(O) -> kz_term:to_lower_binary(O).

-spec fix_error_value(atom() | {atom(), atom()}) -> kz_term:ne_binary().
fix_error_value({E, R}) ->
    list_to_binary([kz_term:to_binary(E)
                   ,": "
                   ,kz_term:to_binary(R)
                   ]);
fix_error_value(E) ->
    kz_term:to_binary(E).

-spec hook_id(kz_json:object()) -> kz_term:ne_binary().
hook_id(JObj) ->
    hook_id(kz_json:get_first_defined([<<"pvt_account_id">>
                                      ,<<"Account-ID">>
                                      ]
                                     ,JObj
                                     )
           ,kz_json:get_first_defined([<<"_id">>, <<"ID">>], JObj)
           ).

-spec hook_id(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
hook_id(AccountId, Id) ->
    <<AccountId/binary, ".", Id/binary>>.

-spec hook_event(kz_term:ne_binary()) -> kz_term:ne_binary().
hook_event(Bin) -> hook_event_lowered(kz_term:to_lower_binary(Bin)).

-spec hook_event_lowered(kz_term:ne_binary()) -> kz_term:ne_binary().
hook_event_lowered(<<"channel_create">>) -> <<"CHANNEL_CREATE">>;
hook_event_lowered(<<"channel_answer">>) -> <<"CHANNEL_ANSWER">>;
hook_event_lowered(<<"channel_destroy">>) -> <<"CHANNEL_DESTROY">>;
hook_event_lowered(<<"channel_bridge">>) -> <<"CHANNEL_BRIDGE">>;
hook_event_lowered(<<"all">>) -> <<"all">>;
hook_event_lowered(Event) ->
    'true' = lists:member(Event, available_events()),
    Event.

-spec load_hooks(pid()) -> 'ok'.
load_hooks(Srv) ->
    lager:debug("loading hooks into memory"),
    case kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/webhooks_listing">> %% excludes disabled
                               ,['include_docs']
                               )
    of
        {'ok', []} ->
            lager:debug("no configured webhooks");
        {'ok', WebHooks} ->
            {NeedMigrate, WHs} = maybe_need_migrate(WebHooks),
            lager:debug("~b out of ~b webhooks need to migrate", [length(NeedMigrate), length(WHs)]),
            load_hooks(Srv, WHs),
            migrate_load_hook(Srv, NeedMigrate);
        {'error', _E} ->
            lager:debug("failed to load webhooks: ~p", [_E])
    end.

-spec init_webhook_db() -> 'ok'.
init_webhook_db() ->
    _ = kz_datamgr:db_create(?KZ_WEBHOOKS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),
    'ok'.

-spec maybe_need_migrate(kz_json:objects()) -> {kz_json:objects(), kz_json:objects()}.
maybe_need_migrate(WebHooks) ->
    NeedMigrate = [<<"callflow">>, <<"inbound_fax">>, <<"outbound_fax">>],
    Fun = fun(Hook) -> lists:member(kzd_webhook:event(kz_json:get_value(<<"doc">>, Hook)), NeedMigrate) end,
    lists:partition(Fun, WebHooks).

-spec migrate_load_hook(pid(), kz_json:objects()) -> 'ok'.
migrate_load_hook(_, []) -> 'ok';
migrate_load_hook(Srv, WebHooks) ->
    case migrate_hooks(update_hook_docs(WebHooks, #{update => [], extra => []})) of
        {'ok', Hooks} ->
            _ = [load_hook(Srv, Hook) || Hook <- Hooks],
            lager:debug("successfully migrated and loaded ~b webhooks into server ~p", [length(Hooks), Srv]);
        {'error', _Reason} ->
            lager:debug("failed to migrate and load webhooks: ~p", [_Reason])
    end.

-spec update_hook_docs(kz_json:objects(), map()) -> map().
update_hook_docs([], Acc) -> Acc;
update_hook_docs([JObj|JObjs], Acc) ->
    Hook = kz_json:get_value(<<"doc">>, JObj),
    update_hook_docs(JObjs, update_hook_doc(Hook, kzd_webhook:event(Hook), Acc)).

-spec update_hook_doc(kz_json:object(), kz_term:ne_binary(), map()) -> map().
update_hook_doc(WebHook, <<"callflow">>, #{update := Update}=Acc) ->
    Acc#{update => [set_as_notifications_webhook(WebHook, <<"callflow">>, kz_doc:id(WebHook), 'undefined', kz_doc:revision(WebHook))
                    | Update
                   ]
        };
update_hook_doc(WebHook, <<"inbound_fax">>, #{update := Update, extra := Extra}=Acc) ->
    FaxErrorId = kz_binary:rand_hex(16),
    Acc#{update => [set_as_notifications_webhook(WebHook, <<"inbound_fax">>, kz_doc:id(WebHook), FaxErrorId, kz_doc:revision(WebHook))
                    | Update
                   ]
        ,extra => [set_as_notifications_webhook(WebHook, <<"inbound_fax_error">>, FaxErrorId, kz_doc:id(WebHook), 'undefined')
                   | Extra
                  ]
        };
update_hook_doc(WebHook, <<"outbound_fax">>, #{update := Update, extra := Extra}=Acc) ->
    FaxErrorId = kz_binary:rand_hex(16),
    Acc#{update => [set_as_notifications_webhook(WebHook, <<"outbound_fax">>, kz_doc:id(WebHook), FaxErrorId, kz_doc:revision(WebHook))
                    | Update
                   ]
        ,extra => [set_as_notifications_webhook(WebHook, <<"outbound_fax_error">>, FaxErrorId, kz_doc:id(WebHook), 'undefined')
                   | Extra
                  ]
        }.

-spec set_as_notifications_webhook(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_json:object().
set_as_notifications_webhook(WebHook, Type, Id, OtherId, Rev) ->
    kz_json:set_values([{<<"_id">>, Id}
                       ,{<<"_rev">>, Rev}
                       ,{<<"hook">>, <<"notifications">>}
                       ,{<<"type">>, Type}
                       ,{<<"other_id">>, OtherId}
                       ]
                      ,WebHook
                      ).

-spec migrate_hooks(map()) -> {'ok', kz_json:objects()} | {'error', any()}.
migrate_hooks(#{update := Update, extra := Extra}) ->
    case kz_datamgr:save_docs(?KZ_WEBHOOKS_DB, Update) of
        {'ok', JObjs} ->
            SavedIds = hooks_saved_successfully(JObjs),
            Saved = lists:filter(fun(J) -> sets:is_element(kz_doc:id(J), SavedIds) end, Update),
            ToSaveExtra = lists:filter(fun(J) -> is_other_hook_saved(J, SavedIds) end, Extra),
            lager:debug("~b/~b callflow,inbound/outbound fax hooks migrated to notifications, migrating ~b inbound/outbound fax error hooks to notifications"
                       ,[sets:size(SavedIds), length(Update), length(ToSaveExtra)]
                       ),
            {'ok', Saved ++ migrate_extra_hooks(ToSaveExtra)};
        {'error', _Reason}=Error ->
            lager:error("failed to save updated webhooks: ~p", [_Reason]),
            Error
    end.

%% if this webhook introduced as part of updating the other webhook
%% check if the other part is saved successfully to continue to save this hook
-spec is_other_hook_saved(kz_json:object(), sets:set()) -> boolean().
is_other_hook_saved(JObj, SavedIds) ->
    OtherId = kz_json:get_value(<<"other_id">>, JObj),
    OtherId =/= 'undefined'
        andalso sets:is_element(OtherId, SavedIds)
        orelse OtherId =:= 'undefined'.

%% FIXME: if the new {in,out}bound_fax_error notifications hooks failed to save we have to rollback
%%        migration, e.g. convert doc with other_id id to appropriate webhooks_{in,out}inbound_fax hook
%%        to re-start migration process in the next start-up
-spec migrate_extra_hooks(kz_json:objects()) -> kz_json:objects().
migrate_extra_hooks(WebHooks) ->
    _ = kz_datamgr:save_docs(?KZ_WEBHOOKS_DB, WebHooks),
    WebHooks.

-spec hooks_saved_successfully(kz_json:objects()) -> sets:set().
hooks_saved_successfully(JObjs) ->
    lists:foldl(fun(JObj, Ids) ->
                        case kz_json:get_value(<<"error">>, JObj) of
                            'undefined' -> sets:add_element(kz_doc:id(JObj), Ids);
                            _ ->
                                lager:error("failed to save updated webhook ~s: ~p", [kz_doc:id(JObj)]),
                                Ids
                        end
                end
               ,sets:new()
               ,JObjs
               ).

-spec load_hooks(pid(), kz_json:objects()) -> 'ok'.
load_hooks(Srv, WebHooks) ->
    _ = [load_hook(Srv, kz_json:get_value(<<"doc">>, Hook)) || Hook <- WebHooks],
    lager:debug("sent hooks into server ~p", [Srv]).

-spec load_hook(pid(), kz_json:object()) -> 'ok'.
load_hook(Srv, WebHook) ->
    try jobj_to_rec(WebHook) of
        Hook -> gen_listener:cast(Srv, {'add_hook', Hook})
    catch
        'throw':{'bad_hook', HookEvent} ->
            lager:debug("failed to load hook ~s.~s: bad_hook: ~s"
                       ,[kz_doc:account_id(WebHook)
                        ,kz_doc:id(WebHook)
                        ,HookEvent
                        ]
                       );
        _E:_R ->
            lager:debug("failed to load hook ~s.~s: ~s: ~p"
                       ,[kz_doc:account_id(WebHook)
                        ,kz_doc:id(WebHook)
                        ,_E
                        ,_R
                        ])
    end.

-spec jobj_to_rec(kz_json:object()) -> webhook().
jobj_to_rec(Hook) ->
    #webhook{id = hook_id(Hook)
            ,uri = kzd_webhook:uri(Hook)
            ,http_verb = kz_term:to_atom(kzd_webhook:verb(Hook), 'true')
            ,hook_event = hook_event(kzd_webhook:event(Hook))
            ,hook_id = kz_doc:id(Hook)
            ,retries = kzd_webhook:retries(Hook)
            ,account_id = kz_doc:account_id(Hook)
            ,include_subaccounts = kzd_webhook:include_subaccounts(Hook)
            ,include_loopback = kzd_webhook:include_internal_legs(Hook)
            ,custom_data = kzd_webhook:custom_data(Hook)
            ,modifiers = kzd_webhook:modifiers(Hook)
            ,format = kzd_webhook:format(Hook)
            }.

-spec init_webhooks() -> 'ok'.
init_webhooks() ->
    case kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'group_level', 1}]
                               )
    of
        {'ok', []} -> lager:debug("no accounts to load views into the MODs");
        {'ok', WebHooks} -> init_webhooks(WebHooks);
        {'error', _E} -> lager:debug("failed to load accounts_listing: ~p", [_E])
    end.

-spec init_webhooks(kz_json:objects()) -> 'ok'.
init_webhooks(WebHooks) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(kz_time:now_s()),
    init_webhooks(WebHooks, Year, Month).

-spec init_webhooks(kz_json:objects(), kz_time:year(), kz_time:month()) -> 'ok'.
init_webhooks(WebHooks, Year, Month) ->
    _ = [init_webhook(WebHook, Year, Month) || WebHook <- WebHooks],
    'ok'.

-spec init_webhook(kz_json:object(), kz_time:year(), kz_time:month()) -> 'ok'.
init_webhook(WebHook, Year, Month) ->
    Db = kz_util:format_account_id(kz_json:get_value(<<"key">>, WebHook), Year, Month),
    kazoo_modb:maybe_create(Db),
    lager:debug("updated account_modb ~s", [Db]).

-spec note_failed_attempt(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
note_failed_attempt(AccountId, HookId) ->
    note_failed_attempt(AccountId, HookId, kz_time:now_ms()).

-spec note_failed_attempt(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer()) -> 'ok'.
note_failed_attempt(AccountId, HookId, TimestampMs) ->
    kz_cache:store_local(?CACHE_NAME
                        ,?FAILURE_CACHE_KEY(AccountId, HookId, TimestampMs)
                        ,'true'
                        ,[{'expires', account_expires_time(AccountId)}]
                        ).

-spec account_expires_time(kz_term:ne_binary()) -> pos_integer().
account_expires_time(AccountId) ->
    Expiry = kapps_account_config:get_global(AccountId
                                            ,?APP_NAME
                                            ,?ATTEMPT_EXPIRY_KEY
                                            ,?MILLISECONDS_IN_MINUTE
                                            ),
    try kz_term:to_integer(Expiry) of
        I -> I
    catch
        _:_ -> ?MILLISECONDS_IN_MINUTE
    end.

-spec system_expires_time() -> pos_integer().
system_expires_time() ->
    kapps_config:get_integer(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, ?MILLISECONDS_IN_MINUTE).

-spec account_failure_count(kz_term:ne_binary()) -> pos_integer().
account_failure_count(AccountId) ->
    try kz_term:to_integer(kapps_account_config:get_global(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, ?DEFAULT_FAILURE_COUNT))
    catch _:_ -> ?DEFAULT_FAILURE_COUNT
    end.

-spec system_failure_count() -> pos_integer().
system_failure_count() ->
    kapps_config:get_integer(?APP_NAME, ?FAILURE_COUNT_KEY, ?DEFAULT_FAILURE_COUNT).

-spec reenable(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
reenable(AccountId, <<"account">>) ->
    enable_account_hooks(AccountId);
reenable(AccountId, <<"descendants">>) ->
    enable_descendant_hooks(AccountId).

-spec enable_account_hooks(kz_term:ne_binary()) -> 'ok'.
enable_account_hooks(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),

    case kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'key', AccountId}
                                ,{'reduce', 'false'}
                                ,'include_docs'
                                ]
                               )
    of
        {'ok', []} -> io:format("account ~s has no webhooks configured~n", [AccountId]);
        {'ok', Hooks} -> enable_hooks(Hooks);
        {'error', _E} -> io:format("failed to load hooks for account ~s: ~p~n", [AccountId, _E])
    end.

-spec enable_hooks(kz_json:objects()) -> 'ok'.
enable_hooks(Hooks) ->
    case hooks_to_reenable(Hooks) of
        [] -> io:format("no hooks to re-enable~n", []);
        Reenable ->
            {'ok', Saved} = kz_datamgr:save_docs(?KZ_WEBHOOKS_DB, Reenable),
            _ = webhooks_disabler:flush_hooks(Reenable),
            io:format("re-enabled ~p hooks~nIDs: ", [length(Saved)]),
            Ids = kz_binary:join([kz_doc:id(D) || D <- Saved], <<", ">>),
            io:format("~s~n", [Ids])
    end.

-spec hooks_to_reenable(kz_json:objects()) -> kz_json:objects().
hooks_to_reenable(Hooks) ->
    [kzd_webhook:enable(Hook)
     || View <- Hooks,
        kzd_webhook:is_auto_disabled(Hook = kz_json:get_value(<<"doc">>, View))
    ].

-spec enable_descendant_hooks(kz_term:ne_binary()) -> 'ok'.
enable_descendant_hooks(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB
                               ,<<"accounts/listing_by_descendants">>
                               ,[{'startkey', [AccountId]}
                                ,{'endkey', [AccountId, kz_json:new()]}
                                ]
                               )
    of
        {'ok', []} ->
            maybe_enable_descendants_hooks([AccountId]);
        {'ok', Descendants} ->
            maybe_enable_descendants_hooks([AccountId
                                            | [kz_json:get_value([<<"value">>, <<"id">>], D) || D <- Descendants]
                                           ]
                                          );
        {'error', _E} ->
            io:format("failed to find descendants for account ~s: ~p~n", [AccountId, _E])
    end.

-spec maybe_enable_descendants_hooks(kz_term:ne_binaries()) -> 'ok'.
maybe_enable_descendants_hooks(Accounts) ->
    lists:foreach(fun maybe_enable_descendant_hooks/1, Accounts).

-spec maybe_enable_descendant_hooks(kz_term:ne_binary()) -> 'ok'.
maybe_enable_descendant_hooks(Account) ->
    io:format("## checking account ~s for hooks to enable ##~n", [Account]),
    enable_account_hooks(Account).

-spec init_metadata(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
init_metadata(Id, JObj) ->
    init_metadata(Id, JObj, kapps_util:get_master_account_db()).

-spec init_metadata(kz_term:ne_binary(), kz_json:object(), {'ok', kz_term:ne_binary()} | {'error', any()}) -> 'ok'.
init_metadata(_, _, {'error', _}) ->
    lager:warning("master account not available"),
    'ok';
init_metadata(Id, JObj, {'ok', MasterAccountDb}) ->
    case kz_datamgr:open_doc(MasterAccountDb, Id) of
        {'error', _} -> save_metadata(MasterAccountDb, JObj);
        {'ok', Doc} ->
            case kz_json:are_equal(kz_doc:public_fields(Doc), kz_doc:public_fields(JObj)) of
                'true' -> 'ok';
                'false' ->
                    lager:debug("updating ~s", [Id]),
                    Merged = kz_json:merge(Doc, JObj),
                    save_metadata(MasterAccountDb, Merged)
            end
    end.

-spec save_metadata(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
save_metadata(MasterAccountDb, JObj) ->
    Metadata =  kz_doc:update_pvt_parameters(JObj, MasterAccountDb, [{'type', <<"webhook_meta">>}]),
    case kz_datamgr:save_doc(MasterAccountDb, Metadata) of
        {'ok', _Saved} ->
            lager:debug("~s initialized successfully", [kz_doc:id(JObj)]);
        {'error', 'conflict'} ->
            lager:debug("~s loaded elsewhere", [kz_doc:id(JObj)]);
        {'error', _E} ->
            lager:warning("failed to load metadata for ~s: ~p", [kz_doc:id(JObj), _E])
    end.

-spec available_events() -> kz_term:ne_binaries().
available_events() ->
    case kz_cache:fetch_local(?CACHE_NAME, ?AVAILABLE_EVENT_KEY) of
        {'error', 'not_found'} ->
            fetch_available_events();
        {'ok', Events} ->
            Events
    end.

-spec fetch_available_events() -> kz_term:ne_binaries().
fetch_available_events() ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MasterAccountDb, ?WEBHOOK_META_LIST) of
        {'ok', []} -> [];
        {'error', _} -> [];
        {'ok', Available} ->
            Events = [Id || <<"webhooks_", Id/binary>> <- [kz_doc:id(Hook) || Hook <- Available]],
            CacheProps = [{'origin', [{'db', MasterAccountDb, <<"webhook_meta">>}]}],
            kz_cache:store_local(?CACHE_NAME, ?AVAILABLE_EVENT_KEY, Events, CacheProps),
            Events
    end.
