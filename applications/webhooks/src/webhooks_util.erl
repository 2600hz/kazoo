%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
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

        ,reenable/2
        ,init_metadata/2
        ]).

%% ETS Management
-export([table_id/0
        ,table_options/0
        ,gift_data/0
        ]).

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

-spec find_webhooks(ne_binary(), api_binary()) -> webhooks().
find_webhooks(_HookEvent, 'undefined') -> [];
find_webhooks(HookEvent, AccountId) ->
    case kz_account:fetch(AccountId, 'accounts') of
        {'ok', JObj} ->
            Accounts = kz_account:tree(JObj) -- [AccountId],
            find_webhooks(HookEvent, AccountId, Accounts);
        {'error', 'not_found'} -> []
    end.

find_webhooks(HookEvent, AccountId, Accounts) ->
    match_account_webhooks(HookEvent, AccountId) ++
        lists:foldl(fun(ParentId, Acc) ->
                            Acc ++ match_subaccount_webhooks(HookEvent, ParentId)
                    end, [], Accounts).

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
    {ShouldFireHook, _} =
        kz_json:foldl(
          fun maybe_fire_foldl/3
                     ,{'true', JObj}
                     ,Modifiers
         ),
    case ShouldFireHook of
        'false' -> 'ok';
        'true' ->  fire_hook(JObj, Hook)
    end.

-type maybe_fire_acc() :: {boolean(), kz_json:object()}.
-spec maybe_fire_foldl(ne_binary(), any(), maybe_fire_acc()) ->
                              maybe_fire_acc().
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
    EventId = kz_binary:rand_hex(16),
    do_fire(Hook, EventId, JObj);
fire_hook(JObj, #webhook{custom_data = CustomData
                        } = Hook) ->
    EventId = kz_binary:rand_hex(16),
    do_fire(Hook, EventId, kz_json:merge_jobjs(CustomData, JObj)).

-spec do_fire(webhook(), ne_binary(), kz_json:object()) -> 'ok'.
do_fire(#webhook{uri = ?NE_BINARY = URI
                ,http_verb = 'get'
                ,retries = Retries
                } = Hook, EventId, JObj) ->
    lager:debug("sending event ~s via 'get'(~b): ~s", [EventId, Retries, URI]),

    Url = kz_term:to_list(<<(kz_term:to_binary(URI))/binary
                            ,(kz_term:to_binary([$? | kz_http_util:json_to_querystring(JObj)]))/binary
                          >>),
    Headers = ?HTTP_REQ_HEADERS(Hook),
    Debug = debug_req(Hook, EventId, URI, Headers, <<>>),
    Fired = kz_http:get(Url, Headers, ?HTTP_OPTS),
    handle_resp(Hook, EventId, JObj, Debug, Fired);
do_fire(#webhook{uri = ?NE_BINARY = URI
                ,http_verb = 'post'
                ,retries = Retries
                } = Hook, EventId, JObj) ->
    lager:debug("sending event ~s via 'post'(~b): ~s", [EventId, Retries, URI]),

    Body = kz_http_util:json_to_querystring(JObj),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}
               | ?HTTP_REQ_HEADERS(Hook)
              ],
    Debug = debug_req(Hook, EventId, URI, Headers, Body),
    Fired = kz_http:post(URI, Headers, Body, ?HTTP_OPTS),

    handle_resp(Hook, EventId, JObj, Debug, Fired).

-spec handle_resp(webhook(), ne_binary(), kz_json:object(), kz_proplist(), kz_http:ret()) -> 'ok'.
handle_resp(Hook, _EventId, _JObj, Debug, {'ok', 200, _, _} = Resp) ->
    lager:debug("sent hook call event(~s) successfully", [_EventId]),
    successful_hook(Hook, Debug, Resp);
handle_resp(Hook, _EventId, _JObj, Debug, {'ok', RespCode, _, _} = Resp) ->
    _ = failed_hook(Hook, Debug, Resp),
    lager:debug("non-200 response code: ~p on account ~s for event ~s"
               ,[RespCode, Hook#webhook.account_id, _EventId]
               );
handle_resp(Hook, EventId, JObj, Debug, {'error', _E} = Resp) ->
    lager:debug("failed to fire hook(~s): ~p", [EventId, _E]),
    _ = failed_hook(Hook, Debug, Resp),
    retry_hook(Hook, EventId, JObj).

-spec retry_hook(webhook(), ne_binary(), kz_json:object()) -> 'ok'.
retry_hook(#webhook{uri = _URI
                   ,retries = 1
                   }, _EventId, _JObj) ->
    lager:debug("retries exhausted for ~s(~s)", [_URI, _EventId]);
retry_hook(#webhook{retries = Retries} = Hook, EventId, JObj) ->
    timer:sleep(2000),
    do_fire(Hook#webhook{retries = Retries - 1}, EventId, JObj).

-spec successful_hook(webhook(), kz_proplist(), kz_http:ret()) -> 'ok'.
-spec successful_hook(webhook(), kz_proplist(), kz_http:ret(), boolean()) -> 'ok'.
successful_hook(Hook, Debug, Resp) ->
    successful_hook(Hook, Debug, Resp, ?SHOULD_LOG_SUCCESS).

successful_hook(_Hook, _Debug, _Resp, 'false') -> 'ok';
successful_hook(#webhook{account_id = AccountId}, Debug, Resp, 'true') ->
    DebugJObj = debug_resp(Resp, Debug, 'undefined'),
    save_attempt(AccountId, DebugJObj).

-spec failed_hook(webhook(), kz_proplist(), kz_http:ret()) -> 'ok'.
failed_hook(#webhook{hook_id = HookId
                    ,account_id = AccountId
                    ,retries = Retries
                    }, Debug, Resp) ->
    note_failed_attempt(AccountId, HookId),
    DebugJObj = debug_resp(Resp, Debug, Retries),
    save_attempt(AccountId, DebugJObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec save_attempt(api_binary(), kz_json:object()) -> 'ok'.
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

-spec debug_req(webhook(), ne_binary(), ne_binary(), kz_proplist(), iodata()) ->
                       kz_proplist().
debug_req(#webhook{hook_id=HookId
                  ,http_verb = Method
                  }, EventId, URI, ReqHeaders, ReqBody) ->
    Headers = kz_json:from_list(
                [{fix_value(K), fix_value(V)}
                 || {K, V} <- ReqHeaders
                ]),
    [{<<"hook_id">>, HookId}
    ,{<<"event_id">>, EventId}
    ,{<<"uri">>, kz_term:to_binary(URI)}
    ,{<<"method">>, kz_term:to_binary(Method)}
    ,{<<"req_headers">>, Headers}
    ,{<<"req_body">>, ReqBody}
    ].

-spec debug_resp(kz_http:ret(), kz_proplist(), hook_retries() | 'undefined') ->
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

-spec fix_value(number() | list()) -> number() | ne_binary().
fix_value(N) when is_number(N) -> N;
fix_value(O) -> kz_term:to_lower_binary(O).

-spec fix_error_value(atom() | {atom(), atom()}) -> ne_binary().
fix_error_value({E, R}) ->
    <<(kz_term:to_binary(E))/binary
      ,": "
      ,(kz_term:to_binary(R))/binary
    >>;
fix_error_value(E) ->
    kz_term:to_binary(E).

-spec hook_id(kz_json:object()) -> ne_binary().
-spec hook_id(ne_binary(), ne_binary()) -> ne_binary().
hook_id(JObj) ->
    hook_id(kz_json:get_first_defined([<<"pvt_account_id">>
                                      ,<<"Account-ID">>
                                      ]
                                     ,JObj
                                     )
           ,kz_json:get_first_defined([<<"_id">>, <<"ID">>], JObj)
           ).

hook_id(AccountId, Id) ->
    <<AccountId/binary, ".", Id/binary>>.

-spec hook_event(ne_binary()) -> ne_binary().
-spec hook_event_lowered(ne_binary()) -> ne_binary().
hook_event(Bin) -> hook_event_lowered(kz_term:to_lower_binary(Bin)).

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
            load_hooks(Srv, WebHooks);
        {'error', 'not_found'} ->
            lager:debug("db or view not found, initing"),
            init_webhook_db(),
            load_hooks(Srv);
        {'error', _E} ->
            lager:debug("failed to load webhooks: ~p", [_E])
    end.

-spec init_webhook_db() -> 'ok'.
init_webhook_db() ->
    _ = kz_datamgr:db_create(?KZ_WEBHOOKS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),
    'ok'.

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
            }.

-spec init_webhooks() -> 'ok'.
-spec init_webhooks(kz_json:objects()) -> 'ok'.
-spec init_webhooks(kz_json:objects(), kz_year(), kz_month()) -> 'ok'.
init_webhooks() ->
    case kz_datamgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'group_level', 1}]
                               )
    of
        {'ok', []} -> lager:debug("no accounts to load views into the MODs");
        {'ok', Accts} -> init_webhooks(Accts);
        {'error', _E} -> lager:debug("failed to load accounts_listing: ~p", [_E])
    end.

init_webhooks(Accts) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(kz_time:now_s()),
    init_webhooks(Accts, Year, Month).

init_webhooks(Accts, Year, Month) ->
    _ = [init_webhook(Acct, Year, Month) || Acct <- Accts],
    'ok'.

-spec init_webhook(kz_json:object(), kz_year(), kz_month()) -> 'ok'.
init_webhook(Acct, Year, Month) ->
    Db = kz_util:format_account_id(kz_json:get_value(<<"key">>, Acct), Year, Month),
    kazoo_modb:maybe_create(Db),
    lager:debug("updated account_modb ~s", [Db]).

-spec note_failed_attempt(ne_binary(), ne_binary()) -> 'ok'.
note_failed_attempt(AccountId, HookId) ->
    kz_cache:store_local(?CACHE_NAME
                        ,?FAILURE_CACHE_KEY(AccountId, HookId, kz_time:now_s())
                        ,'true'
                        ,[{'expires', account_expires_time(AccountId)}]
                        ).

-spec account_expires_time(ne_binary()) -> pos_integer().
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

-spec reenable(ne_binary(), ne_binary()) -> 'ok'.
reenable(AccountId, <<"account">>) ->
    enable_account_hooks(AccountId);
reenable(AccountId, <<"descendants">>) ->
    enable_descendant_hooks(AccountId).

-spec enable_account_hooks(ne_binary()) -> 'ok'.
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

-spec enable_descendant_hooks(ne_binary()) -> 'ok'.
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

-spec maybe_enable_descendants_hooks(ne_binaries()) -> 'ok'.
maybe_enable_descendants_hooks(Accounts) ->
    lists:foreach(fun maybe_enable_descendant_hooks/1, Accounts).

-spec maybe_enable_descendant_hooks(ne_binary()) -> 'ok'.
maybe_enable_descendant_hooks(Account) ->
    io:format("## checking account ~s for hooks to enable ##~n", [Account]),
    enable_account_hooks(Account).

-spec init_metadata(ne_binary(), kz_json:object()) -> 'ok'.
init_metadata(Id, JObj) ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} -> init_metadata(Id, JObj, MasterAccountDb);
        _ -> lager:warning("master account not available")
    end.

-spec init_metadata(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
init_metadata(Id, JObj, MasterAccountDb) ->
    case metadata_exists(MasterAccountDb, Id) of
        {'error', _} -> load_metadata(MasterAccountDb, JObj);
        {'ok', Doc} ->
            lager:debug("~s already exists, updating", [Id]),
            Merged = kz_json:merge(Doc, JObj),
            load_metadata(MasterAccountDb, Merged)
    end.

-spec metadata_exists(ne_binary(), ne_binary()) ->
                             {'ok', kz_json:object()} |
                             kz_datamgr:data_error().
metadata_exists(MasterAccountDb, Id) ->
    kz_datamgr:open_doc(MasterAccountDb, Id).

-spec load_metadata(ne_binary(), kz_json:object()) -> 'ok'.
load_metadata(MasterAccountDb, JObj) ->
    Metadata = update_metadata(MasterAccountDb, JObj),
    case kz_datamgr:save_doc(MasterAccountDb, Metadata) of
        {'ok', _Saved} ->
            lager:debug("~s initialized successfully", [kz_doc:id(JObj)]);
        {'error', 'conflict'} ->
            lager:debug("~s loaded elsewhere", [kz_doc:id(JObj)]);
        {'error', _E} ->
            lager:warning("failed to load metadata for ~s: ~p"
                         ,[kz_doc:id(JObj), _E]
                         )
    end.

-define(AVAILABLE_EVENT_KEY, 'available_events').
-spec available_events() -> ne_binaries().
available_events() ->
    case kz_cache:fetch_local(?CACHE_NAME, ?AVAILABLE_EVENT_KEY) of
        {'error', 'not_found'} ->
            fetch_available_events();
        {'ok', Events} ->
            Events
    end.

-spec fetch_available_events() -> ne_binaries().
fetch_available_events() ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    View = <<"webhooks/webhook_meta_listing">>,
    case kz_datamgr:get_all_results(MasterAccountDb, View) of
        {'ok', []} -> [];
        {'error', _} -> [];
        {'ok', Available} ->
            Events = [kz_json:get_value(<<"key">>, A) || A <- Available],
            CacheProps = [{'origin', [{'db', MasterAccountDb, <<"webhook_meta">>}]}],
            kz_cache:store_local(?CACHE_NAME, ?AVAILABLE_EVENT_KEY, Events, CacheProps),
            Events
    end.

-spec update_metadata(ne_binary(), kz_json:object()) ->
                             kz_json:object().
update_metadata(MasterAccountDb, JObj) ->
    kz_doc:update_pvt_parameters(JObj
                                ,MasterAccountDb
                                ,[{'type', <<"webhook_meta">>}]
                                ).
