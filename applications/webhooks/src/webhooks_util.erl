%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
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
        ,[{"X-Hook-ID", Hook#webhook.hook_id}
          ,{"X-Account-ID", Hook#webhook.account_id}
         ]).

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
to_json(Hook) ->
    kz_json:from_list(
      [{<<"_id">>, Hook#webhook.id}
       ,{<<"uri">>, Hook#webhook.uri}
       ,{<<"http_verb">>, Hook#webhook.http_verb}
       ,{<<"retries">>, Hook#webhook.retries}
       ,{<<"account_id">>, Hook#webhook.account_id}
       ,{<<"custom_data">>, Hook#webhook.custom_data}
       ,{<<"modifiers">>, Hook#webhook.modifiers}
      ]).

-spec find_webhooks(ne_binary(), api_binary()) -> webhooks().
find_webhooks(_HookEvent, 'undefined') -> [];
find_webhooks(HookEvent, AccountId) ->
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
-spec fire_hook(kz_json:object(), webhook(), string(), http_verb(), 0 | hook_retries()) -> 'ok'.
fire_hook(JObj, #webhook{uri=URI
                         ,http_verb=Method
                         ,retries=Retries
                         ,custom_data='undefined'
                        }=Hook) ->
    fire_hook(JObj
              ,Hook
              ,kz_util:to_list(URI)
              ,Method
              ,Retries
             );
fire_hook(JObj, #webhook{uri=URI
                         ,http_verb=Method
                         ,retries=Retries
                         ,custom_data=CustomData
                        }=Hook) ->
    fire_hook(kz_json:merge_jobjs(CustomData, JObj)
              ,Hook
              ,kz_util:to_list(URI)
              ,Method
              ,Retries
             ).

fire_hook(_JObj, Hook, _URI, _Method, 0) ->
    failed_hook(Hook),
    lager:debug("retries exhausted for ~s", [_URI]);
fire_hook(JObj, Hook, URI, 'get', Retries) ->
    lager:debug("sending event via 'get'(~b): ~s", [Retries, URI]),
    Fired = kz_http:get(URI ++ [$?|kz_json:to_querystring(JObj)], ?HTTP_REQ_HEADERS(Hook), ?HTTP_OPTS),
    fire_hook(JObj, Hook, URI, 'get', Retries, Fired);
fire_hook(JObj, Hook, URI, 'post', Retries) ->
    lager:debug("sending event via 'post'(~b): ~s", [Retries, URI]),
    Fired = kz_http:post(URI
                        ,[{"Content-Type", "application/x-www-form-urlencoded"}
                          | ?HTTP_REQ_HEADERS(Hook)
                         ]
                        ,kz_json:to_querystring(JObj)
                        ,?HTTP_OPTS
                        ),

    fire_hook(JObj, Hook, URI, 'post', Retries, Fired).

-spec fire_hook(kz_json:object(), webhook(), string(), http_verb(), hook_retries(), kz_http:ret()) -> 'ok'.
fire_hook(_JObj, Hook, _URI, _Method, _Retries, {'ok', 200, _, _RespBody}) ->
    lager:debug("sent hook call event successfully"),
    successful_hook(Hook);
fire_hook(_JObj, Hook, _URI, _Method, Retries, {'ok', RespCode, _, RespBody}) ->
    _ = failed_hook(Hook, Retries, integer_to_binary(RespCode), RespBody),
    lager:debug("non-200 response code: ~p on account ~s", [RespCode, Hook#webhook.account_id]);
fire_hook(JObj, Hook, URI, Method, Retries, {'error', E}) ->
    lager:debug("failed to fire hook: ~p", [E]),
    _ = failed_hook(Hook, Retries, E),
    retry_hook(JObj, Hook, URI, Method, Retries).

-spec retry_hook(kz_json:object(), webhook(), string(), http_verb(), hook_retries()) -> 'ok'.
retry_hook(JObj, Hook, URI, Method, Retries) ->
    timer:sleep(2000),
    fire_hook(JObj, Hook, URI, Method, Retries-1).

-spec successful_hook(webhook()) -> 'ok'.
-spec successful_hook(webhook(), boolean()) -> 'ok'.
successful_hook(Hook) ->
    successful_hook(Hook, kapps_config:get_is_true(?APP_NAME, <<"log_successful_attempts">>, 'false')).

successful_hook(_Hook, 'false') -> 'ok';
successful_hook(#webhook{hook_id=HookId
                         ,account_id=AccountId
                        }
                ,'true'
               ) ->
    Attempt = kz_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"success">>}
                                ]),
    save_attempt(Attempt, AccountId).

-spec failed_hook(webhook()) -> 'ok'.
-spec failed_hook(webhook(), hook_retries(), any()) -> 'ok'.
-spec failed_hook(webhook(), hook_retries(), ne_binary(), binary()) -> 'ok'.
failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }) ->
    note_failed_attempt(AccountId, HookId),
    Attempt = kz_json:from_list(
                [{<<"hook_id">>, HookId}
                 ,{<<"result">>, <<"failure">>}
                 ,{<<"reason">>, <<"retries exceeded">>}
                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries
            ,RespCode
            ,RespBody
           ) ->
    note_failed_attempt(AccountId, HookId),
    Attempt = kz_json:from_list(
                [{<<"hook_id">>, HookId}
                 ,{<<"result">>, <<"failure">>}
                 ,{<<"reason">>, <<"bad response code">>}
                 ,{<<"response_code">>, RespCode}
                 ,{<<"response_body">>, RespBody}
                 ,{<<"retries_left">>, Retries - 1}
                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries
            ,E
           ) ->
    note_failed_attempt(AccountId, HookId),
    Error = try kz_util:to_binary(E) of
                Bin -> Bin
            catch
                _E:_R ->
                    lager:debug("failed to convert error ~p", [E]),
                    <<"unknown">>
            end,
    Attempt = kz_json:from_list(
                [{<<"hook_id">>, HookId}
                 ,{<<"result">>, <<"failure">>}
                 ,{<<"reason">>, <<"kazoo http client error">>}
                 ,{<<"retries_left">>, Retries - 1}
                 ,{<<"client_error">>, Error}
                ]),
    save_attempt(Attempt, AccountId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec save_attempt(kz_json:object(), api_binary()) -> 'ok'.
save_attempt(Attempt, AccountId) ->
    Now = kz_util:current_tstamp(),
    ModDb = kz_util:format_account_mod_id(AccountId, Now),

    Doc = kz_json:set_values(
            props:filter_undefined(
              [{<<"pvt_account_db">>, ModDb}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_type">>, <<"webhook_attempt">>}
               ,{<<"pvt_created">>, Now}
               ,{<<"pvt_modified">>, Now}
              ])
            ,Attempt
           ),

    _ = kz_datamgr:save_doc(ModDb, Doc, [{'publish_change_notice', 'false'}]),
    'ok'.

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
hook_event(Bin) -> hook_event_lowered(kz_util:to_lower_binary(Bin)).

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
             ,http_verb = kzd_webhook:verb(Hook)
             ,hook_event = hook_event(kzd_webhook:event(Hook))
             ,hook_id = kz_doc:id(Hook)
             ,retries = kzd_webhook:retries(Hook)
             ,account_id = kz_doc:account_id(Hook)
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
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(kz_util:current_tstamp()),
    init_webhooks(Accts, Year, Month).

init_webhooks(Accts, Year, Month) ->
    _ = [init_webhook(Acct, Year, Month) || Acct <- Accts],
    'ok'.

-spec init_webhook(kz_json:object(), kz_year(), kz_month()) -> 'ok'.
init_webhook(Acct, Year, Month) ->
    Db = kz_util:format_account_id(kz_json:get_value(<<"key">>, Acct), Year, Month),
    kazoo_modb:create(Db),
    lager:debug("updated account_mod ~s", [Db]).

-spec note_failed_attempt(ne_binary(), ne_binary()) -> 'ok'.
note_failed_attempt(AccountId, HookId) ->
    kz_cache:store_local(?CACHE_NAME
                         ,?FAILURE_CACHE_KEY(AccountId, HookId, kz_util:current_tstamp())
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
    try kz_util:to_integer(Expiry) of
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
            Ids = kz_util:join_binary([kz_doc:id(D) || D <- Saved], <<", ">>),
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
    _ = [maybe_enable_descendant_hooks(Account) || Account <- Accounts],
    'ok'.

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
            Merged = kz_json:merge_recursive(Doc, JObj),
            load_metadata(MasterAccountDb, Merged)
    end.

-spec metadata_exists(ne_binary(), ne_binary()) ->
                             {'ok', kz_json:object()} |
                             kz_data:data_error().
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
