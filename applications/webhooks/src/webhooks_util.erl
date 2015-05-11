%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
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
-export([init_mods/0
         ,jobj_to_rec/1
         ,hook_event_lowered/1
         ,hook_event/1
         ,load_hooks/1
         ,load_hook/2
         ,hook_id/1
         ,hook_id/2

         ,account_expires_time/1
         ,system_expires_time/0
        ]).

%% ETS Management
-export([table_id/0
         ,table_options/0
         ,gift_data/0
        ]).

-include("webhooks.hrl").

-define(TABLE, 'webhooks_listener').

-define(CONNECT_TIMEOUT_MS
        ,whapps_config:get_integer(?APP_NAME, <<"connect_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).
-define(IBROWSE_OPTS, [{'connect_timeout', ?CONNECT_TIMEOUT_MS}
                       ,{'response_format', 'binary'}
                      ]).

-define(IBROWSE_TIMEOUT_MS
        ,whapps_config:get_integer(?APP_NAME, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND)
       ).

-define(IBROWSE_REQ_HEADERS(Hook)
        ,[{<<"X-Hook-ID">>, Hook#webhook.hook_id}
          ,{<<"X-Account-ID">>, Hook#webhook.account_id}
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

-spec from_json(wh_json:object()) -> webhook().
from_json(Hook) ->
    #webhook{id = hook_id(Hook)
             ,uri = wh_json:get_value(<<"uri">>, Hook)
             ,http_verb = http_verb(wh_json:get_value(<<"http_verb">>, Hook))
             ,hook_event = hook_event(wh_json:get_value(<<"hook">>, Hook))
             ,hook_id = wh_json:get_first_defined([<<"_id">>, <<"ID">>], Hook)
             ,retries = retries(wh_json:get_integer_value(<<"retries">>, Hook, 3))
             ,account_id = wh_json:get_value(<<"pvt_account_id">>, Hook)
             ,custom_data = wh_json:get_ne_value(<<"custom_data">>, Hook)
            }.

-spec to_json(webhook()) -> wh_json:object().
to_json(Hook) ->
    wh_json:from_list(
      [{<<"_id">>, Hook#webhook.id}
       ,{<<"uri">>, Hook#webhook.uri}
       ,{<<"http_verb">>, Hook#webhook.http_verb}
       ,{<<"retries">>, Hook#webhook.retries}
       ,{<<"account_id">>, Hook#webhook.account_id}
       ,{<<"custom_data">>, Hook#webhook.custom_data}
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

-spec fire_hooks(wh_json:object(), webhooks()) -> 'ok'.
fire_hooks(_, []) -> 'ok';
fire_hooks(JObj, [Hook | Hooks]) ->
    fire_hook(JObj, Hook),
    fire_hooks(JObj, Hooks).

-spec fire_hook(wh_json:object(), webhook()) -> 'ok'.
-spec fire_hook(wh_json:object(), webhook(), string(), http_verb(), 0 | hook_retries()) -> 'ok'.
fire_hook(JObj, #webhook{uri=URI
                         ,http_verb=Method
                         ,retries=Retries
                         ,custom_data='undefined'
                        }=Hook) ->
    fire_hook(JObj, Hook, wh_util:to_list(URI), Method, Retries);
fire_hook(JObj, #webhook{uri=URI
                         ,http_verb=Method
                         ,retries=Retries
                         ,custom_data=CustomData
                        }=Hook) ->
    fire_hook(wh_json:merge_jobjs(CustomData, JObj)
              ,Hook
              ,wh_util:to_list(URI)
              ,Method
              ,Retries).

fire_hook(_JObj, Hook, _URI, _Method, 0) ->
    failed_hook(Hook),
    lager:debug("retries exhausted for ~s", [_URI]);
fire_hook(JObj, Hook, URI, 'get', Retries) ->
    lager:debug("sending event via 'get'(~b): ~s", [Retries, URI]),
    fire_hook(JObj, Hook, URI, 'get', Retries
              ,ibrowse:send_req(URI ++ [$?|wh_json:to_querystring(JObj)]
                                ,?IBROWSE_REQ_HEADERS(Hook)
                                ,'get'
                                ,[]
                                ,?IBROWSE_OPTS
                                ,?IBROWSE_TIMEOUT_MS
                               ));
fire_hook(JObj, Hook, URI, 'post', Retries) ->
    lager:debug("sending event via 'post'(~b): ~s", [Retries, URI]),

    fire_hook(JObj, Hook, URI, 'post', Retries
              ,ibrowse:send_req(URI
                                ,[{"Content-Type", "application/x-www-form-urlencoded"}
                                  | ?IBROWSE_REQ_HEADERS(Hook)
                                 ]
                                ,'post'
                                ,wh_json:to_querystring(JObj)
                                ,?IBROWSE_OPTS
                                ,?IBROWSE_TIMEOUT_MS
                               )).

-spec fire_hook(wh_json:object(), webhook(), string(), http_verb(), hook_retries(), ibrowse_ret()) -> 'ok'.
fire_hook(_JObj, Hook, _URI, _Method, _Retries, {'ok', "200", _, _RespBody}) ->
    lager:debug("sent hook call event successfully"),
    successful_hook(Hook);
fire_hook(_JObj, Hook, _URI, _Method, Retries, {'ok', RespCode, _, RespBody}) ->
    _ = failed_hook(Hook, Retries, RespCode, RespBody),
    lager:debug("non-200 response code: ~s", [RespCode]);
fire_hook(JObj, Hook, URI, Method, Retries, {'error', 'req_timedout'}) ->
    lager:debug("request timed out to ~s, retrying", [URI]),
    _ = failed_hook(Hook, Retries, <<"request_timed_out">>),
    retry_hook(JObj, Hook, URI, Method, Retries);
fire_hook(_JObj, Hook, URI, _Method, Retries, {'error', 'retry_later'}) ->
    lager:debug("failed with 'retry_later' to ~s", [URI]),
    _ = failed_hook(Hook, Retries, <<"retry_later">>),
    'ok';
fire_hook(_JObj, Hook, URI, _Method, Retries, {'error', {'conn_failed', {'error', E}}}) ->
    lager:debug("connection failed with ~p to ~s", [E, URI]),
    _ = failed_hook(Hook, Retries, wh_util:to_binary(E)),
    'ok';
fire_hook(JObj, Hook, URI, Method, Retries, {'error', E}) ->
    lager:debug("failed to fire hook: ~p", [E]),
    _ = failed_hook(Hook, Retries, E),
    retry_hook(JObj, Hook, URI, Method, Retries).

-spec retry_hook(wh_json:object(), webhook(), string(), http_verb(), hook_retries()) -> 'ok'.
retry_hook(JObj, Hook, URI, Method, Retries) ->
    timer:sleep(2000),
    fire_hook(JObj, Hook, URI, Method, Retries-1).

-spec successful_hook(webhook()) -> 'ok'.
successful_hook(#webhook{hook_id=HookId
                         ,account_id=AccountId
                        }) ->
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"success">>}
                                ]),
    save_attempt(Attempt, AccountId).

-spec failed_hook(webhook()) -> 'ok'.
-spec failed_hook(webhook(), hook_retries(), term()) -> 'ok'.
-spec failed_hook(webhook(), hook_retries(), string(), binary()) -> 'ok'.
failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }) ->
    note_failed_attempt(AccountId, HookId),
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"failure">>}
                                 ,{<<"reason">>, <<"retries exceeded">>}
                                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries, RespCode, RespBody) ->
    note_failed_attempt(AccountId, HookId),
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"failure">>}
                                 ,{<<"reason">>, <<"bad response code">>}
                                 ,{<<"response_code">>, wh_util:to_binary(RespCode)}
                                 ,{<<"response_body">>, wh_util:to_binary(RespBody)}
                                 ,{<<"retries left">>, Retries-1}
                                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries, E) ->
    note_failed_attempt(AccountId, HookId),
    Error = try wh_util:to_binary(E) of
                Bin -> Bin
            catch
                _E:_R ->
                    lager:debug("failed to convert error ~p", [E]),
                    <<"unknown">>
            end,
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"failure">>}
                                 ,{<<"reason">>, <<"kazoo http client error">>}
                                 ,{<<"retries left">>, Retries-1}
                                 ,{<<"client_error">>, Error}
                                ]),
    save_attempt(Attempt, AccountId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec save_attempt(wh_json:object(), api_binary()) -> 'ok'.
save_attempt(Attempt, AccountId) ->
    Now = wh_util:current_tstamp(),
    ModDb = wh_util:format_account_mod_id(AccountId, Now),

    Doc = wh_json:set_values(
            props:filter_undefined(
              [{<<"pvt_account_db">>, ModDb}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_type">>, <<"webhook_attempt">>}
               ,{<<"pvt_created">>, Now}
               ,{<<"pvt_modified">>, Now}
              ]), Attempt),

    _ = couch_mgr:save_doc(ModDb, Doc),
    'ok'.

-spec hook_id(wh_json:object()) -> ne_binary().
-spec hook_id(ne_binary(), ne_binary()) -> ne_binary().
hook_id(JObj) ->
    hook_id(wh_json:get_first_defined([<<"pvt_account_id">>
                                       ,<<"Account-ID">>
                                      ], JObj)
            ,wh_json:get_first_defined([<<"_id">>, <<"ID">>], JObj)
           ).
hook_id(AccountId, Id) ->
    <<AccountId/binary, ".", Id/binary>>.

-spec http_verb(atom() | binary()) -> http_verb().
http_verb('get') -> 'get';
http_verb('post') -> 'post';
http_verb(Bin) when is_binary(Bin) ->
    try wh_util:to_atom(wh_util:to_lower_binary(Bin)) of
        Atom -> http_verb(Atom)
    catch
        'error':'badarg' -> 'get'
    end;
http_verb(_) -> 'get'.

-spec hook_event(ne_binary()) -> ne_binary().
-spec hook_event_lowered(ne_binary()) -> ne_binary().
hook_event(Bin) -> hook_event_lowered(wh_util:to_lower_binary(Bin)).

hook_event_lowered(<<"channel_create">>) -> <<"CHANNEL_CREATE">>;
hook_event_lowered(<<"channel_answer">>) -> <<"CHANNEL_ANSWER">>;
hook_event_lowered(<<"channel_destroy">>) -> <<"CHANNEL_DESTROY">>;
hook_event_lowered(<<"all">>) -> <<"all">>;
hook_event_lowered(Bin) -> Bin.

-spec retries(integer()) -> hook_retries().
retries(N) when N > 0, N < 5 -> N;
retries(N) when N < 1 -> 1;
retries(_) -> 5.

-spec load_hooks(pid()) -> 'ok'.
load_hooks(Srv) ->
    lager:debug("loading hooks into memory"),
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
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
            init_webhooks(),
            load_hooks(Srv);
        {'error', _E} ->
            lager:debug("failed to load webhooks: ~p", [_E])
    end.

-spec init_webhooks() -> 'ok'.
init_webhooks() ->
    _ = couch_mgr:db_create(?KZ_WEBHOOKS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = couch_mgr:revise_doc_from_file(?WH_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),
    'ok'.

-spec load_hooks(pid(), wh_json:objects()) -> 'ok'.
load_hooks(Srv, WebHooks) ->
    [load_hook(Srv, wh_json:get_value(<<"doc">>, Hook)) || Hook <- WebHooks],
    lager:debug("sent hooks into server ~p", [Srv]).

-spec load_hook(pid(), wh_json:object()) -> 'ok'.
load_hook(Srv, WebHook) ->
    try jobj_to_rec(WebHook) of
        Hook -> gen_listener:cast(Srv, {'add_hook', Hook})
    catch
        'throw':{'bad_hook', HookEvent} ->
            lager:debug("failed to load hook ~s.~s: bad_hook: ~s"
                        ,[wh_json:get_value(<<"pvt_account_id">>, WebHook)
                          ,wh_json:get_value(<<"_id">>, WebHook)
                          ,HookEvent
                         ])
    end.

-spec jobj_to_rec(wh_json:object()) -> webhook().
jobj_to_rec(Hook) ->
    #webhook{id = hook_id(Hook)
             ,uri = wh_json:get_value(<<"uri">>, Hook)
             ,http_verb = http_verb(wh_json:get_value(<<"http_verb">>, Hook))
             ,hook_event = hook_event(wh_json:get_value(<<"hook">>, Hook))
             ,hook_id = wh_json:get_first_defined([<<"_id">>, <<"ID">>], Hook)
             ,retries = retries(wh_json:get_integer_value(<<"retries">>, Hook, 3))
             ,account_id = wh_json:get_value(<<"pvt_account_id">>, Hook)
             ,custom_data = wh_json:get_ne_value(<<"custom_data">>, Hook)
            }.

-spec init_mods() -> 'ok'.
-spec init_mods(wh_json:objects()) -> 'ok'.
init_mods() ->
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/accounts_listing">>
                               ,[{'group_level', 1}]
                              )
    of
        {'ok', []} ->
            lager:debug("no accounts to load views into the MODs");
        {'ok', Accts} ->
            init_mods(Accts);
        {'error', _E} ->
            lager:debug("failed to load accounts_listing: ~p", [_E])
    end.

init_mods(Accts) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),
    init_mods(Accts, Year, Month).
init_mods([], _, _) -> 'ok';
init_mods([Acct|Accts], Year, Month) ->
    init_mod(Acct, Year, Month),
    init_mods(Accts, Year, Month).

-spec init_mod(wh_json:object(), wh_year(), wh_month()) -> 'ok'.
init_mod(Acct, Year, Month) ->
    Db = wh_util:format_account_id(wh_json:get_value(<<"key">>, Acct), Year, Month),
    kazoo_modb:create(Db),
    lager:debug("updated account_mod ~s", [Db]).

-spec note_failed_attempt(ne_binary(), ne_binary()) -> 'ok'.
note_failed_attempt(AccountId, HookId) ->
    wh_cache:store_local(?CACHE_NAME
                         ,?FAILURE_CACHE_KEY(AccountId, HookId, wh_util:current_tstamp())
                         ,'true'
                         ,[{'expires', account_expires_time(AccountId)}]
                        ).

-spec account_expires_time(ne_binary()) -> pos_integer().
account_expires_time(AccountId) ->
    Expiry = whapps_account_config:get_global(AccountId
                                              ,?APP_NAME
                                              ,?ATTEMPT_EXPIRY_KEY
                                              ,?MILLISECONDS_IN_MINUTE
                                             ),
    try wh_util:to_integer(Expiry) of
        I -> I
    catch
        _:_ -> ?MILLISECONDS_IN_MINUTE
    end.

-spec system_expires_time() -> pos_integer().
system_expires_time() ->
    whapps_config:get_integer(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, ?MILLISECONDS_IN_MINUTE).
