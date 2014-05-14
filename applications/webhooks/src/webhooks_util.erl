%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_util).

-include("webhooks.hrl").

-export([find_webhooks/2]).
-export([fire_hooks/2]).

-define(TABLE, 'webhooks_listener').

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
    ets:select(?TABLE, MatchSpec).

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
                                ,[]
                                ,'get'
                                ,[]
                                ,[{'connect_timeout', 1000}]
                                ,1000
                               ));
fire_hook(JObj, Hook, URI, 'post', Retries) ->
    lager:debug("sending event via 'post'(~b): ~s", [Retries, URI]),

    fire_hook(JObj, Hook, URI, 'post', Retries
              ,ibrowse:send_req(URI
                                ,[{"Content-Type", "application/x-www-form-urlencoded"}]
                                ,'post'
                                ,wh_json:to_querystring(JObj)
                                ,[{'connect_timeout', 1000}]
                                ,1000
                               )).

-spec fire_hook(wh_json:object(), webhook(), string(), http_verb(), hook_retries(), ibrowse_ret()) -> 'ok'.
fire_hook(_JObj, Hook, _URI, _Method, _Retries, {'ok', "200", _, _RespBody}) ->
    lager:debug("sent hook event successfully"),
    successful_hook(Hook);
fire_hook(JObj, Hook, URI, Method, Retries, {'ok', RespCode, _, RespBody}) ->
    lager:debug("non-200 response code: ~s", [RespCode]),
    _ = failed_hook(Hook, Retries, RespCode, RespBody),
    fire_hook(JObj, Hook, URI, Method, Retries-1);
fire_hook(JObj, Hook, URI, Method, Retries, {'error', E}) ->
    lager:debug("failed to fire hook: ~p", [E]),
    _ = failed_hook(Hook, Retries, E),
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
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"failure">>}
                                 ,{<<"reason">>, <<"retries exceeded">>}
                                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries, RespCode, RespBody) ->
    Attempt = wh_json:from_list([{<<"hook_id">>, HookId}
                                 ,{<<"result">>, <<"failure">>}
                                 ,{<<"reason">>, <<"bad response code">>}
                                 ,{<<"response_code">>, RespCode}
                                 ,{<<"response_body">>, RespBody}
                                 ,{<<"retries left">>, Retries-1}
                                ]),
    save_attempt(Attempt, AccountId).

failed_hook(#webhook{hook_id=HookId
                     ,account_id=AccountId
                    }
            ,Retries, E) ->
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
