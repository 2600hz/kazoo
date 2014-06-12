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

-export([from_json/1
         ,to_json/1
        ]).
-export([find_webhooks/2]).
-export([fire_hooks/2]).

-define(TABLE, 'webhooks_listener').

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
    wh_json:from_list([
        {<<"_id">>, Hook#webhook.id}
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
