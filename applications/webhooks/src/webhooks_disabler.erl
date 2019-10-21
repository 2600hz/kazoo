%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_disabler).
-behaviour(gen_server).

-export([start_link/0
        ,check_failed_attempts/0
        ,find_failures/0
        ,flush_failures/1, flush_failures/2, flush_hooks/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("webhooks.hrl").

-type state() :: reference().

-define(SERVER, ?MODULE).

-define(EXPIRY_MSG, 'failure_check').

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

-spec init(any()) -> {'ok', state()}.
init(_) ->
    kz_log:put_callid(?MODULE),
    {'ok', start_check_timer()}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', Ref, ?EXPIRY_MSG}, Ref) ->
    _ = kz_process:spawn(fun check_failed_attempts/0),
    {'noreply', start_check_timer()};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("disabler terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec start_check_timer() -> 'disabled' | reference().
start_check_timer() ->
    case kapps_config:get_is_true(?APP_NAME, <<"disable_disabler">>, 'false') of
        'true' -> 'disabled';
        'false' ->
            Expiry = webhooks_util:system_expires_time() div 4,
            erlang:start_timer(Expiry, self(), ?EXPIRY_MSG)
    end.

-spec check_failed_attempts() -> 'ok'.
check_failed_attempts() ->
    Failures = find_failures(),
    check_failures(Failures).

-type failure() :: {{kz_term:ne_binary(), kz_term:ne_binary()}, integer()}.
-type failures() :: [failure()].

-spec find_failures() -> failures().
find_failures() ->
    Keys = kz_cache:fetch_keys_local(?CACHE_NAME),
    find_failures(Keys).

-spec flush_hooks(kz_json:objects()) -> non_neg_integer().
flush_hooks(HookJObjs) ->
    lists:sum(
      [flush_failures(kz_doc:account_id(HookJObj)
                     ,kz_doc:id(HookJObj)
                     )
       || HookJObj <- HookJObjs
      ]
     ).

-spec flush_failures(kz_term:ne_binary()) -> non_neg_integer().
flush_failures(AccountId) ->
    flush_failures(AccountId, 'undefined').

-spec flush_failures(kz_term:ne_binary(), kz_term:api_binary()) -> non_neg_integer().
flush_failures(AccountId, HookId) ->
    FilterFun = fun(K, _V) ->
                        maybe_remove_failure(K, AccountId, HookId)
                end,
    kz_cache:filter_erase_local(?CACHE_NAME, FilterFun).

-spec maybe_remove_failure(tuple(), kz_term:ne_binary(), kz_term:api_binary()) -> boolean().
maybe_remove_failure(?FAILURE_CACHE_KEY(AccountId, HookId, _Timestamp)
                    ,AccountId
                    ,HookId
                    ) ->
    'true';
maybe_remove_failure(?FAILURE_CACHE_KEY(AccountId, _HookId, _Timestamp)
                    ,AccountId
                    ,'undefined'
                    ) ->
    'true';
maybe_remove_failure(_K, _AccountId, _HookId) ->
    'false'.

-spec find_failures([tuple()]) -> failures().
find_failures(Keys) ->
    NowMs = kz_time:now_ms(),
    {NowMs, Dict} = lists:foldl(fun process_failed_key/2, {NowMs, dict:new()}, Keys),
    dict:to_list(Dict).

-spec process_failed_key(tuple(), {kz_time:gregorian_seconds(), dict:dict()}) -> {kz_time:gregorian_seconds(), dict:dict()}.
process_failed_key(?FAILURE_CACHE_KEY(AccountId, HookId, ThenMs)
                  ,{NowMs, Dict}
                  ) ->
    ExpiryMs = webhooks_util:account_expires_time(AccountId),
    case ThenMs > (NowMs - ExpiryMs) of
        'true' ->
            %% If the failure happens within the window, count it
            {NowMs, dict:update_counter({AccountId, HookId}, 1, Dict)};
        'false' ->
            {NowMs, Dict}
    end;
process_failed_key(_Key, Acc) ->
    Acc.

-spec check_failures(failures()) -> 'ok'.
check_failures(Failures) ->
    _ = [check_failure(AccountId, HookId, Count)
         || {{AccountId, HookId}, Count} <- Failures
        ],
    'ok'.

-spec check_failure(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer()) -> 'ok'.
check_failure(AccountId, HookId, Count) ->
    try webhooks_util:account_failure_count(AccountId) of
        N when N =< Count -> disable_hook(AccountId, HookId);
        _ -> 'ok'
    catch
        _:_ ->
            lager:warning("account ~s has an non-integer for ~s/~s", [AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY]),
            SystemCount = webhooks_util:system_failure_count(),
            case Count > SystemCount of
                'false' -> 'ok';
                'true' -> disable_hook(AccountId, HookId)
            end
    end.

-spec disable_hook(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
disable_hook(AccountId, HookId) ->
    Update = kzd_webhooks:disable_updates(<<"too many failed attempts">>),
    Updates = [{'update', Update}
              ,{'ensure_saved', 'true'}
              ,{'should_create', 'false'}
              ],
    case kz_datamgr:update_doc(?KZ_WEBHOOKS_DB, HookId, Updates) of
        {'ok', _Updated} ->
            filter_cache(AccountId, HookId),
            send_notification(AccountId, HookId),
            lager:debug("auto-disabled and saved hook ~s/~s", [AccountId, HookId]);
        {'error', _E} ->
            lager:info("failed to auto-disable hook ~s/~s: ~p", [AccountId, HookId, _E])
    end.

-spec filter_cache(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
filter_cache(AccountId, HookId) ->
    kz_cache:filter_erase_local(?CACHE_NAME
                               ,fun(?FAILURE_CACHE_KEY(A, H, _T), _) when A =:= AccountId,
                                                                          H =:= HookId
                                                                          ->
                                        lager:debug("filtering ~s / ~s (~p)", [H, A, _T]),
                                        'true';
                                   (_K, _V) -> 'false'
                                end
                               ).

-spec send_notification(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_notification(AccountId, HookId) ->
    API = [{<<"Account-ID">>, AccountId}
          ,{<<"Hook-ID">>, HookId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(API, fun kapi_notifications:publish_webhook_disabled/1).
