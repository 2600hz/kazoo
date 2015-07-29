%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_disabler).

-behaviour(gen_server).

-export([start_link/0
         ,check_failed_attempts/0
         ,find_failures/0
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("webhooks.hrl").

-define(EXPIRY_MSG, 'failure_check').

-spec start_link() -> 'ignore'.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(_) -> {'ok', reference()}.
init(_) ->
    wh_util:put_callid(?MODULE),
    {'ok', start_check_timer()}.

handle_call(_Request, _From, State) ->
    {'noreply', State}.

handle_cast(_Msg, State) ->
    {'noreply', State}.

handle_info({'timeout', Ref, ?EXPIRY_MSG}, Ref) ->
    _ = wh_util:spawn(?MODULE, 'check_failed_attempts', []),
    {'noreply', start_check_timer()};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

terminate(_Reason, _State) ->
    lager:debug("disabler terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

start_check_timer() ->
    Expiry = webhooks_util:system_expires_time(),
    erlang:start_timer(Expiry, self(), ?EXPIRY_MSG).

-spec check_failed_attempts() -> 'ok'.
check_failed_attempts() ->
    Failures = find_failures(),
    check_failures(Failures).

-type failure() :: {{ne_binary(), ne_binary()}, integer()}.
-type failures() :: [failure(),...] | [].

-spec find_failures() -> failures().
-spec find_failures([tuple()]) -> failures().
find_failures() ->
    Keys = wh_cache:fetch_keys_local(?CACHE_NAME),
    find_failures(Keys).

find_failures(Keys) ->
    dict:to_list(lists:foldl(fun process_failed_key/2, dict:new(), Keys)).

-spec process_failed_key(tuple(), dict()) -> dict().
process_failed_key(?FAILURE_CACHE_KEY(AccountId, HookId, _Timestamp)
                   ,Dict
                  ) ->
    dict:update_counter({AccountId, HookId}, 1, Dict);
process_failed_key(_Key, Dict) ->
    Dict.

-spec check_failures(failures()) -> 'ok'.
check_failures(Failures) ->
    _ = [check_failure(AccountId, HookId, Count)
         || {{AccountId, HookId}, Count} <- Failures
        ],
    'ok'.

-spec check_failure(ne_binary(), ne_binary(), pos_integer()) -> 'ok'.
check_failure(AccountId, HookId, Count) ->
    try wh_util:to_integer(whapps_account_config:get_global(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, 6)) of
        N when N =< Count ->
            disable_hook(AccountId, HookId);
        _ -> 'ok'
    catch
        _:_ ->
            lager:warning("account ~s has an non-integer for ~s/~s", [AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY]),
            case Count > 6 of
                'true' ->
                    disable_hook(AccountId, HookId);
                'false' -> 'ok'
            end
    end.

-spec disable_hook(ne_binary(), ne_binary()) -> 'ok'.
disable_hook(AccountId, HookId) ->
    case couch_mgr:open_cache_doc(?KZ_WEBHOOKS_DB, HookId) of
        {'ok', HookJObj} ->
            Disabled = kzd_webhook:disable(HookJObj, <<"too many failed attempts">>),
            _ = couch_mgr:ensure_saved(?KZ_WEBHOOKS_DB, Disabled),
            filter_cache(AccountId, HookId),
            send_notification(AccountId, HookId),
            lager:debug("auto-disabled and saved hook ~s/~s", [AccountId, HookId]);
        {'error', _E} ->
            lager:debug("failed to find ~s/~s to disable: ~p", [AccountId, HookId, _E])
    end.

-spec filter_cache(ne_binary(), ne_binary()) -> non_neg_integer().
filter_cache(AccountId, HookId) ->
    wh_cache:filter_erase_local(?CACHE_NAME
                                ,fun(?FAILURE_CACHE_KEY(A, H, _), _) ->
                                         lager:debug("maybe remove ~s/~s", [A, H]),
                                         A =:= AccountId andalso H =:= HookId;
                                    (_K, _V) -> 'false'
                                 end
                               ).

-spec send_notification(ne_binary(), ne_binary()) -> 'ok'.
send_notification(AccountId, HookId) ->
    API = [{<<"Account-ID">>, AccountId}
           ,{<<"Hook-ID">>, HookId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:cast(API, fun wapi_notifications:publish_webhook_disabled/1).
