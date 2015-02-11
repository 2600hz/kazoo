%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webhooks_maintenance).

-export([hooks_configured/0, hooks_configured/1
         ,set_failure_expiry/1, set_failure_expiry/2
         ,set_disable_threshold/1, set_disable_threshold/2
        ]).

-include("webhooks.hrl").

-spec hooks_configured() -> 'ok'.
-spec hooks_configured(ne_binary()) -> 'ok'.
hooks_configured() ->
    webhooks_shared_listener:hooks_configured(),
    'ok'.

hooks_configured(AccountId) ->
    webhooks_shared_listener:hooks_configured(AccountId),
    'ok'.

-spec set_failure_expiry(ne_binary()) -> 'ok'.
-spec set_failure_expiry(ne_binary(), ne_binary()) -> 'ok'.
set_failure_expiry(Expires) ->
    try wh_util:to_integer(Expires) of
        I ->
            whapps_config:set_default(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms~n", [I])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

set_failure_expiry(Account, Expires) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    try wh_util:to_integer(Expires) of
        I ->
            whapps_account_config:set(AccountId, ?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms on account ~s~n", [I, AccountId])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

-spec set_disable_threshold(ne_binary()) -> 'ok'.
-spec set_disable_threshold(ne_binary(), ne_binary()) -> 'ok'.
set_disable_threshold(Count) ->
    try wh_util:to_integer(Count) of
        I ->
            whapps_config:set_default(?APP_NAME, ?FAILURE_COUNT_KEY, I),
            io:format("set default count of failed attempts to disable hook to ~p~n", [I])
    catch
        _:_ ->
            io:format("error in count, must be an integer~n")
    end.

set_disable_threshold(Account, Count) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    try wh_util:to_integer(Count) of
        I ->
            whapps_account_config:set(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, I),
            io:format("set default count of failed attempts to disable hook to ~p~n", [I])
    catch
        _:_ ->
            io:format("error in count, must be an integer~n")
    end.
