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
         ,failure_status/0, failure_status/1
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

failure_status() ->
    Failed = webhooks_listener:find_failures(),
    Sorted = lists:keysort(1, Failed),

    print_failure_header(),
    [print_failure_count(AccountId, HookId, Count) || {{AccountId, HookId}, Count} <- Sorted],
    print_failure_footer().

failure_status(Account) ->
    AccountId = wh_util:format_account_id(Account),
    Failed = webhooks_listener:find_failures(),
    Sorted = lists:keysort(1, Failed),

    print_failure_header(),
    [print_failure_count(AID, HookId, Count) || {{AID, HookId}, Count} <- Sorted, AccountId =:= AID],
    print_failure_footer().

-define(FORMAT_FAILURE_STRING, "| ~-32s | ~-32s | ~5s |~n").
-define(FORMAT_FAILURE_HEADER, "| ~32.32c | ~32.32c | ~5.5c |~n").

print_failure_header() ->
    io:format(?FORMAT_FAILURE_HEADER, [$-, $-, $-]),
    io:format(?FORMAT_FAILURE_STRING, [<<"Account">>, <<"Hook">>, <<"Count">>]).

print_failure_footer() ->
    io:format(?FORMAT_FAILURE_HEADER, [$-, $-, $-]).

print_failure_count(AccountId, HookId, Count) ->
    io:format(?FORMAT_FAILURE_STRING, [AccountId, HookId, wh_util:to_binary(Count)]).
