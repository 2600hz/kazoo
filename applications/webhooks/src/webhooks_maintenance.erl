%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_maintenance).

-export([hooks_configured/0, hooks_configured/1
        ,set_failure_expiry/1, set_failure_expiry/2
        ,set_disable_threshold/1, set_disable_threshold/2
        ,failure_status/0, failure_status/1
        ,enable_account_hooks/1
        ,enable_descendant_hooks/1
        ,flush_account_failures/1
        ,flush_hook_failures/2

        ,register_views/0
        ]).

-include("webhooks.hrl").

-spec hooks_configured() -> 'ok'.
hooks_configured() ->
    webhooks_shared_listener:hooks_configured(),
    'ok'.

-spec hooks_configured(kz_term:ne_binary()) -> 'ok'.
hooks_configured(AccountId) ->
    webhooks_shared_listener:hooks_configured(AccountId),
    'ok'.

-spec set_failure_expiry(kz_term:ne_binary()) -> 'ok'.
set_failure_expiry(Expires) ->
    try kz_term:to_integer(Expires) of
        I ->
            kapps_config:set_default(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms~n", [I])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

-spec set_failure_expiry(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_failure_expiry(Account, Expires) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    try kz_term:to_integer(Expires) of
        I ->
            kapps_account_config:set(AccountId, ?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms on account ~s~n", [I, AccountId])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

-spec set_disable_threshold(kz_term:ne_binary()) -> 'ok'.
set_disable_threshold(Count) ->
    try kz_term:to_integer(Count) of
        I ->
            kapps_config:set_default(?APP_NAME, ?FAILURE_COUNT_KEY, I),
            io:format("set default count of failed attempts to disable hook to ~p~n", [I])
    catch
        _:_ ->
            io:format("error in count, must be an integer~n")
    end.

-spec set_disable_threshold(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_disable_threshold(Account, Count) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    try kz_term:to_integer(Count) of
        I ->
            kapps_account_config:set(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, I),
            io:format("set default count of failed attempts to disable hook to ~p~n", [I])
    catch
        _:_ ->
            io:format("error in count, must be an integer~n")
    end.

-spec failure_status() -> 'ok'.
failure_status() ->
    Failed = webhooks_disabler:find_failures(),
    Sorted = lists:keysort(1, Failed),
    print_failure_header(),
    _ = [print_failure_count(AccountId, HookId, Count) || {{AccountId, HookId}, Count} <- Sorted],
    print_failure_footer().

-spec failure_status(kz_term:ne_binary()) -> 'ok'.
failure_status(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Failed = webhooks_disabler:find_failures(),

    Sorted = lists:keysort(1, Failed),
    print_failure_header(),
    _ = [print_failure_count(AID, HookId, Count) || {{AID, HookId}, Count} <- Sorted, AccountId =:= AID],
    print_failure_footer().

-define(FORMAT_FAILURE_STRING, "| ~-32s | ~-32s | ~5s |~n").
-define(FORMAT_FAILURE_HEADER, "| ~32.32c | ~32.32c | ~5.5c |~n").

print_failure_header() ->
    io:format(?FORMAT_FAILURE_HEADER, [$-, $-, $-]),
    io:format(?FORMAT_FAILURE_STRING, [<<"Account">>, <<"Hook">>, <<"Count">>]).

print_failure_footer() ->
    io:format(?FORMAT_FAILURE_HEADER, [$-, $-, $-]).

print_failure_count(AccountId, HookId, Count) ->
    io:format(?FORMAT_FAILURE_STRING, [AccountId, HookId, kz_term:to_binary(Count)]).

-spec enable_account_hooks(kz_term:ne_binary()) -> 'ok'.
enable_account_hooks(AccountId) ->
    webhooks_util:reenable(AccountId, <<"account">>).

-spec enable_descendant_hooks(kz_term:ne_binary()) -> 'ok'.
enable_descendant_hooks(AccountId) ->
    webhooks_util:reenable(AccountId, <<"descendants">>).

-spec flush_account_failures(kz_term:ne_binary()) -> 'ok'.
flush_account_failures(AccountId) ->
    io:format("flushed ~p failure entries~n"
             ,[webhooks_disabler:flush_failures(AccountId)]
             ).

-spec flush_hook_failures(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush_hook_failures(AccountId, HookId) ->
    io:format("flushed ~p failure entries for ~s~n"
             ,[webhooks_disabler:flush_failures(AccountId, HookId), HookId]
             ).

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder(?APP).
