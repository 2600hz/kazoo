%%%-------------------------------------------------------------------
%%% @Copyright (C) 2013-2015, 2600Hz
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
        ,enable_account_hooks/1
        ,enable_descendant_hooks/1
        ,reset_webhooks_list/0
        ,flush_account_failures/1
        ,flush_hook_failures/2
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
    try kz_term:to_integer(Expires) of
        I ->
            kapps_config:set_default(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms~n", [I])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

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

-spec set_disable_threshold(ne_binary()) -> 'ok'.
-spec set_disable_threshold(ne_binary(), ne_binary()) -> 'ok'.
set_disable_threshold(Count) ->
    try kz_term:to_integer(Count) of
        I ->
            kapps_config:set_default(?APP_NAME, ?FAILURE_COUNT_KEY, I),
            io:format("set default count of failed attempts to disable hook to ~p~n", [I])
    catch
        _:_ ->
            io:format("error in count, must be an integer~n")
    end.

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
-spec failure_status(ne_binary()) -> 'ok'.
failure_status() ->
    Failed = webhooks_disabler:find_failures(),
    Sorted = lists:keysort(1, Failed),
    print_failure_header(),
    _ = [print_failure_count(AccountId, HookId, Count) || {{AccountId, HookId}, Count} <- Sorted],
    print_failure_footer().

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

-spec enable_account_hooks(ne_binary()) -> 'ok'.
enable_account_hooks(AccountId) ->
    webhooks_util:reenable(AccountId, <<"account">>).

-spec enable_descendant_hooks(ne_binary()) -> 'ok'.
enable_descendant_hooks(AccountId) ->
    webhooks_util:reenable(AccountId, <<"descendants">>).

-spec reset_webhooks_list() -> 'ok'.
reset_webhooks_list() ->
    _ = kapi_maintenance:refresh_database(?KZ_WEBHOOKS_DB),
    _ = kapi_maintenance:refresh_views(?KZ_WEBHOOKS_DB),

    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            Ids = get_webhooks(MasterAccountDb),
            _ = kz_datamgr:del_docs(MasterAccountDb, Ids),
            webhooks_init:init_modules();
        {'error', _} ->
            lager:warning("no master account id set, unable to reset webhooks list")
    end.

-spec get_webhooks(ne_binary()) -> ne_binaries().
get_webhooks(MasterAccountDb) ->
    case kz_datamgr:get_all_results(MasterAccountDb, ?WEBHOOK_META_LIST) of
        {'error', _R} ->
            io:format("failed to load view ~s in ~s", [?WEBHOOK_META_LIST, MasterAccountDb]),
            [];
        {'ok', JObjs} ->
            [kz_json:get_value(<<"id">>, J) || J <- JObjs]
    end.

-spec flush_account_failures(ne_binary()) -> 'ok'.
flush_account_failures(AccountId) ->
    io:format("flushed ~p failure entries~n"
             ,[webhooks_disabler:flush_failures(AccountId)]
             ).

-spec flush_hook_failures(ne_binary(), ne_binary()) -> 'ok'.
flush_hook_failures(AccountId, HookId) ->
    io:format("flushed ~p failure entries for ~s~n"
             ,[webhooks_disabler:flush_failures(AccountId, HookId), HookId]
             ).
