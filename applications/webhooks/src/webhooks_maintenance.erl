%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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

        ,reset_hook_definitions/0
        ,reset_hook_definition/1
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
            _ = kapps_config:set_default(?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
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
            _ = kapps_account_config:set(AccountId, ?APP_NAME, ?ATTEMPT_EXPIRY_KEY, I),
            io:format("set default expiry for failure attempts to ~pms on account ~s~n", [I, AccountId])
    catch
        _:_ ->
            io:format("error in expiry time, must be an integer (milliseconds)~n")
    end.

-spec set_disable_threshold(kz_term:ne_binary()) -> 'ok'.
set_disable_threshold(Count) ->
    try kz_term:to_integer(Count) of
        I ->
            _ = kapps_config:set_default(?APP_NAME, ?FAILURE_COUNT_KEY, I),
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
            _ = kapps_account_config:set(AccountId, ?APP_NAME, ?FAILURE_COUNT_KEY, I),
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

-spec reset_hook_definitions() -> 'ok'.
reset_hook_definitions() ->
    {'ok', MasterDb} = kapps_util:get_master_account_db(),
    {'ok', HookDefs} = kz_datamgr:all_docs(MasterDb, [{'startkey', <<"webhooks_">>}
                                                     ,{'endkey', <<"webhooks_{">>} % x, y, z, { in ascii
                                                     ]),
    IDs = [kz_doc:id(HookDef) || HookDef <- HookDefs],
    case kz_datamgr:del_docs(MasterDb, IDs) of
        {'ok', Deleted} -> handle_deleted(Deleted);
        {'error', _E} ->
            io:format("failed to clear old hook definitions: ~p~n", [_E])
    end.

-spec reset_hook_definition(kz_term:ne_binary()) -> 'ok'.
reset_hook_definition(<<"webhooks_", _/binary>>=ID) ->
    {'ok', MasterDb} = kapps_util:get_master_account_db(),
    case kz_datamgr:del_doc(MasterDb, ID) of
        {'ok', Deleted} -> handle_deleted([Deleted]);
        {'error', _E} ->
            io:format("failed to clear hook ~s: ~p~n", [ID, _E])
    end;
reset_hook_definition(Hook) ->
    reset_hook_definition(<<"webhooks_", Hook/binary>>).

reinit_hook_definitions(IDs) ->
    _ = [(kz_term:to_atom(ID)):init()
         || ID <- IDs,
            kz_module:is_exported(ID, 'init', 0)
        ],
    io:format("reinitialized hooks: ~s~n", [kz_binary:join(IDs, <<", ">>)]).

maybe_log_failed([]) -> 'ok';
maybe_log_failed(Failed) ->
    io:format("failed to delete hook definitions: ~s~n"
             ,[kz_binary:join([kz_doc:id(F) || F <- Failed], <<", ">>)]
             ).

handle_deleted(Deleted) ->
    Pred = fun(Del) -> kz_json:is_true(<<"ok">>, Del) end,
    {Success, Failed} = lists:partition(Pred, Deleted),
    maybe_log_failed(Failed),

    reinit_hook_definitions([kz_doc:id(D) || D <- Success]).
