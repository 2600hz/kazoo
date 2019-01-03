%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledger).

-include("kzl.hrl").

-export([get/2
        ,credit/1 ,credit/2 ,credit/4, credit/5, credit/6
        ,debit/1, debit/2, debit/4, debit/5, debit/6
        ]).

-type save_return() :: {'ok', ledger()} | {'error', any()}.

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get(kz_term:ne_binary(), kz_term:ne_binary()) ->
                 {'ok', integer()} |
                 {'error', any()}.
get(Account, Name) ->
    Options = [{'key', Name}],
    case kazoo_modb:get_results(Account, ?LIST_BY_SERVICE_LEGACY, Options) of
        {'ok', []} -> {'ok', 0};
        {'error', _R}=Error -> Error;
        {'ok', [JObj0|_]} ->
            [JObj] = kz_json:values(kz_json:get_value(<<"value">>, JObj0)),
            {'ok', kz_json:get_integer_value(<<"amount">>, JObj, 0)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec credit(ledger()) -> save_return().
credit(Ledger) ->
    credit(kazoo_ledger:account_id(Ledger), Ledger).

-spec credit(kz_term:ne_binary(), ledger()) -> save_return().
credit(LedgerId, Ledger) ->
    create(LedgerId, ?CREDIT, Ledger).

-spec credit(kz_term:ne_binary(), kz_term:ne_binary()
            ,kz_term:ne_binary(), kz_term:proplist()) -> save_return().
credit(LedgerId, SrcService, SrcId, Usage) ->
    credit(LedgerId, SrcService, SrcId, Usage, []).

-spec credit(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
            ,kz_term:proplist(), kz_term:proplist()) -> save_return().
credit(LedgerId, SrcService, SrcId, Usage, Props) ->
    credit(LedgerId, SrcService, SrcId, Usage, Props, LedgerId).

-spec credit(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
            ,kz_term:proplist(), kz_term:proplist(), kz_term:ne_binary()) -> save_return().
credit(LedgerId, SrcService, SrcId, Usage, Props, AccountId) ->
    create(LedgerId, ?CREDIT, SrcService, SrcId, Usage, Props, AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec debit(ledger()) -> save_return().
debit(Ledger) ->
    debit(kazoo_ledger:account_id(Ledger), Ledger).

-spec debit(kz_term:ne_binary(), ledger()) -> save_return().
debit(LedgerId, Ledger) ->
    create(LedgerId, ?DEBIT, Ledger).

-spec debit(kz_term:ne_binary(), kz_term:ne_binary()
           ,kz_term:ne_binary(), kz_term:proplist()) -> save_return().
debit(LedgerId, SrcService, SrcId, Usage) ->
    debit(LedgerId, SrcService, SrcId, Usage, []).

-spec debit(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
           ,kz_term:proplist(), kz_term:proplist()) -> save_return().
debit(LedgerId, SrcService, SrcId, Usage, Props) ->
    debit(LedgerId, SrcService, SrcId, Usage, Props, LedgerId).

-spec debit(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
           ,kz_term:proplist(), kz_term:proplist(), kz_term:ne_binary()) -> save_return().
debit(LedgerId, SrcService, SrcId, Usage, Props, AccountId) ->
    create(LedgerId, ?DEBIT, SrcService, SrcId, Usage, Props, AccountId).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
            ,kz_term:proplist(), kz_term:proplist(),kz_term:ne_binary()) -> save_return().
create(LedgerId, Type, SrcService, SrcId, Usage, Props, AccountId) ->
    Routines = [{fun kazoo_ledger:set_source_service/2, SrcService}
               ,{fun kazoo_ledger:set_source_id/2, SrcId}
               ,{fun set_account/2, AccountId}
               ,{fun set_usage/2, Usage}
               ,{fun set_extra/2, Props}
               ],
    create(LedgerId, Type, lists:foldl(fun apply_routine/2, kazoo_ledger:new(), Routines)).

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), ledger()) -> save_return().
create(LedgerId, Type, Ledger) ->
    Routines = [{fun kazoo_ledger:set_type/2, Type}
               ,{fun kazoo_ledger:save/2, LedgerId}
               ],
    lists:foldl(fun apply_routine/2, Ledger, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account(ledger(), kz_term:ne_binary()) -> ledger().
set_account(Ledger, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Routines = [{fun kazoo_ledger:set_account_id/2, AccountId}
               ,{fun kazoo_ledger:set_account_name/2, kzd_accounts:fetch_name(AccountId)}
               ],
    lists:foldl(fun apply_routine/2, Ledger, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_usage(ledger(), kz_term:proplist()) -> ledger().
set_usage(Ledger, []) -> Ledger;
set_usage(Ledger, [{<<"type">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_type(Ledger, Val), Usage);
set_usage(Ledger, [{<<"quantity">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_quantity(Ledger, Val), Usage);
set_usage(Ledger, [{<<"unit">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_unit(Ledger, Val), Usage);
set_usage(Ledger, [_|Usage]) ->
    set_usage(Ledger, Usage).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_extra(ledger(), kz_term:proplist()) -> ledger().
set_extra(Ledger, []) -> Ledger;
set_extra(Ledger, [{<<"amount">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_amount(Ledger, Val), Props);
set_extra(Ledger, [{<<"description">>, ?NE_BINARY=Val}|Props]) ->
    set_extra(kazoo_ledger:set_description(Ledger, Val), Props);
set_extra(Ledger, [{<<"description">>, _}|Props]) ->
    set_extra(Ledger, Props);
set_extra(Ledger, [{<<"period_start">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_period_start(Ledger, Val), Props);
set_extra(Ledger, [{<<"period_end">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_period_end(Ledger, Val), Props);
set_extra(Ledger, [{<<"metadata">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_metadata(Ledger, Val), Props);
set_extra(Ledger, [_ | Props]) ->
    set_extra(Ledger, Props).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec apply_routine(function() | {function(), any()}, ledger()) -> ledger().
apply_routine({F, V}, L) -> F(L, V);
apply_routine(F, L) -> F(L).
