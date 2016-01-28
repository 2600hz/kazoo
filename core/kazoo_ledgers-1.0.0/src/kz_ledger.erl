%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_ledger).

-include("kzl.hrl").

-export([get/2
         ,credit/4, credit/5
         ,debit/4, debit/5
        ]).

-type save_return() :: {'ok', ledger()} | {'error', any()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary(), ne_binary()) -> {'ok', integer()} | {'error', any()}.
get(Account, Name) ->
    Options = [
        'reduce'
        ,{'key', Name}
    ],
    case kazoo_modb:get_results(Account, ?LIST_BY_SERVICE, Options) of
        {'ok', []} -> {'ok', 0};
        {'error', _R}=Error -> Error;
        {'ok', [JObj|_]} ->
            {'ok', wh_json:get_integer_value(<<"value">>, JObj, 0)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec credit(ne_binary(), ne_binary()
             ,ne_binary(), wh_proplist()) -> save_return().
-spec credit(ne_binary(), ne_binary(), ne_binary()
            ,wh_proplist(), wh_proplist()) -> save_return().
credit(SrcService, SrcId, Account, Usage) ->
    credit(SrcService, SrcId, Account, Usage, []).

credit(SrcService, SrcId, Account, Usage, Props) ->
    create(<<"credit">>, SrcService, SrcId, Account, Usage, Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec debit(ne_binary(), ne_binary()
             ,ne_binary(), wh_proplist()) -> save_return().
-spec debit(ne_binary(), ne_binary(), ne_binary()
            ,wh_proplist(), wh_proplist()) -> save_return().
debit(SrcService, SrcId, Account, Usage) ->
    debit(SrcService, SrcId, Account, Usage, []).

debit(SrcService, SrcId, Account, Usage, Props) ->
    create(<<"debit">>, SrcService, SrcId, Account, Usage, Props).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), ne_binary(), ne_binary()
             ,ne_binary(), wh_proplist(), wh_proplist()) -> save_return().
create(Type, SrcService, SrcId, Account, Usage, Props) ->
    Routines = [
        fun(L) -> kazoo_ledger:set_type(L, Type) end
        ,fun(L) -> kazoo_ledger:set_source_service(L, SrcService) end
        ,fun(L) -> kazoo_ledger:set_source_id(L, SrcId) end
        ,fun(L) -> set_account(L, Account) end
        ,fun(L) -> set_usage(L, Usage) end
        ,fun(L) -> set_extra(L, Props) end
        ,fun kazoo_ledger:save/1
    ],
    lists:foldl(fun(F, L) -> F(L) end, kazoo_ledger:new(), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_account(ledger(), ne_binary()) -> ledger().
set_account(Ledger, Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Routines = [
        fun(L) -> kazoo_ledger:set_account_id(L, AccountId) end
        ,fun(L) ->
            Name = whapps_util:get_account_name(AccountId),
            kazoo_ledger:set_account_name(L, Name)
        end
    ],
    lists:foldl(fun(F, L) -> F(L) end, Ledger, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_usage(ledger(), wh_proplist()) -> ledger().
set_usage(Ledger, []) -> Ledger;
set_usage(Ledger, [{<<"type">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_type(Ledger, Val), Usage);
set_usage(Ledger, [{<<"quantity">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_quantity(Ledger, Val), Usage);
set_usage(Ledger, [{<<"unit">>, Val}|Usage]) ->
    set_usage(kazoo_ledger:set_usage_unit(Ledger, Val), Usage);
set_usage(Ledger, [_|Usage]) ->
    set_usage(Ledger, Usage).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_extra(ledger(), wh_proplist()) -> ledger().
set_extra(Ledger, []) -> Ledger;
set_extra(Ledger, [{<<"amount">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_amount(Ledger, Val), Props);
set_extra(Ledger, [{<<"description">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_description(Ledger, Val), Props);
set_extra(Ledger, [{<<"period_start">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_period_start(Ledger, Val), Props);
set_extra(Ledger, [{<<"period_end">>, Val}|Props]) ->
    set_extra(kazoo_ledger:set_period_end(Ledger, Val), Props);
set_extra(Ledger, [_|Props]) ->
    set_extra(Ledger, Props).
