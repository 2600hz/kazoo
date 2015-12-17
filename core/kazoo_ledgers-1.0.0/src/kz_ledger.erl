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

-export([get/2]).
-export([credit/3, credit/4, debit/3, debit/4]).

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
    case kazoo_modb:get_results(Account, ?LIST_BY_TYPE, Options) of
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
-spec credit(ne_binary(), integer(), ne_binary()) -> {'ok', ledger()} | {'error', any()}.
-spec credit(ne_binary(), integer(), ne_binary(), api_binary()) -> {'ok', ledger()} | {'error', any()}.
credit(Name, Amount, Account) ->
    credit(Name, Amount, Account, 'undefined').

credit(Name, Amount, Account, Desc) ->
    create(Name, Amount, Account, Desc, <<"credit">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec debit(ne_binary(), integer(), ne_binary()) -> {'ok', ledger()} | {'error', any()}.
-spec debit(ne_binary(), integer(), ne_binary(), api_binary()) -> {'ok', ledger()} | {'error', any()}.
debit(Name, Amount, Account) ->
    debit(Name, Amount, Account, 'undefined').

debit(Name, Amount, Account, Desc) ->
    create(Name, Amount, Account, Desc, <<"debit">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), integer(), ne_binary(), api_binary(), ne_binary()) ->
                    {'ok', ledger()} |
                    {'error', any()}.
create(Name, Amount, Account, Desc, Type) ->
    Routines = [
        fun(L) -> kazoo_ledger:set_name(L, Name) end
        ,fun(L) -> kazoo_ledger:set_amount(L, Amount) end
        ,fun(L) -> kazoo_ledger:set_account(L, Account) end
        ,fun(L) -> kazoo_ledger:set_description(L, Desc) end
        ,fun(L) -> kazoo_ledger:set_type(L, Type) end
        ,fun kazoo_ledger:save/1
    ],
    lists:foldl(fun(F, L) -> F(L) end, kazoo_ledger:new(), Routines).
