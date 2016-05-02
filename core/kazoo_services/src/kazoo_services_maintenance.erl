%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_services_maintenance).

-export([credit/2]).
-export([debit/2]).
-export([refresh/0]).
-export([reconcile/0, reconcile/1]).
-export([sync/1]).
-export([make_reseller/1]).
-export([demote_reseller/1]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).

-include("kazoo_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add arbitrary credit to an account, without charging the accounts
%% credit card
%% @end
%%--------------------------------------------------------------------
-spec credit(ne_binary(), text()) -> 'no_return'.
credit(AccountId, Amount) ->
    Units = wht_util:dollars_to_units(Amount),

    case create_transaction(kz_transaction:credit(AccountId, Units)) of
        'ok' ->
            lager:info("credited account ~p $~p", [AccountId, Amount]);
        {'error', _R} ->
            lager:info("failed to credit account: ~s: ~p", [AccountId, _R])
    end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add arbitrary debit to an account, without charging the accounts
%% debit card
%% @end
%%--------------------------------------------------------------------
-spec debit(ne_binary(), text()) -> 'no_return'.
debit(AccountId, Amount) ->
    Units = wht_util:dollars_to_units(Amount),

    case create_transaction(kz_transaction:debit(AccountId, Units)) of
        'ok' -> lager:info("debited account ~s $~p", [AccountId, Amount]);
        {'error', _R} ->
            lager:info("failed to debit account ~s: ~p"
                       ,[AccountId, _R]
                      )
    end,
    'no_return'.

-spec create_transaction(kz_transaction:kz_transaction()) ->
                                'ok' |
                                {'error', any()}.
create_transaction(Transaction) ->
    Routines = [fun admin_discretion/1
                ,fun admin_description/1
               ],
    T = lists:foldl(fun(F, T) -> F(T) end
                    ,Transaction
                    ,Routines
                   ),
    case kz_transaction:save(T) of
        {'ok', _} -> 'ok';
        {'error', _R}=E -> E
    end.

-spec admin_discretion(kz_transaction:kz_transaction()) ->
                              kz_transaction:kz_transaction().
admin_discretion(T) ->
    kz_transaction:set_reason(<<"admin_discretion">>, T).

-spec admin_description(kz_transaction:kz_transaction()) ->
                              kz_transaction:kz_transaction().
admin_description(T) ->
    kz_transaction:set_description(<<"system adminstrator credit modification">>, T).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% maintenance function for the services db
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:db_create(?KZ_SERVICES_DB),
    kz_datamgr:revise_docs_from_folder(?KZ_SERVICES_DB, 'kazoo_services', "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% scheduals an eventual sync with the bookkeeper and will dirty the
%% full reseller tree (as it normally does when changes occur)
%% @end
%%--------------------------------------------------------------------
-spec reconcile() -> 'no_return'.
-spec reconcile(text()) -> 'no_return'.

reconcile() ->
    reconcile('all').

reconcile('all') ->
    Accounts = kapps_util:get_all_accounts('raw'),
    Total = length(Accounts),
    _ = lists:foldr(fun(Account, Current) ->
                            io:format("reconcile services (~p/~p) '~s'~n", [Current, Total, Account]),
                            _ = reconcile(Account),
                            Current + 1
                    end, 1, Accounts),
    'no_return';
reconcile(Account) when not is_binary(Account) ->
    reconcile(kz_util:to_binary(Account));
reconcile(Account) ->
    try kz_services:reconcile(Account) of
        Any -> Any
    catch
        _E:_R ->
            io:format("failed to reconcile account ~s(~p):~n  ~p~n", [Account, _E, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% runs an immediate sync with a bookkeeper without dirting the
%% reseller tree (only the one account is affected)
%% @end
%%--------------------------------------------------------------------
-spec sync(text()) -> 'ok'.
sync(Account) when not is_binary(Account) ->
    sync(kz_util:to_binary(Account));
sync(Account) ->
    kz_service_sync:sync(Account),
    'ok'.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller_id to the provided value on the provided account
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(text(), text()) -> 'ok'.
set_reseller_id(Reseller, Account) when not is_binary(Account) ->
    set_reseller_id(Reseller, kz_util:to_binary(Account));
set_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    set_reseller_id(kz_util:to_binary(Reseller), Account);
set_reseller_id(Reseller, Account) ->
    whs_account_conversion:set_reseller_id(Reseller, Account).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%--------------------------------------------------------------------
-spec cascade_reseller_id(text(), text()) -> 'ok'.
cascade_reseller_id(Reseller, Account) when not is_binary(Account) ->
    cascade_reseller_id(Reseller, kz_util:to_binary(Account));
cascade_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    cascade_reseller_id(kz_util:to_binary(Reseller), Account);
cascade_reseller_id(Reseller, Account) ->
    whs_account_conversion:cascade_reseller_id(Reseller, Account).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%--------------------------------------------------------------------
-spec demote_reseller(text()) -> 'ok'.
demote_reseller(Account) when not is_binary(Account) ->
    demote_reseller(kz_util:to_binary(Account));
demote_reseller(Account) ->
    whs_account_conversion:demote(Account).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%--------------------------------------------------------------------
-spec make_reseller(text()) -> 'ok'.
make_reseller(Account) when not is_binary(Account) ->
    make_reseller(kz_util:to_binary(Account));
make_reseller(Account) ->
    whs_account_conversion:promote(Account).
