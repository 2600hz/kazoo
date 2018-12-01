%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_maintenance).

-include("braintree.hrl").

-export([sync_all_accounts_payments_info/0
        ,sync_account_services_payments_info/1
        ,sync_payment_info/2
        ]).

-spec sync_all_accounts_payments_info() -> 'ok'.
sync_all_accounts_payments_info() ->
    Accounts = kapps_util:get_all_accounts(),
    TotalLength = length(Accounts),
    Fun = fun(AccountId, Count) ->
                  io:format(user, " (~b/~b) ", [Count, TotalLength]),
                  sync_account_services_payments_info(AccountId),
                  Count + 1
          end,
    lists:foldl(Fun, 1, Accounts).

-spec sync_account_services_payments_info(kz_term:ne_binary()) -> 'ok'.
sync_account_services_payments_info(AccountId) ->
    io:format(user, "Syncing account ~s payments info with Kazoo services: ", [AccountId]),
    case braintree_customer:find(AccountId) of
        {'not_found', _} ->
            io:format(user, "customter not found~n", []);
        #bt_customer{credit_cards = Cards} ->
            case braintree_util:update_services_cards(AccountId, Cards) of
                {'ok', _Services} -> io:format(user, "synced~n", []);
                {'error', _} -> io:format(user, "failed~n", [])
            end
    end.

-spec sync_payment_info(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
sync_payment_info(AccountId, CardId) ->
    io:format(user, "Syncing account ~s payments info ~s with Kazoo services: ", [AccountId, CardId]),
    case braintree_customer:find(AccountId) of
        {'not_found', _} ->
            io:format(user, "customter not found~n", []);
        #bt_customer{credit_cards = Cards} ->
            case [Card || #bt_card{token = Id}=Card <- Cards, Id =:= CardId] of
                [] -> io:format(user, " card ~s not found~n", [CardId]);
                [Card|_] ->
                    case braintree_util:update_services_cards(AccountId, Card) of
                        {'ok', _Services} -> io:format(user, "synced~n", []);
                        {'error', _} -> io:format(user, "failed~n", [])
                    end
            end
    end.
