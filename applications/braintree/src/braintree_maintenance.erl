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
                  io:format(" (~b/~b) ", [Count, TotalLength]),
                  sync_account_services_payments_info(kz_util:format_account_id(AccountId)),
                  timer:sleep(1000),
                  Count + 1
          end,
    _ = lists:foldl(Fun, 1, Accounts),
    'ok'.

-spec sync_account_services_payments_info(kz_term:ne_binary()) -> 'ok'.
sync_account_services_payments_info(AccountId) ->
    io:format("Syncing account ~s payments info: ", [AccountId]),
    try braintree_customer:find(AccountId) of
        #bt_customer{credit_cards = Cards} ->
            case braintree_util:update_services_cards(AccountId, Cards) of
                {'ok', _Services} -> io:format("synced~n", []);
                {'error', _} -> io:format("failed to update service doc~n", [])
            end;
        #bt_api_error{errors = Errors} ->
            io:format("braintree failed with ~p~n", format_errors(Errors))
    catch
        'throw':{_, ErrJObj} -> io:format("braintree failed with ~s ~n", [kz_json:encode(ErrJObj)])
    end.

format_errors([]) -> <<"unknown">>;
format_errors([#bt_error{code = Code, message = Msg}|_]) ->
    <<"code: ", (kz_term:to_binary(Code))/binary, " msg: ", (kz_term:to_binary(Msg))/binary>>.

-spec sync_payment_info(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
sync_payment_info(AccountId, CardId) ->
    io:format("Syncing account ~s card ~s info: ", [AccountId, CardId]),
    try braintree_card:find(CardId) of
        #bt_card{customer_id = AccountId} = Card ->
            case braintree_util:update_services_card(AccountId, Card) of
                {'ok', _Services} -> io:format("synced~n", []);
                {'error', _} -> io:format("failed to update service doc~n", [])
            end;
        #bt_card{} ->
            io:format("card not found~n");
        #bt_api_error{errors = Errors} ->
            io:format("braintree failed with ~p~n", format_errors(Errors))
    catch
        'throw':{_, ErrJObj} -> io:format("braintree failed with ~s ~n", [kz_json:encode(ErrJObj)])
    end.
