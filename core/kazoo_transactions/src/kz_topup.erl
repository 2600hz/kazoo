%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_topup).

-export([init/2]).

-include("include/kazoo_transactions.hrl").

-define(KZ_SERVICES_DB, <<"services">>).

-type error() :: 'topup_disabled' |
                 'topup_undefined' |
                 'amount_undefined' |
                 'limit_undefined' |
                 'balance_above_threshold' |
                 'undefined' |
                 atom().


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init(api_binary(), integer()) ->
                  'ok' |
                  {'error', error()}.
init(Account, Balance) ->
    case get_top_up(Account) of
        {'error', _}=E -> E;
        {'ok', Amount, Threshold} ->
            maybe_top_up(Account, wht_util:units_to_dollars(Balance), Amount, Threshold)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_top_up(api_binary() | kz_account:doc()) ->
                        {'error', error()} |
                        {'ok', integer(), integer()}.
get_top_up(<<_/binary>> = Account) ->
    case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            case kz_account:fetch(Account) of
                {'error', _E}=Error ->
                    lager:error("could not open account ~s: ~p", [Account, _E]),
                    Error;
                {'ok', AccountJObj} -> get_top_up(kz_json:get_value(<<"topup">>, AccountJObj))
            end
    end;
get_top_up('undefined') -> {'error', 'topup_undefined'};
get_top_up(JObj) ->
    case
        {kz_json:get_integer_value(<<"amount">>, JObj)
         ,kz_json:get_integer_value(<<"threshold">>, JObj)
        }
    of
        {'undefined', _} -> {'error', 'amount_undefined'};
        {_, 'undefined'} -> {'error', 'limit_undefined'};
        {Amount, Threshold} when Amount > 0
                                 andalso Threshold > 0 ->
            {'ok', Amount, Threshold};
        {_, _} -> {'error', 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_top_up(ne_binary(), number(), integer(), integer()) ->
                          'ok' |
                          {'error', error()}.
maybe_top_up(Account, Balance, Amount, Threshold) when Balance =< Threshold ->
    AccountId = kz_account:format_id(Account, 'raw'),
    case kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, AccountId) of
        {'error', _R}=E -> E;
        {'ok', ServicesJObj} ->
            Transactions = kzd_services:transactions(ServicesJObj),
            trying_top_up(Account, Amount, Transactions)
    end;
maybe_top_up(Account, Balance, _, Threshold) ->
    lager:warning("balance (~p) is still > to threshold (~p) for account ~s", [Balance, Threshold, Account]),
    {'error', 'balance_above_threshold'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec trying_top_up(ne_binary(), integer(), kz_json:objects()) -> 'ok'.
trying_top_up(Account, Amount, []) ->
    lager:info("no top up transactions found, processing..."),
    top_up(Account, Amount);
trying_top_up(Account, Amount, [JObj|JObjs]) ->
    Transaction = kz_transaction:from_json(JObj),
    case kz_transaction:reason(Transaction) =:= <<"topup">> of
        'true' ->
            lager:info("top up for ~s already done, skipping...", [Account]),
            'ok';
        'false' ->
            trying_top_up(Account, Amount, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @do
%%
%% @end
%%--------------------------------------------------------------------
-spec top_up(ne_binary(), integer()) -> 'ok' | {'error', any()}.
top_up(Account, Amount) ->
    Transaction = kz_transaction:debit(Account, wht_util:dollars_to_units(Amount)),
    Transaction1 = kz_transaction:set_reason(<<"topup">>, Transaction),
    case kz_transaction:service_save(Transaction1) of
        {'error', _E}=E -> E;
        {'ok', _} ->
            lager:info("account ~s top up for ~p", [Account, Amount])
    end.
