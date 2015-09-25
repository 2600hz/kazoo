%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_topup).

-export([init/2]).

-include("../include/whistle_transactions.hrl").

-define(WH_SERVICES_DB, <<"services">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init(api_binary(), integer()) -> 'ok' | 'error'.
init(Account, Balance) ->
    case get_top_up(Account) of
        {'error', 'topup_undefined'} -> 'error';
        {'error', 'topup_disabled'} ->
            lager:debug("trying to top up account ~s but top up is disabled", [Account]),
            'error';
        {'error', _E} ->
            lager:error("could not get top up settings for ~s : ~p", [Account, _E]),
            'error';
        {'ok', Amount, Threshold} ->
            maybe_top_up(Account, wht_util:units_to_dollars(Balance), Amount, Threshold)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_top_up(api_binary() | wh_json:object()) ->
                        {'error', _} |
                        {'ok', integer(), integer()}.
get_top_up(<<_/binary>> = Account) ->
    case whapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>, 'false') of
        'false' -> {'error', 'topup_disabled'};
        'true' ->
            AccountId = wh_util:format_account_id(Account, 'raw'),
            AccountDb = wh_util:format_account_id(Account, 'encoded'),
            case couch_mgr:open_doc(AccountDb, AccountId) of
                {'error', _}=Error ->
                    lager:error("could not open account ~s in ~s", [AccountId, AccountDb]),
                    Error;
                {'ok', Doc} -> get_top_up(wh_json:get_value(<<"topup">>, Doc))
            end
    end;
get_top_up('undefined') -> {'error', 'topup_undefined'};
get_top_up(JObj) ->
    case
        {wh_json:get_integer_value(<<"amount">>, JObj)
         ,wh_json:get_integer_value(<<"threshold">>, JObj)
        }
    of
        {'undefined', _} -> {'error', 'amount_undefined'};
        {_, 'undefined'} -> {'error', 'limit_undefined'};
        {Amount, Threshold} when Amount > 0 andalso Threshold > 0 -> {'ok', Amount, Threshold};
        {_, _} -> {'error', 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_top_up(ne_binary(), number(), integer(), integer()) -> 'ok' | 'error'.
maybe_top_up(Account, Balance, Amount, Threshold) when Balance =< Threshold ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            Transactions = wh_json:get_value(<<"transactions">>, JObj, []),
            trying_top_up(Account, Amount, Transactions);
        {'error', _R} ->
            lager:error("unable to open account ~s services doc: ~p", [Account, _R]),
            'error'
    end;
maybe_top_up(Account, Balance, _, Threshold) ->
    lager:warning("balance (~p) is still > to threshold (~p) for account ~s", [Balance, Threshold, Account]),
    'error'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec trying_top_up(ne_binary(), integer(), wh_json:objects()) -> 'ok' | 'error'.
trying_top_up(Account, Amount, []) ->
    lager:info("no top up transactions found, processing..."),
    top_up(Account, Amount);
trying_top_up(Account, Amount, [JObj|JObjs]) ->
    Transaction = wh_transaction:from_json(JObj),
    case wh_transaction:reason(Transaction) =:= <<"topup">> of
        'true' ->
            lager:info("top up for ~s already done, skipping...", [Account]),
            'error';
        'false' ->
            trying_top_up(Account, Amount, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @do
%%
%% @end
%%--------------------------------------------------------------------
-spec top_up(ne_binary(), integer()) -> 'ok' | 'error'.
top_up(Account, Amount) ->
    Transaction = wh_transaction:debit(Account, wht_util:dollars_to_units(Amount)),
    Transaction1 = wh_transaction:set_reason(<<"topup">>, Transaction),
    case wh_transaction:service_save(Transaction1) of
        {'error', _E} ->
            lager:error("fail to top up ~s : ~p", [Account, _E]),
            'error';
        {'ok', _} ->
            lager:info("account ~s top up for ~p", [Account, Amount])
    end.
