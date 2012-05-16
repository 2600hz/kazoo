%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([get_limits/1]).
-export([write_debit_to_ledger/2]).
%%-export([write_credit_to_ledger/3]).
-export([current_balance/1]).

-include_lib("jonny5/src/jonny5.hrl").

-define(LIMITS_KEY(AccountId), {limits, AccountId}).

-spec get_limits/1 :: (ne_binary()) -> #limits{}.
get_limits(Account) ->    
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {ok, Limits} -> Limits; 
        {error, not_found} ->
            JObj = case couch_mgr:open_doc(AccountDb, <<"limits">>) of
                       {ok, J} -> J;
                       {error, _R} ->
                           lager:debug("failed to open limits doc in account db ~s", [AccountDb]),
                           create_init_limits(AccountDb)
                   end,
            DefaultUsePrepay = whapps_config:get_is_true(<<"jonny5">>, <<"default_use_prepay">>, true),
            DefaultPostpay = whapps_config:get_is_true(<<"jonny5">>, <<"default_allow_postpay">>, false),
            DefaultMaxPostpay = whapps_config:get_float(<<"jonny5">>, <<"default_max_postpay_amount">>, 0.0),
            DefaultReserve = whapps_config:get_float(<<"jonny5">>, <<"default_reserve_amount">>, ?DEFAULT_RATE),
            Limits = #limits{twoway_trunks = get_limit(<<"twoway_trunks">>, JObj)
                             ,inbound_trunks = get_limit(<<"inbound_trunks">>, JObj)
                             ,resource_consuming_calls = get_limit(<<"resource_consuming_calls">>, JObj)
                             ,calls = get_limit(<<"calls">>, JObj)
                             ,allow_prepay = wh_json:is_true(<<"allow_prepay">>, JObj, DefaultUsePrepay)
                             ,allow_postpay = wh_json:is_true(<<"pvt_allow_postpay">>, JObj, DefaultPostpay)
                             ,max_postpay_amount = wh_json:get_float_value(<<"pvt_max_postpay_amount">>, JObj, DefaultMaxPostpay)
                             ,reserve_amount = wh_json:get_float_value(<<"pvt_reserve_amount">>, JObj, DefaultReserve)
                            },
            wh_cache:store_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId), Limits, 900),
            Limits
    end.

-spec get_limit/2 :: (ne_binary(), wh_json:json_object()) -> integer().
get_limit(Key, JObj) ->
    DefaultValue = whapps_config:get_integer(<<"jonny5">>, <<"default_", Key/binary>>, -1),
    PublicValue =  wh_json:get_integer_value(Key, JObj, DefaultValue),
    case wh_json:get_integer_value(<<"pvt_", Key/binary>>, JObj) of
        undefined -> PublicValue;
        -1 -> -1;
        PrivateValue when PrivateValue < PublicValue -> PrivateValue;
        _Else -> PublicValue
    end.

-spec create_init_limits/1 :: (ne_binary()) -> wh_json:json_object().
create_init_limits(AccountDb) ->
    TStamp = wh_util:current_tstamp(),
    JObj = wh_json:from_list([{<<"_id">>, <<"limits">>}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_account_id">>, wh_util:format_account_id(AccountDb, raw)}
                              ,{<<"pvt_type">>, <<"limits">>}
                              ,{<<"pvt_created">>, TStamp}
                              ,{<<"pvt_modified">>, TStamp}
                              ,{<<"pvt_vsn">>, 1}
                             ]),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {ok, J} -> 
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            J;
         {error, _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            wh_json:new()
    end.

-spec write_debit_to_ledger/2 :: (wh_json:json_object(), float()) -> {'ok', wh_json:json_object()} |
                                                                       {'error', _}.
write_debit_to_ledger(JObj, Units) ->
    write_to_ledger(JObj, Units, debit).

%%-spec write_credit_to_ledger/3 :: (ne_binary(), ne_binary(), integer()) -> {'ok', wh_json:json_object()} |
%%                                                                           {'error', _}.
%%write_credit_to_ledger(Id, Account, Units) ->
%%    write_to_ledger(Id, Account, Units, credit).

-spec write_to_ledger/3 :: (wh_json:json_object(), float(), debit | credit) -> {'ok', wh_json:json_object()} |
                                                                                 {'error', _}.
write_to_ledger(JObj, Units, Type) ->
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),    
    Timestamp = wh_json:get_binary_value(<<"Timestamp">>, JObj, <<"start">>),
    BridgeId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], JObj),
    Id = <<BridgeId/binary, "-", Timestamp/binary>>,
    Entry = wh_json:from_list([{<<"_id">>, Id}
                               ,{<<"reason">>, <<"per_minute_call">>}
                               ,{<<"bridge_id">>, BridgeId}
                               ,{<<"amount">>, wapi_money:dollars_to_units(Units)}
                               ,{<<"pvt_account_id">>, AccountId}
                               ,{<<"pvt_account_db">>, AccountDb}
                               ,{<<"pvt_type">>, wh_util:to_binary(Type)}
                               ,{<<"pvt_created">>, wh_util:current_tstamp()}
                               ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                               ,{<<"pvt_vsn">>, 1}
                               ,{<<"pvt_whapp">>, ?APP_NAME}
                              ]),
    io:format("SAVE: ~p~n", [Entry]),
    {ok, Entry}.
%%    couch_mgr:save_doc(AccountDb, JObj).

-spec current_balance/1 :: (ne_binary()) -> integer().
current_balance(Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),    
    ViewOptions = [{<<"reduce">>, true}],
    case couch_mgr:get_results(AccountDb, <<"transactions/credit_remaining">>, ViewOptions) of
        {ok, []} -> 0;
        {ok, [ViewRes|_]} -> wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _} -> 0
    end.
