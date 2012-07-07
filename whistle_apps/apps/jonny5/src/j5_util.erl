%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([get_limits/1]).
-export([write_debit_to_ledger/4]).
-export([write_credit_to_ledger/4]).
-export([current_balance/1]).
-export([session_cost/2]).
-export([get_session_id/1]).

-include_lib("jonny5/src/jonny5.hrl").

-define(LIMITS_KEY(AccountId), {limits, AccountId}).

-spec get_limits/1 :: (ne_binary()) -> #limits{}.
get_limits(Account) ->
        
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
        case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {ok, Limits} ->
                Limits;
             
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
                             ,max_postpay_amount = wapi_money:dollars_to_units(wh_json:get_float_value(<<"pvt_max_postpay_amount">>, JObj, DefaultMaxPostpay))
                             ,reserve_amount = wapi_money:dollars_to_units(wh_json:get_float_value(<<"pvt_reserve_amount">>, JObj, DefaultReserve))
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

-spec write_debit_to_ledger/4 :: (ne_binary(), float(), wh_json:json_object(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                               {'error', _}.
write_debit_to_ledger(Suffix, Units, JObj, Ledger) ->
    write_to_ledger(Suffix, Units, JObj, Ledger, debit).

-spec write_credit_to_ledger/4 :: (ne_binary(), float(), wh_json:json_object(), ne_binary()) -> {'ok', wh_json:json_object()} |
                                                                                                {'error', _}.
write_credit_to_ledger(Suffix, Units, JObj, Ledger) ->
    write_to_ledger(Suffix, Units, JObj, Ledger, credit).

-spec write_to_ledger/5 :: (ne_binary(), float(), wh_json:json_object(), ne_binary(), debit | credit) -> {'ok', wh_json:json_object()} |
                                                                                                         {'error', _}.
write_to_ledger(_, 0.0, _, _, _) ->
    lager:debug("skipping update, units are zero", []),
    {ok, wh_json:new()};
write_to_ledger(Suffix, Units, JObj, Ledger, Type) ->
    LedgerId = wh_util:format_account_id(Ledger, raw),
    LedgerDb = wh_util:format_account_id(Ledger, encoded),
    Timestamp = wh_json:get_binary_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
    SessionId = get_session_id(JObj),
    Id = <<SessionId/binary, "-", (wh_util:to_binary(Suffix))/binary>>,
    Entry = wh_json:from_list([{<<"_id">>, Id}
                               ,{<<"reason">>, <<"per_minute_channel">>}
                               ,{<<"session_id">>, SessionId}
                               ,{<<"account_id">>, wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)}
                               ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                               ,{<<"amount">>, abs(Units)}
                               ,{<<"pvt_account_id">>, LedgerId}
                               ,{<<"pvt_account_db">>, LedgerDb}
                               ,{<<"pvt_type">>, wh_util:to_binary(Type)}
                               ,{<<"pvt_created">>, Timestamp}
                               ,{<<"pvt_modified">>, Timestamp}
                               ,{<<"pvt_vsn">>, 1}
                               ,{<<"pvt_whapp">>, ?APP_NAME}
                              ]),
%%    lager:debug("write ledger ~s", [wh_json:encode(Entry)]),
    couch_mgr:save_doc(LedgerDb, Entry).

-spec current_balance/1 :: (ne_binary()) -> integer().
current_balance(Ledger) ->
    LedgerDb = wh_util:format_account_id(Ledger, encoded),    
    ViewOptions = [{<<"reduce">>, true}],
    case couch_mgr:get_results(LedgerDb, <<"transactions/credit_remaining">>, ViewOptions) of
        {ok, []} -> 
            lager:debug("no current balance for ~s", [Ledger]),
            0;
        {ok, [ViewRes|_]} -> 
            Credit = wh_json:get_integer_value(<<"value">>, ViewRes, 0),
            lager:debug("current balance for ~s is ~p", [Ledger, Credit]),
            Credit;
        {error, _R} -> 
            lager:debug("unable to get current balance for ~s: ~p", [Ledger, _R]),
            0
    end.

-spec session_cost/2 :: (ne_binary(), ne_binary()) -> integer().
session_cost(SessionId, Ledger) ->
    LedgerDb = wh_util:format_account_id(Ledger, encoded),    
    ViewOptions = [{<<"reduce">>, true}
                   ,{<<"group">>, true}
                   ,{<<"key">>, SessionId}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/session_cost">>, ViewOptions) of
        {ok, []} -> 0;
        {ok, [ViewRes|_]} -> wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _R} -> 
            lager:debug("unable to get session cost for ~s: ~p", [SessionId, _R]),
            0
    end.

-spec get_session_id/1 :: (wh_json:json_object()) -> ne_binary().
get_session_id(JObj) ->
    wh_util:to_hex_binary(crypto:md5(wh_json:get_value(<<"Call-ID">>, JObj))).
