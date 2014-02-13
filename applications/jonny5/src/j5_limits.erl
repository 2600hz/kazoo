%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_limits).

-export([get/1]).
-export([fetch/1]).
-export([account_id/1]).
-export([enabled/1]).
-export([soft_limit_outbound/1]).
-export([soft_limit_inbound/1]).
-export([allotments/1]).
-export([calls/1]).
-export([resource_consuming_calls/1]).
-export([inbound_trunks/1]).
-export([outbound_trunks/1]).
-export([twoway_trunks/1]).
-export([allow_prepay/1]).
-export([allow_postpay/1]).
-export([reserve_amount/1]).
-export([max_postpay/1]).

-include_lib("jonny5.hrl").

-record(limits, {account_id = undefined
                 ,account_db = undefined
                 ,enabled = true
                 ,twoway_trunks = -1
                 ,inbound_trunks = 0
                 ,outbound_trunks = 0
                 ,resource_consuming_calls = -1
                 ,bundled_inbound_trunks = 0
                 ,bundled_outbound_trunks = 0
                 ,bundled_twoway_trunks = 0
                 ,calls = -1
                 ,allow_prepay = true
                 ,allow_postpay = false
                 ,max_postpay_amount = 0
                 ,reserve_amount = 0
                 ,allotments = wh_json:new()
                 ,soft_limit_inbound = false
                 ,soft_limit_outbound = false
                }).
-opaque limits() :: #limits{}.
-export_type([limits/0]).

-define(LIMITS_KEY(AccountId), {'limits', AccountId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> #limits{}.
get(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case wh_cache:peek_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId)) of
        {'ok', Limits} -> Limits;
        {'error', 'not_found'} -> fetch(AccountId)
    end.

-spec fetch(ne_binary()) -> #limits{}.
fetch(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    JObj = get_limit_jobj(AccountDb),
    DefaultUsePrepay = whapps_config:get_is_true(<<"jonny5">>, <<"default_use_prepay">>, 'true'),
    DefaultPostpay = whapps_config:get_is_true(<<"jonny5">>, <<"default_allow_postpay">>, 'false'),
    DefaultMaxPostpay = whapps_config:get_float(<<"jonny5">>, <<"default_max_postpay_amount">>, 0.0),
    DefaultReserve = whapps_config:get_float(<<"jonny5">>, <<"default_reserve_amount">>, ?DEFAULT_RATE),
    Limits = #limits{account_id = AccountId
                     ,account_db = AccountDb
                     ,enabled = wh_json:is_true(<<"pvt_enabled">>, JObj, 'true')
                     ,twoway_trunks = get_limit(<<"twoway_trunks">>, JObj)
                     ,inbound_trunks = get_limit(<<"inbound_trunks">>, JObj)
                     ,outbound_trunks = get_limit(<<"outbound_trunks">>, JObj, 0)
                     ,resource_consuming_calls = get_limit(<<"resource_consuming_calls">>, JObj)
                     ,calls = get_limit(<<"calls">>, JObj)
                     ,bundled_inbound_trunks = get_bundled_inbound_limit(AccountDb, JObj)
                     ,bundled_outbound_trunks = get_bundled_outbound_limit(AccountDb, JObj)
                     ,bundled_twoway_trunks = get_bundled_twoway_limit(AccountDb, JObj)
                     ,allow_prepay = wh_json:is_true(<<"allow_prepay">>, JObj, DefaultUsePrepay)
                     ,allow_postpay = wh_json:is_true(<<"pvt_allow_postpay">>, JObj, DefaultPostpay)
                     ,max_postpay_amount = wht_util:dollars_to_units(abs(wh_json:get_float_value(<<"pvt_max_postpay_amount">>, JObj, DefaultMaxPostpay))) * -1
                     ,reserve_amount = wht_util:dollars_to_units(abs(wh_json:get_float_value(<<"pvt_reserve_amount">>, JObj, DefaultReserve)))
                     ,allotments = wh_json:get_value(<<"pvt_allotments">>, JObj, wh_json:new())
                     ,soft_limit_inbound = wh_json:is_true(<<"pvt_soft_limit_inbound">>, JObj)
                     ,soft_limit_outbound = wh_json:is_true(<<"pvt_soft_limit_outbound">>, JObj)
                    },
    CacheProps = [{'origin', {'db', AccountDb}}],
    wh_cache:store_local(?JONNY5_CACHE, ?LIMITS_KEY(AccountId), Limits, CacheProps),
    Limits.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id(#limits{}) -> api_binary().
account_id(#limits{account_id=AccountId}) -> AccountId;
account_id(_) -> 'undefined'.
 
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec enabled(#limits{}) -> boolean().
enabled(#limits{enabled=Enabled}) -> Enabled.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec soft_limit_outbound(#limits{}) -> boolean().
soft_limit_outbound(#limits{soft_limit_outbound=SoftLimit}) -> SoftLimit.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec soft_limit_inbound(#limits{}) -> boolean().
soft_limit_inbound(#limits{soft_limit_inbound=SoftLimit}) -> SoftLimit.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allotments(#limits{}) -> wh_json:object().
allotments(#limits{allotments=Allotments}) -> Allotments.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calls(#limits{}) -> -1 | non_neg_integer().
calls(#limits{calls=Calls}) -> Calls.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_consuming_calls(#limits{}) -> -1 | non_neg_integer().
resource_consuming_calls(#limits{resource_consuming_calls=Calls}) -> Calls.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec inbound_trunks(#limits{}) -> -1 | non_neg_integer().
inbound_trunks(#limits{bundled_inbound_trunks=BundledTrunks
                       ,inbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec outbound_trunks(#limits{}) -> -1 | non_neg_integer().
outbound_trunks(#limits{bundled_outbound_trunks=BundledTrunks
                        ,outbound_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec twoway_trunks(#limits{}) -> -1 | non_neg_integer().
twoway_trunks(#limits{bundled_twoway_trunks=BundledTrunks
                      ,twoway_trunks=Trunks}) ->
    BundledTrunks + Trunks.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_prepay(#limits{}) -> boolean().
allow_prepay(#limits{allow_prepay=AllowPrepay}) -> AllowPrepay.


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_postpay(#limits{}) -> boolean().
allow_postpay(#limits{allow_postpay=AllowPostpay}) -> AllowPostpay.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reserve_amount(#limits{}) -> non_neg_integer().
reserve_amount(#limits{reserve_amount=ReserveAmount}) -> ReserveAmount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec max_postpay(#limits{}) -> non_neg_integer().
max_postpay(#limits{max_postpay_amount=MaxPostpay}) -> MaxPostpay.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit(ne_binary(), wh_json:object()) -> integer().
get_limit(Key, JObj) ->
    get_limit(Key, JObj, -1).

-spec get_limit(ne_binary(), wh_json:object(), integer()) -> integer().
get_limit(Key, JObj, Default) ->
    DefaultValue = whapps_config:get_integer(<<"jonny5">>, <<"default_", Key/binary>>, Default),
    PublicValue =  wh_json:get_integer_value(Key, JObj, DefaultValue),
    case wh_json:get_integer_value(<<"pvt_", Key/binary>>, JObj) of
        'undefined' -> PublicValue;
        -1 -> -1;
        PrivateValue when PrivateValue < PublicValue -> PrivateValue;
        _Else -> PublicValue
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_bundled_inbound_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_inbound_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_inbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->     
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_outbound_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_outbound_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_outbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->     
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_twoway_limit(ne_binary(), wh_json:object()) -> non_neg_integer().
get_bundled_twoway_limit(AccountDb, JObj) ->
    case wh_json:get_ne_value(<<"pvt_bundled_twoway_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->     
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_limit(ne_binary(), ne_binary()) -> non_neg_integer().
get_bundled_limit(AccountDb, View) ->
    case couch_mgr:get_results(AccountDb, View, []) of
        {'ok', JObjs} -> filter_bundled_limit(JObjs);
        {'error', _R} ->
            lager:debug("failed get bundled limit from ~s in ~s: ~p"
                        ,[View, AccountDb, _R]),
            0
    end.

-spec filter_bundled_limit(wh_json:objects()) -> non_neg_integer().
filter_bundled_limit(JObjs) ->
    length([JObj
            || JObj <- JObjs
                   ,wh_json:is_true([<<"value">>, <<"enabled">>]
                                    ,JObj
                                    ,'true')
           ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_limit_jobj(ne_binary()) -> wh_json:object().
get_limit_jobj(AccountDb) ->
    case couch_mgr:open_doc(AccountDb, <<"limits">>) of
        {'ok', J} -> J;
        {'error', 'not_found'} ->
            lager:debug("failed to open limits doc in account db ~s", [AccountDb]),
            create_init_limits(AccountDb)
    end.

-spec create_init_limits(ne_binary()) -> wh_json:object().
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
        {'ok', J} ->
            lager:debug("created initial limits document in db ~s", [AccountDb]),
            J;
         {'error', _R} ->
            lager:debug("failed to create initial limits document in db ~s: ~p", [AccountDb, _R]),
            wh_json:new()
    end.
