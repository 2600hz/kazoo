%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Given a rate_req, find appropriate rate for the call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_rater).

-export([init/0, handle_req/2]).

-include("hotornot.hrl").

-spec init() -> ok.
init() -> kapps_maintenance:refresh(?KZ_RATES_DB).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_rate:req_v(JObj),
    _ = kz_util:put_callid(JObj),
    lager:debug("valid rating request"),
    case get_rate_data(JObj) of
        {'error', 'no_rate_found'} ->
            maybe_publish_no_rate_found(JObj);
        {'ok', Resp} ->
            kapi_rate:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj)
                                  ,props:filter_undefined(Resp)
                                  ),
            kapi_rate:broadcast_resp(props:filter_undefined(Resp))
    end.

-spec maybe_publish_no_rate_found(kz_json:object()) -> 'ok'.
maybe_publish_no_rate_found(JObj) ->
    case kz_json:is_true(<<"Send-Empty">>, JObj, 'false') of
        'true' -> publish_no_rate_found(JObj);
        'false' -> 'ok'
    end.

-spec publish_no_rate_found(kz_json:object()) -> 'ok'.
publish_no_rate_found(JObj) ->
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),

    Resp = [{<<"Msg-ID">>, MsgId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("publishing empty rate resp for ~s(~s)", [ServerId, MsgId]),
    kz_amqp_worker:cast(Resp, fun(P) -> kapi_rate:publish_resp(ServerId, P) end).

-spec get_rate_data(kz_json:object()) ->
                           {'ok', api_terms()} |
                           {'error', 'no_rate_found'}.
get_rate_data(JObj) ->
    ToDID = kz_json:get_value(<<"To-DID">>, JObj),
    FromDID = kz_json:get_value(<<"From-DID">>, JObj),
    case hon_util:candidate_rates(ToDID) of
        {'ok', []} ->
            kz_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("no rates found for ~s to ~s", [FromDID, ToDID]),
            {'error', 'no_rate_found'};
        {'error', _E} ->
            kz_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("rate lookup error for ~s to ~s: ~p"
                       ,[FromDID, ToDID, _E]
                       ),
            {'error', 'no_rate_found'};
        {'ok', Rates} ->
            get_rate_data(JObj, ToDID, FromDID, Rates)
    end.

-spec get_rate_data(kz_json:object(), ne_binary(), api_binary(), kz_json:objects()) ->
                           {'ok', api_terms()} |
                           {'error', 'no_rate_found'}.
get_rate_data(JObj, ToDID, FromDID, Rates) ->
    lager:debug("candidate rates found, filtering"),
    Matching = hon_util:matching_rates(Rates, JObj),

    case hon_util:sort_rates(Matching) of
        [] ->
            kz_notify:system_alert("no rate found after filter/sort for ~s to ~s", [FromDID, ToDID]),
            lager:debug("no rates left for ~s to ~s after filter"
                       ,[FromDID, ToDID]
                       ),
            {'error', 'no_rate_found'};
        [Rate|_] ->
            lager:debug("using rate ~s for ~s to ~s"
                       ,[kz_json:get_value(<<"rate_name">>, Rate)
                        ,FromDID
                        ,ToDID
                        ]
                       ),
            {'ok', rate_resp(Rate, JObj)}
    end.

-spec maybe_get_rate_discount(kz_json:object()) -> api_binary().
-spec maybe_get_rate_discount(kz_json:object(), api_binary()) -> api_binary().
maybe_get_rate_discount(JObj) ->
    maybe_get_rate_discount(JObj, kz_json:get_value(<<"Account-ID">>, JObj)).

maybe_get_rate_discount(_JObj, 'undefined') -> 'undefined';
maybe_get_rate_discount(JObj, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable to open account ~s definition: ~p", [AccountId, _R]),
            'undefined';
        {'ok', Def} ->
            Number = kz_json:get_value(<<"To-DID">>, JObj),
            Classification = knm_converters:classify(Number),
            lager:debug("~s number discount percentage: ~p", [Classification, Def]),
            kz_json:get_value([<<"pvt_discounts">>, Classification, <<"percentage">>], Def)
    end.

-spec rate_resp(kz_json:object(), kz_json:object()) -> kz_proplist().
rate_resp(Rate, JObj) ->
    RateCost = get_rate_cost(Rate),
    RateSurcharge = get_surcharge(Rate),
    RateMinimum = kz_json:get_integer_value(<<"rate_minimum">>, Rate, ?DEFAULT_MINIMUM),
    BaseCost = wht_util:base_call_cost(RateCost, RateMinimum, RateSurcharge),
    PrivateCost = get_private_cost(Rate),
    lager:debug("base cost for a minute call: ~p", [BaseCost]),
    ShouldUpdateCalleeId = should_update_callee_id(JObj),
    [{<<"Rate">>, kz_util:to_binary(RateCost)}
    ,{<<"Rate-Increment">>, kz_json:get_integer_value(<<"rate_increment">>, Rate, ?DEFAULT_INCREMENT)}
    ,{<<"Rate-Minimum">>, kz_util:to_binary(RateMinimum)}
    ,{<<"Discount-Percentage">>, maybe_get_rate_discount(JObj)}
    ,{<<"Surcharge">>, kz_util:to_binary(RateSurcharge)}
    ,{<<"Prefix">>, kz_json:get_binary_value(<<"prefix">>, Rate)}
    ,{<<"Rate-Name">>, kz_json:get_binary_value(<<"rate_name">>, Rate)}
    ,{<<"Rate-Description">>, kz_json:get_binary_value(<<"description">>, Rate)}
    ,{<<"Rate-ID">>, kz_doc:id(Rate)}
    ,{<<"Base-Cost">>, kz_util:to_binary(BaseCost)}
    ,{<<"Pvt-Cost">>, kz_util:to_binary(PrivateCost)}
    ,{<<"Rate-NoCharge-Time">>, kz_json:get_integer_value(<<"rate_nocharge_time">>, Rate, ?DEFAULT_NOCHARGE)}
    ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
    ,{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
    ,{<<"Update-Callee-ID">>, ShouldUpdateCalleeId}
    ,{<<"Rate-Version">>, kz_json:get_binary_value(<<"rate_version">>, Rate)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec get_surcharge(kz_json:object()) -> kz_transaction:units().
get_surcharge(Rate) ->
    Surcharge = kz_json:get_float_value(<<"rate_surcharge">>, Rate, ?DEFAULT_SURCHARGE),
    wht_util:dollars_to_units(Surcharge).

-spec get_rate_cost(kz_json:object()) -> kz_transaction:units().
get_rate_cost(Rate) ->
    Cost = kz_json:get_float_value(<<"rate_cost">>, Rate, ?DEFAULT_COST),
    wht_util:dollars_to_units(Cost).

-spec get_private_cost(kz_json:object()) -> kz_transaction:units().
get_private_cost(Rate) ->
    Cost = kz_json:get_float_value(<<"pvt_internal_rate_cost">>, Rate, ?DEFAULT_INT_COST),
    wht_util:dollars_to_units(Cost).

-spec should_update_callee_id(ne_binary() | kz_json:object()) -> boolean().
should_update_callee_id(?NE_BINARY = AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountDoc} ->
            kz_json:is_true([<<"caller_id_options">>, <<"show_rate">>], AccountDoc, 'false');
        {'error', _R} ->
            lager:debug("failed to load account ~p for update callee id ~p", [AccountId, _R]),
            'false'
    end;
should_update_callee_id(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' -> 'false';
        Id -> should_update_callee_id(Id)
    end.
