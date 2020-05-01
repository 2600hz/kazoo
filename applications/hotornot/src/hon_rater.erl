%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Given a rate_req, find appropriate rate for the call
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_rater).

-export([init/0, handle_req/2]).

-include("hotornot.hrl").

-spec init() -> 'ok'.
init() ->
    _ = [kapps_maintenance:refresh(kzd_ratedeck:format_ratedeck_db(Ratedeck))
         || Ratedeck <- kz_services_ratedecks:ratedecks()
        ],
    'ok'.

-spec handle_req(kapi_rate:req(), kz_term:proplist()) -> 'ok'.
handle_req(RateReq, _Props) ->
    'true' = kapi_rate:req_v(RateReq),
    _ = kz_log:put_callid(RateReq),
    lager:debug("valid rating request"),
    case get_rate_data(RateReq, kapi_rate:authorizing_type(RateReq)) of
        {'error', 'no_rate_found'} ->
            maybe_publish_no_rate_found(RateReq);
        {'ok', Resp} ->
            RespAPI = props:filter_undefined(Resp),
            kapi_rate:publish_resp(kz_api:server_id(RateReq), RespAPI),
            kapi_rate:broadcast_resp(RespAPI)
    end.

-spec maybe_publish_no_rate_found(kapi_rate:req()) -> 'ok'.
maybe_publish_no_rate_found(RateReq) ->
    case kapi_rate:send_empty(RateReq) of
        'true' -> publish_no_rate_found(RateReq);
        'false' -> 'ok'
    end.

-spec publish_no_rate_found(kapi_rate:req()) -> 'ok'.
publish_no_rate_found(RateReq) ->
    MsgId = kz_api:msg_id(RateReq),
    ServerId = kz_api:server_id(RateReq),

    Resp = [{<<"Msg-ID">>, MsgId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("publishing empty ~srate resp for ~s(~s)", [maybe_empty_mobile_log(RateReq), ServerId, MsgId]),
    kapi_rate:publish_resp(ServerId, Resp).

-spec maybe_empty_mobile_log(kapi_rate:req()) -> string().
maybe_empty_mobile_log(RateReq) ->
    case kapi_rate:authorizing_type(RateReq) of
        <<"mobile">> -> "mobile ";
        _ -> ""
    end.

-spec get_rate_data(kapi_rate:req(), kz_term:api_ne_binary()) ->
          {'ok', kz_term:api_terms()} |
          {'error', 'no_rate_found'}.
get_rate_data(RateReq, <<"mobile">>) ->
    ToDID = kapi_rate:to_did(RateReq),
    FromDID = kapi_rate:from_did(RateReq),

    case hotornot_config:mobile_rate() of
        'undefined' ->
            maybe_publish_no_rate_found(RateReq);
        Rate ->
            lager:debug("using mobile rate for ~s to ~s", [FromDID, ToDID]),
            {'ok', rate_resp(Rate, RateReq)}
    end;
get_rate_data(RateReq, _AuthType) ->
    ToDID = kapi_rate:to_did(RateReq),
    FromDID = kapi_rate:from_did(RateReq),
    AccountId = kapi_rate:account_id(RateReq),
    RatedeckId = kapi_rate:ratedeck_id(RateReq),

    case hon_util:candidate_rates(ToDID, AccountId, RatedeckId) of
        {'ok', []} ->
            maybe_send_system_alert(RateReq, FromDID, ToDID),
            lager:debug("no rates found for ~s to ~s", [FromDID, ToDID]),
            {'error', 'no_rate_found'};
        {'error', _E} ->
            maybe_send_system_alert(RateReq, FromDID, ToDID),
            lager:debug("rate lookup error for ~s to ~s: ~p"
                       ,[FromDID, ToDID, _E]
                       ),
            {'error', 'no_rate_found'};
        {'ok', Rates} ->
            get_rate_data(RateReq, ToDID, FromDID, Rates)
    end.

-spec maybe_send_system_alert(kapi_rate:req(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_send_system_alert(RateReq, FromDID, ToDID) ->
    Direction = kapi_rate:direction(RateReq),
    case hotornot_config:should_publish_alert(Direction) of
        'true' ->
            kz_notify:system_alert("no ~s rate found for ~s to ~s", [Direction, FromDID, ToDID]);
        'false' ->
            lager:debug("not publishing no_rate_found alert for ~s rate", [Direction])
    end.

-spec get_rate_data(kapi_rate:req(), kz_term:ne_binary(), kz_term:api_binary(), kz_json:objects()) ->
          {'ok', kz_term:api_terms()} |
          {'error', 'no_rate_found'}.
get_rate_data(RateReq, ToDID, FromDID, Rates) ->
    lager:debug("candidate rates found, filtering"),
    Matching = hon_util:matching_rates(Rates, RateReq),

    get_rate_data_from_matching(RateReq, ToDID, FromDID, Matching).

get_rate_data_from_matching(_RateReq, ToDID, FromDID, []) ->
    kz_notify:system_alert("no matching rate found for ~s to ~s", [FromDID, ToDID]),
    lager:debug("no matching rates for ~s to ~s", [FromDID, ToDID]),
    {'error', 'no_rate_found'};
get_rate_data_from_matching(RateReq, ToDID, FromDID, Matching) ->
    get_rate_data_from_sorted(RateReq, ToDID, FromDID, hon_util:sort_rates(Matching)).

get_rate_data_from_sorted(_RateReq, ToDID, FromDID, []) ->
    kz_notify:system_alert("no rate found after filter/sort for ~s to ~s", [FromDID, ToDID]),
    lager:debug("no rates left for ~s to ~s after filter",[FromDID, ToDID]),
    {'error', 'no_rate_found'};
get_rate_data_from_sorted(RateReq, _ToDID, _FromDID, [Rate|_]) ->
    lager:debug("using rate ~s for ~s to ~s"
               ,[kzd_rates:rate_name(Rate), _FromDID, _ToDID]
               ),
    {'ok', rate_resp(Rate, RateReq)}.

-spec maybe_get_rate_discount(kapi_rate:req()) -> kz_term:api_binary().
maybe_get_rate_discount(RateReq) ->
    maybe_get_rate_discount(RateReq, kapi_rate:account_id(RateReq)).

-spec maybe_get_rate_discount(kapi_rate:req(), kz_term:api_binary()) -> kz_term:api_binary().
maybe_get_rate_discount(_RateReq, 'undefined') -> 'undefined';
maybe_get_rate_discount(RateReq, AccountId) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable to open account ~s definition: ~p", [AccountId, _R]),
            'undefined';
        {'ok', Def} ->
            Number = kapi_rate:to_did(RateReq),
            Classification = knm_converters:classify(Number),
            lager:debug("~s number discount percentage: ~p", [Classification, Def]),
            kz_json:get_value([<<"pvt_discounts">>, Classification, <<"percentage">>], Def)
    end.

-spec rate_resp(kz_json:object(), kapi_rate:req()) -> kz_term:proplist().
rate_resp(Rate, RateReq) ->
    RateCost = kz_currency:dollars_to_units(kzd_rates:rate_cost(Rate, 0.0)),
    RateSurcharge = kz_currency:dollars_to_units(kzd_rates:rate_surcharge(Rate, 0.0)),
    RateMinimum = kzd_rates:rate_minimum(Rate, hotornot_config:default_minimum()),
    BaseCost = kapps_call_util:base_call_cost(RateCost, RateMinimum, RateSurcharge),
    PrivateCost = kzd_rates:private_cost(Rate),
    lager:debug("base cost for a call: ~p", [BaseCost]),
    ShouldUpdateCalleeId = should_update_callee_id(RateReq),

    [{<<"Rate">>, kz_term:to_binary(RateCost)}
    ,{<<"Rate-Increment">>, kzd_rates:rate_increment(Rate, hotornot_config:default_increment())}
    ,{<<"Rate-Minimum">>, kz_term:to_binary(RateMinimum)}
    ,{<<"Discount-Percentage">>, maybe_get_rate_discount(RateReq)}
    ,{<<"Surcharge">>, kz_term:to_binary(RateSurcharge)}
    ,{<<"Prefix">>, kzd_rates:prefix(Rate)}
    ,{<<"Rate-Name">>, kzd_rates:rate_name(Rate)}
    ,{<<"Rate-Description">>, kzd_rates:description(Rate)}
    ,{<<"Rate-ID">>, kz_doc:id(Rate)}
    ,{<<"Base-Cost">>, kz_term:to_binary(BaseCost)}
    ,{<<"Pvt-Cost">>, kz_term:to_binary(PrivateCost)}
    ,{<<"Rate-NoCharge-Time">>, kzd_rates:rate_nocharge_time(Rate, hotornot_config:default_nocharge())}
    ,{<<"Msg-ID">>, kz_api:msg_id(RateReq)}
    ,{<<"Call-ID">>, kz_api:call_id(RateReq)}
    ,{<<"Update-Callee-ID">>, ShouldUpdateCalleeId}
    ,{<<"Rate-Version">>, kzd_rates:rate_version(Rate)}
    ,{<<"Ratedeck-ID">>, kzd_rates:ratedeck_id(Rate)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec should_update_callee_id(kz_term:ne_binary() | kapi_rate:req()) -> boolean().
should_update_callee_id(?NE_BINARY = AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountDoc} ->
            kzd_accounts:caller_id_options_show_rate(AccountDoc, 'false');
        {'error', _R} ->
            lager:debug("failed to load account ~p for update callee id ~p", [AccountId, _R]),
            'false'
    end;
should_update_callee_id(RateReq) ->
    case kz_api:account_id(RateReq) of
        'undefined' -> 'false';
        Id -> should_update_callee_id(Id)
    end.
