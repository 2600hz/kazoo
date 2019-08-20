%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_allotments).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    Allotment = find_allotment(Request, Limits),
    maybe_consume_allotment(Allotment, Request, Limits).

-spec maybe_consume_allotment(kz_term:api_object(), j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_consume_allotment('undefined', Request, _) ->
    lager:debug("account has no allotment", []),
    Request;
maybe_consume_allotment(Allotment, Request, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    Amount = kz_json:get_integer_value(<<"amount">>, Allotment, 0),
    Minimum = kz_json:get_integer_value(<<"minimum">>, Allotment, 0),
    ConsumeGroup = kz_json:get_value(<<"group_consume">>, Allotment, []),
    GroupConsumed = maybe_group_consumed(ConsumeGroup, Allotment, Limits, 0),
    case allotment_consumed_so_far(Allotment, Limits) of
        {'error', _R} when GroupConsumed > (Amount - Minimum) ->
            lager:debug("account ~s has used all ~ws of their allotment"
                       ,[AccountId, Amount]
                       ),
            Request;
        {'error', _R} -> Request;
        Consumed when (Consumed + GroupConsumed) > (Amount - Minimum) ->
            lager:debug("account ~s has used all ~ws of their allotment"
                       ,[AccountId, Amount]
                       ),
            Request;
        Consumed ->
            lager:debug("account ~s has ~ws remaining of their allotment"
                       ,[AccountId, Amount - Consumed - GroupConsumed]
                       ),
            Classification = kz_json:get_value(<<"classification">>, Allotment),
            j5_request:authorize(<<"allotment_", Classification/binary>>, Request, Limits)
    end.

-spec maybe_group_consumed(kz_term:binaries(), kz_json:object(), j5_limits:limits(), non_neg_integer()) -> non_neg_integer().
maybe_group_consumed([], _Allotment, _Limits, Acc) -> Acc;
maybe_group_consumed([Member|Group], Allotment, Limits, Acc) when is_binary(Member) ->
    NewAllotment = kz_json:set_value(<<"classification">>, Member, Allotment),
    case allotment_consumed_so_far(NewAllotment, Limits) of
        {'error', _R} -> maybe_group_consumed(Group, Allotment, Limits, Acc);
        Consumed -> maybe_group_consumed(Group, Allotment, Limits, Acc+Consumed)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(Request, Limits) ->
    case j5_request:billing(Request, Limits) of
        <<"allotment_", _/binary>> -> maybe_reconcile_allotment(Request, Limits);
        _Else -> 'ok'
    end.

-spec maybe_reconcile_allotment(j5_request:request(), j5_limits:limits()) -> 'ok'.
maybe_reconcile_allotment(Request, Limits) ->
    case find_allotment(Request, Limits) of
        'undefined' -> 'ok';
        Allotment ->
            BillingSeconds = j5_request:billing_seconds(Request),
            AllotmentSeconds = get_allotment_seconds(BillingSeconds, Allotment),
            reconcile_allotment(AllotmentSeconds, Allotment, Request, Limits)
    end.

-spec get_allotment_seconds(non_neg_integer(), kz_json:object()) -> non_neg_integer().
get_allotment_seconds(BillingSeconds, Allotment) ->
    NoConsumeTime = kz_json:get_integer_value(<<"no_consume_time">>, Allotment, 0),
    Increment = kz_json:get_integer_value(<<"increment">>, Allotment, 1),
    Minimum = kz_json:get_integer_value(<<"minimum">>, Allotment, 0),
    case BillingSeconds > NoConsumeTime of
        'true' -> kapps_call_util:calculate_cost(60, Increment, Minimum, 0, BillingSeconds);
        'false' -> 0
    end.

-spec reconcile_allotment(non_neg_integer(), kz_json:object(), j5_request:request(), j5_limits:limits()) ->
                                 'ok'.
reconcile_allotment(0, _, _, _) -> 'ok';
reconcile_allotment(Seconds, Allotment, Request, Limits) ->
    CallId = j5_request:call_id(Request),
    AccountId = j5_limits:account_id(Limits),
    LedgerDb = kz_util:format_account_mod_id(AccountId),
    Timestamp = kz_time:now_s(),
    Id = <<CallId/binary, "-allotment-consumption">>,
    lager:debug("adding allotment debit ~s to ledger ~s for ~wsec"
               ,[Id, LedgerDb, Seconds]
               ),
    Props =
        props:filter_undefined(
          [{<<"_id">>, Id}
          ,{<<"account_id">>, AccountId}
          ,{<<"seconds">>, abs(Seconds)}
          ,{<<"call_id">>, CallId}
          ,{<<"name">>, kz_json:get_value(<<"name">>, Allotment)}
          ,{<<"classification">>, kz_json:get_value(<<"classification">>, Allotment)}
          ,{<<"request">>, j5_request:to_jobj(Request)}
          ,{<<"pvt_created">>, Timestamp}
          ,{<<"pvt_modified">>, Timestamp}
          ,{<<"pvt_vsn">>, 1}
          ,{<<"pvt_whapp">>, ?APP_NAME}
          ,{<<"pvt_type">>, <<"allotment_consumption">>}
          ]
         ),
    _ = kz_datamgr:save_doc(LedgerDb, kz_json:from_list(Props)),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_allotment(j5_request:request(), j5_limits:limits()) -> kz_term:api_object().
find_allotment(Request, Limits) ->
    case j5_request:classification(Request) of
        'undefined' -> 'undefined';
        Classification ->
            Direction = j5_request:call_direction(Request),
            find_allotment_by_classification(Direction, Classification, Limits)
    end.

-spec find_allotment_by_classification(kz_term:ne_binary(), kz_term:ne_binary(), j5_limits:limits()) -> kz_term:api_object().
find_allotment_by_classification(Direction, Classification, Limits) ->
    DirectionalClassification = <<Direction/binary, "_", Classification/binary>>,
    case find_allotment_by_classification(DirectionalClassification, Limits) of
        'undefined' ->
            find_allotment_by_classification(Classification, Limits);
        Allotment -> Allotment
    end.

-spec find_allotment_by_classification(kz_term:ne_binary(), j5_limits:limits()) -> kz_term:api_object().
find_allotment_by_classification(Classification, Limits) ->
    Allotments = j5_limits:allotments(Limits),
    lager:debug("checking if account ~s has any allotments for ~s"
               ,[j5_limits:account_id(Limits), Classification]
               ),
    case kz_json:get_value(Classification, Allotments) of
        'undefined' -> 'undefined';
        Allotment -> kz_json:set_value(<<"classification">>, Classification, Allotment)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allotment_consumed_so_far(kz_json:object(), j5_limits:limits()) ->
                                       integer() |
                                       {'error', any()}.
allotment_consumed_so_far(Allotment, Limits) ->
    Classification = kz_json:get_value(<<"classification">>, Allotment),
    Cycle = kz_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    CycleStart = cycle_start(Cycle),
    CycleSpan = cycle_span(Cycle),
    CycleEnd = CycleStart + CycleSpan,
    case allotment_consumed_so_far(CycleStart, CycleEnd, Classification, Limits, 0) of
        {'error', _}=Error -> Error;
        Consumed ->
            Consumed + j5_channels:allotment_consumed(CycleStart, CycleSpan, Classification, Limits)
    end.

-spec allotment_consumed_so_far(non_neg_integer(), non_neg_integer(), kz_term:ne_binary(), j5_limits:limits(), 0..3) ->
                                       integer() |
                                       {'error', any()}.
allotment_consumed_so_far(_, _, _, _, Attempts) when Attempts > 2 -> 0;
allotment_consumed_so_far(CycleStart, CycleEnd, Classification, Limits, Attempts) ->
    AccountId = j5_limits:account_id(Limits),
    LedgerDb = kz_util:format_account_mod_id(AccountId),
    ViewOptions = [{'startkey', [Classification, CycleStart]}
                  ,{'endkey', [Classification, CycleEnd]}
                  ,{'reduce', 'false'}
                  ],
    case kz_datamgr:get_results(LedgerDb, <<"allotments/consumed">>, ViewOptions) of
        {'ok', JObjs} -> sum_allotment_consumed_so_far(JObjs, CycleStart);
        {'error', 'not_found'} ->
            _ = kazoo_modb:refresh_views(LedgerDb),
            allotment_consumed_so_far(CycleStart, CycleEnd, Classification, Limits, Attempts + 1);
        {'error', _R}=Error ->
            lager:debug("unable to get consumed quantity for ~s allotment from ~s: ~p"
                       ,[Classification, LedgerDb, _R]
                       ),
            Error
    end.

-spec sum_allotment_consumed_so_far(kz_json:objects(), non_neg_integer()) -> non_neg_integer().
sum_allotment_consumed_so_far(JObjs, CycleStart) ->
    sum_allotment_consumed_so_far(JObjs, CycleStart, 0).

-spec sum_allotment_consumed_so_far(kz_json:objects(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
sum_allotment_consumed_so_far([], _, Seconds) -> Seconds;
sum_allotment_consumed_so_far([JObj|JObjs], CycleStart, Seconds) ->
    [_, Timestamp] = kz_json:get_value(<<"key">>, JObj),
    Duration = kz_json:get_value(<<"value">>, JObj),
    case (Timestamp - Duration) > CycleStart of
        'true' ->
            sum_allotment_consumed_so_far(JObjs, CycleStart, Seconds + Duration);
        'false' ->
            sum_allotment_consumed_so_far(JObjs, CycleStart, Seconds + (Timestamp - CycleStart))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cycle_start(kz_term:ne_binary()) -> integer().
cycle_start(<<"monthly">>) ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}});
cycle_start(<<"weekly">>) ->
    {Date, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) -
        (calendar:day_of_the_week(Date) - 1) * ?SECONDS_IN_DAY;
cycle_start(<<"daily">>) ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}});
cycle_start(<<"hourly">>) ->
    {{Year, Month, Day}, {Hour, _, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}});
cycle_start(<<"minutely">>) ->
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, 0}}).

-spec cycle_span(kz_term:ne_binary()) -> integer().
cycle_span(<<"monthly">>) -> 2629743; % avg days in month
cycle_span(<<"weekly">>) -> ?SECONDS_IN_WEEK;
cycle_span(<<"daily">>) -> ?SECONDS_IN_DAY;
cycle_span(<<"hourly">>) -> ?SECONDS_IN_HOUR;
cycle_span(<<"minutely">>) -> 60.
