%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_allotments).

-export([authorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    Allotment = try_find_allotment(Request, Limits),
    maybe_consume_allotment(Allotment, Request, Limits).

-spec maybe_consume_allotment('undefined'| wh_json:object(), j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_consume_allotment('undefined', Request, _) ->
    lager:debug("account has no allotment", []),
    Request;
maybe_consume_allotment(Allotment, Request, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    case allotment_consumed_so_far(Allotment, Limits) of
        {'error', _R} -> Request;
        Consumed when Consumed > (Amount - 60) ->
            lager:debug("account ~s has used all ~ws of their allotment"
                        ,[AccountId, Amount]),
            Request;
        Consumed ->
            lager:debug("account ~s has ~ws remaining of their allotment"
                        ,[AccountId, Amount - Consumed]),
            Classification = wh_json:get_value(<<"classification">>, Allotment),
            j5_request:authorize(<<"allotment_", Classification/binary>>, Request, Limits)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(Request, Limits) ->
    case j5_request:billing(Request, Limits) of
        <<"allotment_", _/binary>> -> maybe_reconcile_allotment(Request, Limits);
        _Else -> 'ok'
    end.

-spec maybe_reconcile_allotment(j5_request:request(), j5_limits:limits()) -> 'ok'.
maybe_reconcile_allotment(Request, Limits) ->
    case try_find_allotment(Request, Limits) of
        'undefined' -> 'ok';
        Allotment ->
            BillingSeconds = j5_request:billing_seconds(Request),
            reconcile_allotment(BillingSeconds, Allotment, Request, Limits)
    end.

-spec reconcile_allotment(non_neg_integer(), wh_json:object(), j5_request:request(), j5_limits:limits()) ->
                                 'ok'.
reconcile_allotment(0, _, _, _) -> 'ok';
reconcile_allotment(Seconds, Allotment, Request, Limits) ->
    CallId = j5_request:call_id(Request),
    AccountId = j5_limits:account_id(Limits),
    LedgerDb = wh_util:format_account_mod_id(AccountId),
    Timestamp = wh_util:current_tstamp(),
    Id = <<CallId/binary, "-allotment-consumption">>,
    lager:debug("adding allotment debit ~s to ledger ~s for ~wsec"
                ,[Id, LedgerDb, Seconds]),
    Props = [{<<"_id">>, Id}
             ,{<<"account_id">>, AccountId}
             ,{<<"seconds">>, abs(Seconds)}
             ,{<<"call_id">>, CallId}
             ,{<<"name">>, wh_json:get_value(<<"name">>, Allotment)}
             ,{<<"classification">>, wh_json:get_value(<<"classification">>, Allotment)}
             ,{<<"pvt_created">>, Timestamp}
             ,{<<"pvt_modified">>, Timestamp}
             ,{<<"pvt_vsn">>, 1}
             ,{<<"pvt_whapp">>, ?APP_NAME}
             ,{<<"pvt_type">>, <<"allotment_consumption">>}
            ],
    _ = couch_mgr:save_doc(LedgerDb, wh_json:from_list(Props)),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec try_find_allotment(j5_request:request(), j5_limits:limits()) -> api_object().
try_find_allotment(Request, Limits) ->
    case j5_request:classification(Request) of
        'undefined' -> 'undefined';
        Classification -> try_find_allotment_classification(Classification, Limits)
    end.

-spec try_find_allotment_classification(ne_binary(), j5_limits:limits()) -> api_object().
try_find_allotment_classification(Classification, Limits) ->
    Allotments = j5_limits:allotments(Limits),
    lager:debug("checking if account ~s has any allotments for ~s"
                ,[j5_limits:account_id(Limits)
                  ,Classification
                 ]),
    case wh_json:get_value(Classification, Allotments) of
        'undefined' -> 'undefined';
        Allotment -> wh_json:set_value(<<"classification">>, Classification, Allotment)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allotment_consumed_so_far(wh_json:object(), j5_limits:limits()) ->
                                       integer() |
                                       {'error', _}.
allotment_consumed_so_far(Allotment, Limits) ->
    Classification = wh_json:get_value(<<"classification">>, Allotment),
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    CycleStart = cycle_start(Cycle),
    CycleSpan = cycle_span(Cycle),
    case allotment_consumed_so_far(CycleStart, Classification, Limits, 0) of
        {'error', _}=Error -> Error;
        Consumed ->
            Consumed + j5_channels:allotment_consumed(CycleStart, CycleSpan, Classification, Limits)
    end.

-spec allotment_consumed_so_far(non_neg_integer(), ne_binary(), j5_limits:limits(), 0..3) ->
                                       integer() |
                                       {'error', _}.
allotment_consumed_so_far(_, _, _, Attempts) when Attempts > 2 -> 0;
allotment_consumed_so_far(CycleStart, Classification, Limits, Attempts) ->
    AccountId = j5_limits:account_id(Limits),
    LedgerDb = wh_util:format_account_mod_id(AccountId),
    ViewOptions = [{'startkey', [Classification, CycleStart]}
                   ,{'reduce', 'false'}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"allotments/consumed">>, ViewOptions) of
        {'error', 'not_found'} ->
            add_transactions_view(LedgerDb, CycleStart, Classification, Limits, Attempts);
        {'error', _R}=Error ->
            lager:debug("unable to get consumed quanity for ~s allotment from ~s: ~p"
                        ,[Classification, LedgerDb, _R]),
            Error;
        {'ok', JObjs} -> sum_allotment_consumed_so_far(JObjs, CycleStart)
    end.

-spec sum_allotment_consumed_so_far(wh_json:objects(), non_neg_integer()) -> non_neg_integer().
sum_allotment_consumed_so_far(JObjs, CycleStart) ->
    sum_allotment_consumed_so_far(JObjs, CycleStart, 0).

-spec sum_allotment_consumed_so_far(wh_json:objects(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
sum_allotment_consumed_so_far([], _, Seconds) -> Seconds;
sum_allotment_consumed_so_far([JObj|JObjs], CycleStart, Seconds) ->
    [_, Timestamp] = wh_json:get_value(<<"key">>, JObj),
    Duration = wh_json:get_value(<<"value">>, JObj),
    case (Timestamp - Duration) > CycleStart of
        'true' ->
            sum_allotment_consumed_so_far(JObjs, CycleStart, Seconds + Duration);
        'false' ->
            sum_allotment_consumed_so_far(JObjs, CycleStart, Seconds + (Timestamp - CycleStart))
    end.

-spec add_transactions_view(ne_binary(), non_neg_integer(), ne_binary(), j5_limits:limits(), 0..3) ->
                                   integer() |
                                   {'error', _}.
add_transactions_view(LedgerDb, CycleStart, Classification, Limits, Attempts) ->
    _ = couch_mgr:revise_views_from_folder(LedgerDb, 'jonny5'),
    allotment_consumed_so_far(CycleStart, Classification, Limits, Attempts + 1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cycle_start(ne_binary()) -> integer().
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

-spec cycle_span(ne_binary()) -> integer().
cycle_span(<<"monthly">>) -> 2629743; % avg days in month
cycle_span(<<"weekly">>) -> ?SECONDS_IN_WEEK;
cycle_span(<<"daily">>) -> ?SECONDS_IN_DAY;
cycle_span(<<"hourly">>) -> ?SECONDS_IN_HOUR;
cycle_span(<<"minutely">>) -> 60.
