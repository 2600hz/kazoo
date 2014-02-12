%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_allotments).

-export([authorize/2]).
-export([reauthorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec authorize(j5_request(), j5_limits()) -> j5_request().
authorize(Request, Limits) ->
    Allotment = try_find_allotment(Request, Limits),
    maybe_consume_allotment(Allotment, Request, Limits).

-spec reauthorize(j5_request(), j5_limits()) -> j5_request().
reauthorize(Request, Limits) ->
    %% Until the call has been answered for 60 seconds we have
    %% a reservation from the authz_req
    AnsweredTime = j5_request:answered_time(Request),
    Timestamp = j5_request:timestamp(Request),
    case (Timestamp - AnsweredTime) > 55 of
        'false' -> j5_request:authorize(<<"allotment">>, Request, Limits);
        'true' ->
            Allotment = try_find_allotment(Request, Limits),
            maybe_tick_allotment_consumption(Allotment, Request, Limits)
    end.

-spec reconcile_cdr(j5_request(), j5_limits()) -> 'ok'.
reconcile_cdr(Request, Limits) ->
    BillingSeconds = j5_request:billing_seconds(Request), 
    case j5_request:billing(Request, Limits) of
        <<"allotment">> when BillingSeconds =< 0 ->
            release_unsed_allotment(Request, Limits);
        %% TODO: reconcile usage for systems not hearbeating...
        _Else -> 'ok'
    end.

-spec release_unsed_allotment(j5_request(), j5_limits()) -> 'ok'.
release_unsed_allotment(Request, Limits) ->
    case try_find_allotment(Request, Limits) of
        'undefined' -> 'ok';
        Allotment ->
            return_allotment_consumption(Allotment, 60, Request, Limits)
    end.

-spec maybe_tick_allotment_consumption('undefined' | wh_json:object(), j5_request(), j5_limits()) -> j5_request().
maybe_tick_allotment_consumption('undefined', _, _) -> 'ok';
maybe_tick_allotment_consumption(Allotment, Request, Limits) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    case allotment_consumed_sofar(Allotment, Limits) of
        Consumed when (Amount - Consumed) =< 60 ->
            lager:debug("allotment consumed, transitioning to per_minute", []),
            %% TODO: how to send billing-seconds-offset?
            %% CCVs = wh_json:from_list([{<<"Billing-Seconds-Offset">>, (Timestamp - Answered) + 62}]),

            %% Since we are authing the last 60 seconds take it the last
            %% of the allotment and move to per_minute (or flat_rate)...
            tick_allotment_consumption(Allotment, 60, Request, Limits),
            Request;
        _Else ->
            tick_allotment_consumption(Allotment, 60, Request, Limits),
            j5_request:authorize(<<"allotment">>, Request, Limits)
    end.

-spec try_find_allotment(j5_request(), j5_limits()) -> wh_json:object() | 'undefined'.
try_find_allotment(Request, #limits{allotments=Allotments
                                    ,account_id=AccountId}) ->
    case wnm_util:classify_number(j5_request:number(Request)) of
        'undefined' -> 'undefined';
        Classification ->
            lager:debug("checking if account ~s has any allotments for ~s"
                        ,[AccountId, Classification]),
            wh_json:get_value(Classification, Allotments)
    end.

-spec maybe_consume_allotment('undefined'| wh_json:object(), j5_request(), j5_limits()) -> j5_request().
maybe_consume_allotment('undefined', Request, _) -> 
    lager:debug("account has no allotments", []),
    Request;
maybe_consume_allotment(Allotment, Request, #limits{account_id=AccountId}=Limits) ->
    Amount = wh_json:get_integer_value(<<"amount">>, Allotment, 0),
    case allotment_consumed_sofar(Allotment, Limits) of
        {'error', _R} -> Request;
        Consumed when Consumed > (Amount - 60) ->
            lager:debug("account ~s has used all ~wsecs of their allotment"
                        ,[AccountId, Amount]),
            Request;
        _Else ->
            start_allotment_consumption(Allotment, 60, Request, Limits),
            j5_request:authorize(<<"allotment">>, Request, Limits)
    end.

-spec allotment_consumed_sofar(wh_json:object(), j5_limits()) -> integer() | {'error', _}.
allotment_consumed_sofar(Allotment, Limits) ->
    allotment_consumed_sofar(Allotment, Limits, 0).

-spec allotment_consumed_sofar(wh_json:object(), j5_limits(), 0..3) -> integer() | {'error', _}.
allotment_consumed_sofar(_, _, Attempts) when Attempts > 2 -> {'error', 'not_found'};
allotment_consumed_sofar(Allotment, #limits{account_id=AccountId}=Limits, Attempts) ->
    LedgerDb = wh_util:format_account_mod_id(AccountId),
    Name = wh_json:get_value(<<"name">>, Allotment),
    Cycle = wh_json:get_ne_value(<<"cycle">>, Allotment, <<"monthly">>),
    ViewOptions = [{'startkey', [Name, cycle_start(Cycle)]}
                   ,'group'
                   ,{'group_level', 1}
                   ,'reduce'
                  ],
    case couch_mgr:get_results(LedgerDb, <<"allotments/consumed">>, ViewOptions) of
        {'error', 'not_found'} -> 
            add_transactions_view(LedgerDb, Allotment, Limits, Attempts);
        {'error', _R}=Error ->
            lager:debug("unable to get ~s allotment ~s for ~s: ~p"
                        ,[Cycle, Name, LedgerDb, _R]),
            Error;
        {'ok', []} -> 
            lager:debug("~s allotment ~s for ~s at 0"
                        ,[Cycle, Name, LedgerDb]),
            0;
        {'ok', [JObj|_]} ->
            Consumed = abs(wh_json:get_integer_value(<<"value">>, JObj, 0)),
            lager:debug("~s allotment ~s for ~s at ~w"
                        ,[Cycle, Name, LedgerDb, Consumed]),
            Consumed
    end.

-spec add_transactions_view(ne_binary(), wh_json:object(), j5_limits(), 0..3) -> integer() | {'error', _}.
add_transactions_view(LedgerDb, Allotment, Limits, Attempts) ->
    _ = couch_mgr:revise_views_from_folder(LedgerDb, 'jonny5'),
    allotment_consumed_sofar(Allotment, Limits, Attempts + 1).

-spec cycle_start(ne_binary()) -> integer().
cycle_start(<<"monthly">>) ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}});
cycle_start(<<"weekly">>) ->
    {Date, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) - (calendar:day_of_the_week(Date) - 1) * 86400;
cycle_start(<<"daily">>) ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}});
cycle_start(<<"hourly">>) ->
    {{Year, Month, Day}, {Hour, _, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}});
cycle_start(<<"minutely">>) ->
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, 0}}).

-spec start_allotment_consumption(wh_json:object(), integer(), j5_request(), j5_limits()) -> wh_jobj_return().
start_allotment_consumption(Allotment, Units, Request, Limits) ->
    CallId = j5_request:call_id(Request),
    Props = [{<<"_id">>, <<CallId/binary, "-start">>}
             ,{<<"call_id">>, CallId}
             ,{<<"name">>, wh_json:get_value(<<"name">>, Allotment)}
             ,{<<"reason">>, <<"allotment_channel">>}
             ,{<<"pvt_type">>, <<"allotment_debit">>}
            ],
    write_to_ledger(Props, Units, Limits).

-spec tick_allotment_consumption(wh_json:object(), integer(), j5_request(), j5_limits()) -> wh_jobj_return().
tick_allotment_consumption(Allotment, Units, Request, Limits) ->
    CallId = j5_request:call_id(Request),
    Timestamp = j5_request:timestamp(Request),
    Props = [{<<"_id">>, <<CallId/binary, "-"
                           ,(wh_util:to_binary(Timestamp))/binary>>}
             ,{<<"call_id">>, CallId}
             ,{<<"name">>, wh_json:get_value(<<"name">>, Allotment)}
             ,{<<"reason">>, <<"allotment_channel">>}
             ,{<<"pvt_type">>, <<"allotment_debit">>}
            ],
    write_to_ledger(Props, Units, Limits).

-spec return_allotment_consumption(wh_json:object(), integer(), j5_request(), j5_limits()) -> wh_jobj_return().
return_allotment_consumption(Allotment, Units, Request, Limits) ->
    CallId = j5_request:call_id(Request),
    Props = [{<<"_id">>, <<CallId/binary, "-stop">>}
             ,{<<"call_id">>, CallId}
             ,{<<"name">>, wh_json:get_value(<<"name">>, Allotment)}
             ,{<<"reason">>, <<"allotment_channel">>}
             ,{<<"pvt_type">>, <<"allotment_credit">>}
            ],
    write_to_ledger(Props, Units, Limits).

-spec write_to_ledger(proplist(), integer(), j5_limits()) -> wh_jobj_return().
write_to_ledger(Props, Units, #limits{account_id=AccountId}) ->
    LedgerDb = wh_util:format_account_mod_id(AccountId),
    Timestamp = wh_util:current_tstamp(),
    lager:debug("adding ~s ~s to ledger ~s for ~wsec"
                ,[props:get_value(<<"pvt_type">>, Props)
                  ,props:get_value(<<"_id">>, Props)
                  ,LedgerDb
                  ,Units
                 ]),
    JObj = wh_json:from_list(
             [{<<"account_id">>, AccountId}
              ,{<<"amount">>, abs(Units)}
              ,{<<"pvt_created">>, Timestamp}
              ,{<<"pvt_modified">>, Timestamp}
              ,{<<"pvt_vsn">>, 1}
              ,{<<"pvt_whapp">>, ?APP_NAME}
              | Props
             ]),
    couch_mgr:save_doc(LedgerDb, JObj).
