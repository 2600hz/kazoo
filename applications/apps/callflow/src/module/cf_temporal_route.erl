%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 8 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_temporal_route).

-include("../callflow.hrl").

-export([handle/2, test/4]).

-import(whapps_json, [get_value/2, get_value/3]).
-import(whistle_util, [to_integer/1]).
-import(logger, [format_log/3]).

-import(calendar, [
                    day_of_the_week/1, day_of_the_week/3, gregorian_seconds_to_datetime/1
                   ,datetime_to_gregorian_seconds/1, date_to_gregorian_days/1, date_to_gregorian_days/3
                   ,last_day_of_the_month/2, gregorian_days_to_date/1, universal_time/0, iso_week_number/1
                  ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    CFPid ! find_temporal_routes(whapps_json:get_value(<<"database">>, Data)).

find_temporal_routes(Db) ->    
    case couch_mgr:get_all_results(Db, <<"temporal-route/listing_by_occurence">>) of
        {ok, JObj} ->
            format_log(info, "FOUND: ~p", [JObj]),
            {continue, <<"match">>};
        _ ->
            {continue, <<"no_match">>}
    end.

test(Rule, Every, Modifier, Count) ->
    T0 = current_date(),  
    lists:foldr(fun(Seq, T1) ->
                        T = event_date(Rule, Every, Modifier, T0, T1),
                        format_log(info, "~p) ~p", [Seq, T]),
                        T
                end, T0, lists:seq(Count, 1, -1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
event_date("daily", 1, _, _, {Y1, M1, D1}) ->
    normalize_date({Y1, M1, D1 + 1});

event_date("daily", I0, _, {Y0, M0, D0}, {Y1, M1, D1}) ->
    DS0 = date_to_gregorian_days({Y0, M0, D0}),
    DS1 = date_to_gregorian_days({Y1, M1, D1}),
    Offset = trunc( ( DS1 - DS0 ) / I0 ) * I0,
    normalize_date({Y0, M0, D0 + Offset + I0});

event_date("weekly", 1, DOWs, _, {Y1, M1, D1}) ->
    DOW1 = day_of_the_week({Y1, M1, D1}),
    Offset = hd([ DOW || DOW <- [ to_dow(D) || D <- DOWs ], DOW > DOW1 ]
                ++ [ to_dow( hd( DOWs ) ) + 7 ])
               - DOW1,     
    normalize_date({Y1, M1, D1 + Offset});

event_date("weekly", I0, DOWs, {Y0, M0, D0}, {Y1, M1, D1}) ->
    DOW1 = day_of_the_week({Y1, M1, D1}),
    case [ DOW || DOW <- [to_dow(D) || D <- DOWs], DOW > DOW1 ] of 
        [] ->            
            W0 = iso_week_number({Y0, M0, D0}),
            W1 = iso_week_number({Y1, M1, D1}),
            Offset = trunc( iso_week_difference(W0, W1) / I0 ) * I0,
            {Y2, M2, D2} = iso_week_to_gregorian_date({element(1, W0), element(2, W0) + Offset + I0}),
            normalize_date({Y2, M2, D2 - 1 + to_dow( hd( DOWs ) )});
        [Offset|_] ->
            normalize_date({Y1, M1, D1 + Offset - DOW1})
    end;

event_date("monthly", 1, {every, DOW0}, _, {Y1, M1, D1}) ->
    DOW1 = to_dow(DOW0),
    case calendar:day_of_the_week({Y1, M1, D1}) of
        DOW1 ->
            normalize_date({Y1, M1, D1 + 7});
        D ->
            Offset = ( 7 - D ) + DOW1,
            normalize_date({Y1, M1, D1 + Offset})
    end;

event_date("monthly", 1, {last, DOW0}, _, {Y1, M1, _}) ->
    try
        date_of_dow(Y1, M1, to_dow(DOW0), 4)
    catch 
        _:_ -> 
            date_of_dow(Y1, M1, to_dow(DOW0), 3)
    end;

event_date("monthly", 1, {When, DOW0}, _, {Y1, M1, _}) ->
    find_next_weekday(Y1, M1, convert_when(When), to_dow(DOW0));

event_date("monthly", 1, Days, _, {Y1, M1, D1}) when is_list(Days) ->
    Offset = hd([D || D <- Days, D > D1]
                ++ [last_day_of_the_month(Y1, M1) + hd(Days)]),
    normalize_date({Y1, M1, Offset});

event_date("monthly", I0, Days, {Y0, M0, _}, {Y1, M1, D1}) when is_list(Days) ->
    case [D || D <- Days, D > D1] of
        [] ->
            Distance = ( Y1 - Y0 ) * 12 + M0 - M1,
            Offset = trunc( Distance / I0 ) * I0,
            normalize_date({Y1, M0 + Offset + I0, hd( Days )});
        [Day|_] ->
            normalize_date({Y1, M1, Day})
    end;

event_date("monthly", I0, {When, DOW0}, {Y0, M0, _}, {Y1, M1, D1}) ->
    Distance = ( Y1 - Y0 ) * 12 + abs(M0 - M1),
    Offset = trunc( Distance / I0 ) * I0,
    find_next_weekday(Y1, M0 + Offset + I0, convert_when(When), to_dow(DOW0));

event_date(_, _, _, _, _) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(normalize_date/1 :: (Date :: date()) -> non_neg_integer()).
normalize_date({Y, M, D}) when M =:= 13 -> 
    normalize_date({Y + 1, 1, D});
normalize_date({Y, M, D}) when M > 12 ->
    normalize_date({Y + 1, M - 12, D});
normalize_date({Y, M, D}=Date) -> 
    case last_day_of_the_month(Y, M) of
        Days when D > Days ->
            normalize_date({Y, M + 1, D - Days});
        _ ->  
            Date
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(current_date/0 :: () -> date()).
current_date() ->
    {Date, _} = localtime:utc_to_local(universal_time(), "America/Los_Angeles"),
    Date.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(current_date_in_seconds/0 :: () -> seconds()).
current_date_in_seconds() ->
    datetime_to_gregorian_seconds({current_date(), {0, 0, 0}}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
convert_when(first) -> 0;
convert_when(second) -> 1;    
convert_when(third) -> 2;
convert_when(fourth) -> 3;
convert_when(fifth) -> 4.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
to_dow(monday) -> 1;
to_dow(tuesday) -> 2;
to_dow(wensday) -> 3;
to_dow(thursday) -> 4;
to_dow(friday) -> 5;
to_dow(saturday) -> 6; 
to_dow(sunday) -> 7.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
find_next_weekday(Y1, M1, When, Weekday) when M1 =:= 13 -> 
    find_next_weekday(Y1 + 1, 1, When, Weekday);
find_next_weekday(Y1, M1, When, Weekday) when M1 > 12 ->
    find_next_weekday(Y1 + 1, M1 - 12, When, Weekday);
find_next_weekday(Y1, M1, When, Weekday) ->
    try
        date_of_dow(Y1, M1, Weekday, When)
    catch
        _:_ ->
            find_next_weekday(Y1, M1 + 1, When, Weekday)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(date_of_dow/4 :: (Year :: non_neg_integer(), Month :: non_neg_integer()
                       ,DOW :: day_of_week(), Occurance :: integer()) -> non_neg_integer()).
date_of_dow(Year, 1, DOW, Occurance) ->
    date_of_dow(Year - 1, 13, DOW, Occurance);
date_of_dow(Year, Month, DOW, Occurance) ->
    RefDate = {Year, Month - 1, last_day_of_the_month(Year, Month - 1)},
    RefDays = date_to_gregorian_days(RefDate),     
    Days = case day_of_the_week(RefDate) of                
               DOW ->
                   RefDays + 7 + (7 * Occurance);
               RefDOW when DOW < RefDOW ->
                   RefDays + DOW + (7 - RefDOW) + (7 * Occurance);
               RefDOW ->
                   RefDays + abs(DOW - RefDOW) + (7 * Occurance)
          end,
    {Year, Month, Day} = gregorian_days_to_date(Days),    
    normalize_date({Year, Month, Day}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(iso_week_difference/2 :: (Week0 :: iso_week(), Week1 :: iso_week()) -> non_neg_integer()).                                    
iso_week_difference({Y0, W0}, {Y0, W1}) ->
    abs( W0 - W1 );
iso_week_difference({Y0, W0}, {Y1, W1}) when Y1 - Y0 == 1->
    {Y0, W} = iso_week_number({Y0, 12, last_day_of_the_month(Y0, 12)}),
    ( W - W0 ) + W1;
iso_week_difference({Y0, W0}, {Y1, W1}) ->
    {_, Partial} = iso_week_number({Y0, 12, last_day_of_the_month(Y0, 12)}),    
    lists:foldr(fun(Y, Acc) ->
                        case iso_week_number({Y, 12, last_day_of_the_month(Y, 12)}) of
                            {_, 1} -> 52 + Acc;
                            {Y, W} -> W + Acc
                        end
                end, abs(Partial - W0), lists:seq(Y0 + 1, Y1)) + W1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(iso_week_to_gregorian_date/1 :: (Date :: iso_week()) -> date()).                                            
iso_week_to_gregorian_date({Year, Week}) ->
    Jan1 = date_to_gregorian_days(Year, 1, 1),
    Offset = 4 - day_of_the_week(Year, 1, 4),
    Days =
        if
            Offset =:= 0 ->
                Jan1 + ( Week * 7 );
            true ->
                Jan1 + Offset + ( ( Week - 1 ) * 7 )
        end,
    gregorian_days_to_date(Days).
