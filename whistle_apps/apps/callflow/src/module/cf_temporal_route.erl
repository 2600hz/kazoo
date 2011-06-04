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

-export([handle/2]).

-import(calendar, [
                    day_of_the_week/1, day_of_the_week/3, gregorian_seconds_to_datetime/1
                   ,datetime_to_gregorian_seconds/1, date_to_gregorian_days/1, date_to_gregorian_days/3
                   ,last_day_of_the_month/2, gregorian_days_to_date/1, universal_time/0
                  ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(continue, binary())).
handle(_, #cf_call{cf_pid=CFPid}=Call) ->
    CFPid ! find_temporal_routes(Call).

find_temporal_routes(#cf_call{account_db=Db}) ->
    case couch_mgr:get_all_results(Db, <<"temporal-route/listing_by_occurence">>) of
        {ok, JObj} ->
            logger:format_log(info, "FOUND: ~p", [JObj]),
            {continue, <<"match">>};
        _ ->
            {continue, <<"no_match">>}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The big daddy 
%% Calculates the date of the next event given the type, interval, 
%% rule, start date, and current date.
%% @end
%%--------------------------------------------------------------------
event_date("daily", 1, _, _, {Y1, M1, D1}) ->
    %% add one and fix 
    normalize_date({Y1, M1, D1 + 1});

event_date("daily", I0, _, {Y0, M0, D0}, {Y1, M1, D1}) ->
    %% Calculate the distance in days as a function of
    %%   the interval and fix
    DS0 = date_to_gregorian_days({Y0, M0, D0}),
    DS1 = date_to_gregorian_days({Y1, M1, D1}),
    Offset = trunc( ( DS1 - DS0 ) / I0 ) * I0,
    normalize_date({Y0, M0, D0 + Offset + I0});

event_date("weekly", 1, DOWs, _, {Y1, M1, D1}) ->
    DOW1 = day_of_the_week({Y1, M1, D1}),
    %% Take the head of a list of DOWs after removing any up to 
    %%   and including the current DOW (DOW1).
    %% Also append the first DOW plus 7 
    %%  (ie: the occurance next week of that DOW)
    Offset = hd([ DOW || DOW <- [ to_dow(D) || D <- DOWs ], DOW > DOW1 ]
                ++ [ to_dow( hd( DOWs ) ) + 7 ])
               - DOW1,
    normalize_date({Y1, M1, D1 + Offset});

event_date("weekly", I0, DOWs, {Y0, M0, D0}, {Y1, M1, D1}) ->
    DOW1 = day_of_the_week({Y1, M1, D1}),
    W0 = iso_week_number({Y0, M0, D0}),
    W1 = iso_week_number({Y1, M1, D1}),
    WeekDiff = iso_week_difference(W0, W1),
    case [ DOW || DOW <- [to_dow(D) || D <- DOWs], DOW > DOW1 ] of
        %% During an 'active' week but before the last DOW move to the next day this week
        [Offset|_] when WeekDiff =:= 0; WeekDiff rem I0 =:= 0 ->
            normalize_date({Y1, M1, D1 + Offset - DOW1}); 
        %% Empty list:
        %%   The last DOW during an 'active' week, 
        %% Non Empty List that failed the guard:
        %%   During an 'inactive' week
        _ ->
            Offset = trunc( WeekDiff / I0 ) * I0,
            {Y2, M2, D2} = iso_week_to_gregorian_date({element(1, W0), element(2, W0) + Offset + I0}),
            normalize_date({Y2, M2, ( D2 - 1 ) + to_dow( hd( DOWs ) )})
    end;

event_date("monthly", 1, {every, DOW0}, _, {Y1, M1, D1}) ->
    DOW1 = to_dow(DOW0),
    case calendar:day_of_the_week({Y1, M1, D1}) of
        DOW1 ->
            %% Today is the DOW we wanted, calculate to next week 
            normalize_date({Y1, M1, D1 + 7});
        D when DOW1 > D ->
            %% If the DOW has not occured this week yet
            normalize_date({Y1, M1, D1 + (DOW1 - D)});            
        D ->
            %% If the DOW occurance has already happend, calculate to next week
            Offset = ( 7 - D ) + DOW1,
            normalize_date({Y1, M1, D1 + Offset})
    end;

event_date("monthly", 1, {last, DOW0}, _, {Y1, M1, _}) ->
    try
        %% The max number of weeks a month can ever have is four.
        %% This will throw if it doesnt or the return is corrected
        %% by moving to the next month, in which case we know the
        %% last DOW. Either way the last DOW would occur in the third week.
        {Y1, M1, _} = date_of_dow(Y1, M1, to_dow(DOW0), 4)
    catch
        _:_ ->
            date_of_dow(Y1, M1, to_dow(DOW0), 3)
    end;

event_date("monthly", 1, {When, DOW0}, _, {Y1, M1, _}) ->
    find_next_weekday(Y1, M1, from_ordinal(When), to_dow(DOW0));

event_date("monthly", 1, Days, _, {Y1, M1, D1}) when is_list(Days) ->
    %% Take the head of a list of Days after removing any up to 
    %%   and including the current Day (D1).
    %% Also append the first Day plus the last day of the month 
    %%  (ie: the occurance of that day next month).
    Offset = hd([D || D <- Days, D > D1]
                ++ [last_day_of_the_month(Y1, M1) + hd(Days)]),
    normalize_date({Y1, M1, Offset});

event_date("monthly", I0, Days, {Y0, M0, _}, {Y1, M1, D1}) when is_list(Days) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case [D || D <- Days, D > D1] of
        %% The day hasn't happend on an 'active' month
        [Day|_] when Distance =:= Offset ->
            normalize_date({Y0, M0 + Offset, Day});
        _ -> 
            %% Empty List:
            %%   All of the days in the list have already happened
            %% Non Empty List that failed the guard:
            %%   The day hasn't happend on an 'inactive' month
            normalize_date({Y0, M0 + Offset + I0, hd( Days )})
    end;

event_date("monthly", I0, {When, DOW0}, {Y0, M0, _}, {Y1, M1, _D1}) ->
    Distance = ( Y1 - Y0 ) * 12 + abs(M0 - M1),
    Offset = trunc( Distance / I0 ) * I0,
    find_next_weekday(Y0, M0 + Offset + I0, from_ordinal(When), to_dow(DOW0));

event_date(_, _, _, _, _) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes dates, for example corrects for months that are given
%% with more days then they have (ie: {2011, 1, 36} -> {2011, 2, 5}).
%% @end
%%--------------------------------------------------------------------
-spec(normalize_date/1 :: (Date :: date()) -> date()).
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
%% Convert the ordinal words to cardinal numbers representing
%% the position 
%% @end
%%--------------------------------------------------------------------
from_ordinal(first) -> 0;
from_ordinal(second) -> 1;
from_ordinal(third) -> 2;
from_ordinal(fourth) -> 3;
from_ordinal(fifth) -> 4.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Map the days of the week to cardinal numbers representing the 
%% position, in accordance with ISO 8601
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
%% Safety wrapper on date_of_dow used to loop over failing attempts
%% until the date can be calculated.
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
%% Unsafe calculation of the date for a specific day of the week, this
%% function will explode on occasion. 
%% @end
%%--------------------------------------------------------------------
-spec(date_of_dow/4 :: (Year :: non_neg_integer(), Month :: 2..13
                       ,DOW :: 1..7, Occurance :: 0..4) -> date()).
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
    {Y, M, D} = gregorian_days_to_date(Days),
    normalize_date({Y, M, D}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates the distance, in total weeks, between two ISO 8601 weeks
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
                end, abs(Partial - W0), lists:seq(Y0 + 1, Y1 - 1)) + W1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Caclulates the gregorian date of a given ISO 8901 week
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for calender:iso_week_number introduced in R14B02 (?) using
%% a local copy on older systems
%% @end
%%--------------------------------------------------------------------
iso_week_number(Date) ->
    case erlang:function_exported(calendar, iso_week_number, 1) of
	true -> calendar:iso_week_number(Date);
	false -> our_iso_week_number(Date)
    end.

%% TAKEN FROM THE R14B02 SOURCE FOR calender.erl
our_iso_week_number({Year,_Month,_Day}=Date) ->
    D = calendar:date_to_gregorian_days(Date),
    W01_1_Year = gregorian_days_of_iso_w01_1(Year),
    W01_1_NextYear = gregorian_days_of_iso_w01_1(Year + 1),
    if W01_1_Year =< D andalso D < W01_1_NextYear ->
	    % Current Year Week 01..52(,53)
	    {Year, (D - W01_1_Year) div 7 + 1};
	D < W01_1_Year ->
	    % Previous Year 52 or 53
	    PWN = case calender:day_of_the_week(Year - 1, 1, 1) of
		4 -> 53;
		_ -> case calendar:day_of_the_week(Year - 1, 12, 31) of
			4 -> 53;
			_ -> 52
		     end
		end,
	    {Year - 1, PWN};
	W01_1_NextYear =< D ->
	    % Next Year, Week 01
	    {Year + 1, 1}
    end.

-spec gregorian_days_of_iso_w01_1(calendar:year()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
    D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
    DOW = calendar:day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
	D0101 - DOW + 1;
    true ->
	D0101 + 7 - DOW + 1
    end.


-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% RECURRENCE TESTS
%%
%% Tests date predictions for events that recur
%% @end
%%--------------------------------------------------------------------
daily_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011, 1, 2}, event_date("daily", 1, undefined, {2011, 1, 1}, {2011, 1, 1})),     
    ?assertEqual({2011, 6, 2}, event_date("daily", 1, undefined, {2011, 6, 1}, {2011, 6, 1})),
    %%  increment over month boundary
    ?assertEqual({2011, 2, 1}, event_date("daily", 1, undefined, {2011, 1, 1}, {2011, 1, 31})),
    ?assertEqual({2011, 7, 1}, event_date("daily", 1, undefined, {2011, 6, 1}, {2011, 6, 30})),
    %% increment over year boundary
    ?assertEqual({2011, 1, 1}, event_date("daily", 1, undefined, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 1, 1}, event_date("daily", 1, undefined, {2010, 6, 1}, {2010, 12, 31})),
    %% leap year (into)
    ?assertEqual({2008, 2, 29}, event_date("daily", 1, undefined, {2008, 1, 1}, {2008, 2, 28})),
    ?assertEqual({2008, 2, 29}, event_date("daily", 1, undefined, {2008, 6, 1}, {2008, 2, 28})),
    %% leap year (over)
    ?assertEqual({2008, 3, 1}, event_date("daily", 1, undefined, {2008, 1, 1}, {2008, 2, 29})),
    ?assertEqual({2008, 3, 1}, event_date("daily", 1, undefined, {2008, 6, 1}, {2008, 2, 29})),
    %% shift start date (no impact)
    ?assertEqual({2011, 1, 2}, event_date("daily", 1, undefined, {2008, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("daily", 1, undefined, {2009, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("daily", 1, undefined, {2010, 1, 1}, {2011, 1, 1})),
    %% even step (small)
    ?assertEqual({2011, 1, 5}, event_date("daily", 4, undefined, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 2}, event_date("daily", 4, undefined, {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 1, 4}, event_date("daily", 4, undefined, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 6, 5}, event_date("daily", 4, undefined, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 7, 3}, event_date("daily", 4, undefined, {2011, 6, 1}, {2011, 6, 29})),
    %% odd step (small)
    ?assertEqual({2011, 1, 8}, event_date("daily", 7, undefined, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 5}, event_date("daily", 7, undefined, {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 1, 7}, event_date("daily", 7, undefined, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 6, 8}, event_date("daily", 7, undefined, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 7, 6}, event_date("daily", 7, undefined, {2011, 6, 1}, {2011, 6, 29})),
    %% even step (large)
    ?assertEqual({2011, 2, 18}, event_date("daily", 48, undefined, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 20}, event_date("daily", 48, undefined, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 7, 19}, event_date("daily", 48, undefined, {2011, 6, 1}, {2011, 6, 1})),
    %% odd step (large)
    ?assertEqual({2011, 3, 27}, event_date("daily", 85, undefined, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 3, 2}, event_date("daily", 85, undefined, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 8, 25}, event_date("daily", 85, undefined, {2011, 6, 1}, {2011, 6, 1})),
    %% shift start date (past)
    ?assertEqual({2011, 2, 5}, event_date("daily", 4, undefined, {2011, 2, 1}, {2011, 2, 3})),
    ?assertEqual({2011, 2, 6}, event_date("daily", 4, undefined, {2011, 2, 2}, {2011, 2, 3})),
    %% current date before
    ?assertEqual({2011, 1, 5}, event_date("daily", 4, undefined, {2011, 1, 5}, {2011, 1, 1})),
    %% current date on
    ?assertEqual({2011, 1, 9}, event_date("daily", 4, undefined, {2011, 1, 5}, {2011, 1, 5})),
    %% current date after
    ?assertEqual({2011, 1, 9}, event_date("daily", 4, undefined, {2011, 1, 5}, {2011, 1, 6})),
    %% long span
    ?assertEqual({2011, 1, 2}, event_date("daily", 4, undefined, {1983, 4, 11}, {2011, 1, 1})),
    ?assertEqual({2011, 4, 12}, event_date("daily", 4, undefined, {1983, 4, 11}, {2011, 4, 11})).

weekly_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 4}, event_date("weekly", 1, [tuesday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 1, [wensday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 1, [thursday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 1, [friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 8}, event_date("weekly", 1, [saturday], {2011, 1, 1}, {2011, 1, 1})), %% 1st is a sat
    ?assertEqual({2011, 1, 2}, event_date("weekly", 1, [sunday], {2011, 1, 1}, {2011, 1, 1})),
    %%  increment over month boundary
    ?assertEqual({2011, 2, 7}, event_date("weekly", 1, [monday], {2011, 1, 1}, {2011, 1, 31})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 1, [tuesday], {2011, 1, 1}, {2011, 1, 25})),
    ?assertEqual({2011, 2, 2}, event_date("weekly", 1, [wensday], {2011, 1, 1}, {2011, 1, 26})),
    ?assertEqual({2011, 2, 3}, event_date("weekly", 1, [thursday], {2011, 1, 1}, {2011, 1, 27})),
    ?assertEqual({2011, 2, 4}, event_date("weekly", 1, [friday], {2011, 1, 1}, {2011, 1, 28})),
    ?assertEqual({2011, 2, 5}, event_date("weekly", 1, [saturday], {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 2, 6}, event_date("weekly", 1, [sunday], {2011, 1, 1}, {2011, 1, 30})),
    %%  increment over year boundary
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday], {2010, 1, 1}, {2010, 12, 27})),
    ?assertEqual({2011, 1, 4}, event_date("weekly", 1, [tuesday], {2010, 1, 1}, {2010, 12, 28})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 1, [wensday], {2010, 1, 1}, {2010, 12, 29})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 1, [thursday], {2010, 1, 1}, {2010, 12, 30})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 1, [friday], {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 1, 1}, event_date("weekly", 1, [saturday], {2010, 1, 1}, {2010, 12, 25})),
    ?assertEqual({2011, 1, 2}, event_date("weekly", 1, [sunday], {2010, 1, 1}, {2010, 12, 26})),
    %%  leap year (into)
    ?assertEqual({2008, 2, 29}, event_date("weekly", 1, [friday], {2008, 1, 1}, {2008, 2, 28})),
    %%  leap year (over)
    ?assertEqual({2008, 3, 1}, event_date("weekly", 1, [saturday], {2008, 1, 1}, {2008, 2, 28})),
    ?assertEqual({2008, 3, 7}, event_date("weekly", 1, [friday], {2008, 1, 1}, {2008, 2, 29})),
    %% shift start date (no impact)
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday], {2008, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday], {2009, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday], {2010, 1, 2}, {2011, 1, 1})),
    %% multiple DOWs
    ?assertEqual({2011, 1, 2}, event_date("weekly", 1, [monday,tuesday,wensday,thursday,friday,saturday,sunday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 3}, event_date("weekly", 1, [monday,tuesday,wensday,thursday,friday,saturday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 4}, event_date("weekly", 1, [tuesday,wensday,thursday,friday,saturday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 1, [wensday,thursday,friday,saturday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 1, [thursday,friday,saturday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 1, [friday,saturday], {2011, 1, 1}, {2011, 1, 1})),
    %% even step (small)
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 12}, event_date("weekly", 2, [wensday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 13}, event_date("weekly", 2, [thursday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 14}, event_date("weekly", 2, [friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 15}, event_date("weekly", 2, [saturday], {2011, 1, 1}, {2011, 1, 1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    %%                and google calendar agrees (thunderbird and outlook be damned!)
    ?assertEqual({2011, 1, 2}, event_date("weekly", 2, [sunday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 16}, event_date("weekly", 2, [sunday], {2011, 1, 1}, {2011, 1, 2})),
    %% odd step (small)
    ?assertEqual({2011, 1, 17}, event_date("weekly", 3, [monday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 3, [tuesday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 19}, event_date("weekly", 3, [wensday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 20}, event_date("weekly", 3, [thursday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 21}, event_date("weekly", 3, [friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 22}, event_date("weekly", 3, [saturday], {2011, 1, 1}, {2011, 1, 1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011, 1, 2}, event_date("weekly", 3, [sunday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 23}, event_date("weekly", 3, [sunday], {2011, 1, 1}, {2011, 1, 2})),
    %% even step (large)
    ?assertEqual({2011, 6, 13}, event_date("weekly", 24, [monday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 14}, event_date("weekly", 24, [tuesday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 15}, event_date("weekly", 24, [wensday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 16}, event_date("weekly", 24, [thursday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 17}, event_date("weekly", 24, [friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 18}, event_date("weekly", 24, [saturday], {2011, 1, 1}, {2011, 1, 1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011, 1, 2}, event_date("weekly", 24, [sunday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 6, 19}, event_date("weekly", 24, [sunday], {2011, 1, 1}, {2011, 1, 2})),
    %% odd step (large)
    ?assertEqual({2011, 9, 5}, event_date("weekly", 36, [monday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 6}, event_date("weekly", 36, [tuesday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 7}, event_date("weekly", 36, [wensday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 8}, event_date("weekly", 36, [thursday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 9}, event_date("weekly", 36, [friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 10}, event_date("weekly", 36, [saturday], {2011, 1, 1}, {2011, 1, 1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011, 1, 2}, event_date("weekly", 36, [sunday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 9, 11}, event_date("weekly", 36, [sunday], {2011, 1, 1}, {2011, 1, 2})),
    %% shift start date (past)
    ?assertEqual({2011, 1, 3}, event_date("weekly", 2, [monday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 4}, event_date("weekly", 2, [tuesday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 2, [wensday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 2, [thursday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 2, [friday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 8}, event_date("weekly", 2, [saturday], {2010, 12, 26}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 9}, event_date("weekly", 2, [sunday], {2010, 12, 26}, {2011, 1, 1})),
    %% multiple DOWs with step (currently on start)
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 2})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 8})),
    %% multiple DOWs with step (start in past)
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 12}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 12}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 14}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 14}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 16})),
    %% multiple DOWs over month boundary 
    ?assertEqual({2011, 2, 7}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 28})),
    %% multiple DOWs over year boundary 
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2010, 1, 1}, {2010, 12, 31})),
    %% long span
    ?assertEqual({2011, 1, 10}, event_date("weekly", 4, [monday], {1983, 4, 11}, {2011, 1, 1})), 
    ?assertEqual({2011, 5, 2}, event_date("weekly", 4, [monday], {1983, 4, 11}, {2011, 4, 11})).    

every_other_day_weekly_recurrence_test() ->
    %% This is a proof of concept based on (implementation of a random find on the internet)
    %% http://www.outlookbanter.com/outlook-calandaring/80737-set-recurrance-every-other-weekday.html

    %% Blue
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 2})),     
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 8})),
    ?assertEqual({2011, 1, 10}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 12}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 12}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 14}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 14}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 16})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 17})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 18})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 19})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 20})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 21})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 22})),
    ?assertEqual({2011, 1, 24}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 23})),
    ?assertEqual({2011, 1, 26}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 24})),
    ?assertEqual({2011, 1, 26}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 25})),
    ?assertEqual({2011, 1, 28}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 26})),
    ?assertEqual({2011, 1, 28}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 27})),
    ?assertEqual({2011, 2, 7}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 28})),
    ?assertEqual({2011, 2, 7}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 2, 7}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 30})),
    ?assertEqual({2011, 2, 7}, event_date("weekly", 2, [monday, wensday, friday], {2011, 1, 1}, {2011, 1, 31})),

    %% White
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 2})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 3})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 4})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 5})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 6})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 7})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 8})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 9})),     
    ?assertEqual({2011, 1, 11}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 10})),     
    ?assertEqual({2011, 1, 13}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 11})),     
    ?assertEqual({2011, 1, 13}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 12})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 13})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 14})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 15})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 16})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 17})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 18})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 19})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 20})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 21})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 22})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 23})),     
    ?assertEqual({2011, 1, 25}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 24})),
    ?assertEqual({2011, 1, 27}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 25})),
    ?assertEqual({2011, 1, 27}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 26})),
    ?assertEqual({2011, 2, 8}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 27})),
    ?assertEqual({2011, 2, 8}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 28})),
    ?assertEqual({2011, 2, 8}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 2, 8}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 30})),
    ?assertEqual({2011, 2, 8}, event_date("weekly", 2, [tuesday, thursday], {2011, 1, 1}, {2011, 1, 31})),

    %% White
    ?assertEqual({2011, 1, 3}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 2})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 5}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 7}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 8})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 17}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 16})),
    ?assertEqual({2011, 1, 19}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 17})),
    ?assertEqual({2011, 1, 19}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 18})),
    ?assertEqual({2011, 1, 21}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 19})),
    ?assertEqual({2011, 1, 21}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 20})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 21})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 22})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 23})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 24})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 25})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 26})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 27})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 28})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 29})),
    ?assertEqual({2011, 1, 31}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 30})),
    ?assertEqual({2011, 2, 2}, event_date("weekly", 2, [monday, wensday, friday], {2010, 12, 25}, {2011, 1, 31})),

    %% Blue
    ?assertEqual({2011, 1, 4}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 2})),
    ?assertEqual({2011, 1, 4}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 6}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 8})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 16})),
    ?assertEqual({2011, 1, 18}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 17})),
    ?assertEqual({2011, 1, 20}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 18})),
    ?assertEqual({2011, 1, 20}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 19})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 20})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 21})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 22})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 23})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 24})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 25})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 26})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 27})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 28})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 29})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 30})),
    ?assertEqual({2011, 2, 1}, event_date("weekly", 2, [tuesday, thursday], {2010, 12, 25}, {2011, 1, 31})).

monthly_every_recurrence_test() ->
    %% basic increment (also crosses month boundary) 
    ?assertEqual({2011, 1, 3}, event_date("monthly", 1, {every, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, {every, monday}, {2011, 1, 1}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 17}, event_date("monthly", 1, {every, monday}, {2011, 1, 1}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 24}, event_date("monthly", 1, {every, monday}, {2011, 1, 1}, {2011, 1, 17})),
    ?assertEqual({2011, 1, 31}, event_date("monthly", 1, {every, monday}, {2011, 1, 1}, {2011, 1, 24})),
    ?assertEqual({2011, 1, 4}, event_date("monthly", 1, {every, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 11}, event_date("monthly", 1, {every, tuesday}, {2011, 1, 1}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 18}, event_date("monthly", 1, {every, tuesday}, {2011, 1, 1}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, {every, tuesday}, {2011, 1, 1}, {2011, 1, 18})),
    ?assertEqual({2011, 2, 1}, event_date("monthly", 1, {every, tuesday}, {2011, 1, 1}, {2011, 1, 25})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, {every, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 12}, event_date("monthly", 1, {every, wensday}, {2011, 1, 1}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 19}, event_date("monthly", 1, {every, wensday}, {2011, 1, 1}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 26}, event_date("monthly", 1, {every, wensday}, {2011, 1, 1}, {2011, 1, 19})),
    ?assertEqual({2011, 2, 2}, event_date("monthly", 1, {every, wensday}, {2011, 1, 1}, {2011, 1, 26})),
    ?assertEqual({2011, 1, 6}, event_date("monthly", 1, {every, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 13}, event_date("monthly", 1, {every, thursday}, {2011, 1, 1}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, {every, thursday}, {2011, 1, 1}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 27}, event_date("monthly", 1, {every, thursday}, {2011, 1, 1}, {2011, 1, 20})),
    ?assertEqual({2011, 2, 3}, event_date("monthly", 1, {every, thursday}, {2011, 1, 1}, {2011, 1, 27})),
    ?assertEqual({2011, 1, 7}, event_date("monthly", 1, {every, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 14}, event_date("monthly", 1, {every, friday}, {2011, 1, 1}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 21}, event_date("monthly", 1, {every, friday}, {2011, 1, 1}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 28}, event_date("monthly", 1, {every, friday}, {2011, 1, 1}, {2011, 1, 21})),
    ?assertEqual({2011, 2, 4}, event_date("monthly", 1, {every, friday}, {2011, 1, 1}, {2011, 1, 28})),
    ?assertEqual({2011, 1, 8}, event_date("monthly", 1, {every, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, {every, saturday}, {2011, 1, 1}, {2011, 1, 8})),
    ?assertEqual({2011, 1, 22}, event_date("monthly", 1, {every, saturday}, {2011, 1, 1}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 29}, event_date("monthly", 1, {every, saturday}, {2011, 1, 1}, {2011, 1, 22})),
    ?assertEqual({2011, 2, 5}, event_date("monthly", 1, {every, saturday}, {2011, 1, 1}, {2011, 1, 29})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 9}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 2})),
    ?assertEqual({2011, 1, 16}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 23}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 16})),
    ?assertEqual({2011, 1, 30}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 23})),
    ?assertEqual({2011, 2, 6}, event_date("monthly", 1, {every, sunday}, {2011, 1, 1}, {2011, 1, 30})),
    %% increment over year boundary 
    ?assertEqual({2011, 1, 3}, event_date("monthly", 1, {every, monday}, {2010, 1, 1}, {2010, 12, 27})),
    ?assertEqual({2011, 1, 4}, event_date("monthly", 1, {every, tuesday}, {2010, 1, 1}, {2010, 12, 28})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, {every, wensday}, {2010, 1, 1}, {2010, 12, 29})),
    ?assertEqual({2011, 1, 6}, event_date("monthly", 1, {every, thursday}, {2010, 1, 1}, {2010, 12, 30})),
    ?assertEqual({2011, 1, 7}, event_date("monthly", 1, {every, friday}, {2010, 1, 1}, {2010, 12, 31})),
    ?assertEqual({2011, 1, 1}, event_date("monthly", 1, {every, saturday}, {2010, 1, 1}, {2010, 12, 25})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, {every, sunday}, {2010, 1, 1}, {2010, 12, 26})),
    %%  leap year (into) 
    ?assertEqual({2008, 2, 29}, event_date("monthly", 1, {every, friday}, {2008, 1, 1}, {2008, 2, 28})),
    %% leap year (over)
    ?assertEqual({2008, 3, 1}, event_date("monthly", 1, {every, saturday}, {2008, 1, 1}, {2008, 2, 28})),
    %% long span
    ?assertEqual({1983, 4, 11}, event_date("monthly", 1, {every, monday}, {1983, 4, 11}, {1983, 4, 10})).

monthly_last_recurrence_test() ->
    %% basic increment 
    ?assertEqual({2011, 1, 31}, event_date("monthly", 1, {last, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, {last, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 26}, event_date("monthly", 1, {last, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 27}, event_date("monthly", 1, {last, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 28}, event_date("monthly", 1, {last, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 29}, event_date("monthly", 1, {last, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 30}, event_date("monthly", 1, {last, sunday} , {2011, 1, 1}, {2011, 1, 1})),    
    %% basic increment (mid year)
    ?assertEqual({2011, 6, 27}, event_date("monthly", 1, {last, monday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 28}, event_date("monthly", 1, {last, tuesday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 29}, event_date("monthly", 1, {last, wensday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 30}, event_date("monthly", 1, {last, thursday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 24}, event_date("monthly", 1, {last, friday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 25}, event_date("monthly", 1, {last, saturday}, {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 6, 26}, event_date("monthly", 1, {last, sunday} , {2011, 6, 1}, {2011, 6, 1})),
    %%  leap year
    ?assertEqual({2008, 2, 25}, event_date("monthly", 1, {last, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 26}, event_date("monthly", 1, {last, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 27}, event_date("monthly", 1, {last, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 28}, event_date("monthly", 1, {last, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 29}, event_date("monthly", 1, {last, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 23}, event_date("monthly", 1, {last, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 24}, event_date("monthly", 1, {last, sunday} , {2008, 1, 1}, {2008, 2, 1})),
    %%  long span
    ?assertEqual({1983, 4, 25}, event_date("monthly", 1, {last, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 26}, event_date("monthly", 1, {last, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 27}, event_date("monthly", 1, {last, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 28}, event_date("monthly", 1, {last, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 29}, event_date("monthly", 1, {last, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 30}, event_date("monthly", 1, {last, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 24}, event_date("monthly", 1, {last, sunday} , {1983, 1, 1}, {1983, 4, 1})).

monthly_every_ordinal_recurrence_test() ->
    %% basic first
    ?assertEqual({2011, 1, 3}, event_date("monthly", 1, {first, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 4}, event_date("monthly", 1, {first, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, {first, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 6}, event_date("monthly", 1, {first, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 7}, event_date("monthly", 1, {first, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 1}, event_date("monthly", 1, {first, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, {first, sunday} , {2011, 1, 1}, {2011, 1, 1})),
    %% basic second
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, {second, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 11}, event_date("monthly", 1, {second, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 12}, event_date("monthly", 1, {second, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 13}, event_date("monthly", 1, {second, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 14}, event_date("monthly", 1, {second, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 8}, event_date("monthly", 1, {second, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 9}, event_date("monthly", 1, {second, sunday} , {2011, 1, 1}, {2011, 1, 1})),   
    %% basic third
    ?assertEqual({2011, 1, 17}, event_date("monthly", 1, {third, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 18}, event_date("monthly", 1, {third, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 19}, event_date("monthly", 1, {third, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, {third, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 21}, event_date("monthly", 1, {third, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, {third, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 16}, event_date("monthly", 1, {third, sunday} , {2011, 1, 1}, {2011, 1, 1})),   
    %% basic fourth
    ?assertEqual({2011, 1, 24}, event_date("monthly", 1, {fourth, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, {fourth, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 26}, event_date("monthly", 1, {fourth, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 27}, event_date("monthly", 1, {fourth, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 28}, event_date("monthly", 1, {fourth, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 22}, event_date("monthly", 1, {fourth, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 23}, event_date("monthly", 1, {fourth, sunday} , {2011, 1, 1}, {2011, 1, 1})),
    %% basic fifth
    ?assertEqual({2011, 1, 31}, event_date("monthly", 1, {fifth, monday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 1}, event_date("monthly", 1, {fifth, tuesday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 2}, event_date("monthly", 1, {fifth, wensday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 3}, event_date("monthly", 1, {fifth, thursday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 4}, event_date("monthly", 1, {fifth, friday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 29}, event_date("monthly", 1, {fifth, saturday}, {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 30}, event_date("monthly", 1, {fifth, sunday} , {2011, 1, 1}, {2011, 1, 1})),
    %% leap year first
    ?assertEqual({2008, 2, 4}, event_date("monthly", 1, {first, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 5}, event_date("monthly", 1, {first, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 6}, event_date("monthly", 1, {first, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 7}, event_date("monthly", 1, {first, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 1}, event_date("monthly", 1, {first, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 2}, event_date("monthly", 1, {first, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 3}, event_date("monthly", 1, {first, sunday} , {2008, 1, 1}, {2008, 2, 1})),
    %% leap year second
    ?assertEqual({2008, 2, 11}, event_date("monthly", 1, {second, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 12}, event_date("monthly", 1, {second, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 13}, event_date("monthly", 1, {second, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 14}, event_date("monthly", 1, {second, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 8}, event_date("monthly", 1, {second, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 9}, event_date("monthly", 1, {second, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 10}, event_date("monthly", 1, {second, sunday} , {2008, 1, 1}, {2008, 2, 1})),   
    %% leap year third
    ?assertEqual({2008, 2, 18}, event_date("monthly", 1, {third, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 19}, event_date("monthly", 1, {third, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 20}, event_date("monthly", 1, {third, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 21}, event_date("monthly", 1, {third, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 15}, event_date("monthly", 1, {third, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 16}, event_date("monthly", 1, {third, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 17}, event_date("monthly", 1, {third, sunday} , {2008, 1, 1}, {2008, 2, 1})),   
    %% leap year fourth
    ?assertEqual({2008, 2, 25}, event_date("monthly", 1, {fourth, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 26}, event_date("monthly", 1, {fourth, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 27}, event_date("monthly", 1, {fourth, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 28}, event_date("monthly", 1, {fourth, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 22}, event_date("monthly", 1, {fourth, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 23}, event_date("monthly", 1, {fourth, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 24}, event_date("monthly", 1, {fourth, sunday} , {2008, 1, 1}, {2008, 2, 1})),
    %% leap year fifth
    ?assertEqual({2008, 3, 3}, event_date("monthly", 1, {fifth, monday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 3, 4}, event_date("monthly", 1, {fifth, tuesday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 3, 5}, event_date("monthly", 1, {fifth, wensday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 3, 6}, event_date("monthly", 1, {fifth, thursday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 2, 29}, event_date("monthly", 1, {fifth, friday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 3, 1}, event_date("monthly", 1, {fifth, saturday}, {2008, 1, 1}, {2008, 2, 1})),
    ?assertEqual({2008, 3, 2}, event_date("monthly", 1, {fifth, sunday} , {2008, 1, 1}, {2008, 2, 1})),
    %% long span first
    ?assertEqual({1983, 4, 4}, event_date("monthly", 1, {first, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 5}, event_date("monthly", 1, {first, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 6}, event_date("monthly", 1, {first, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 7}, event_date("monthly", 1, {first, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 1}, event_date("monthly", 1, {first, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 2}, event_date("monthly", 1, {first, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 3}, event_date("monthly", 1, {first, sunday} , {1983, 1, 1}, {1983, 4, 1})),
    %% long span second
    ?assertEqual({1983, 4, 11}, event_date("monthly", 1, {second, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 12}, event_date("monthly", 1, {second, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 13}, event_date("monthly", 1, {second, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 14}, event_date("monthly", 1, {second, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 8}, event_date("monthly", 1, {second, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 9}, event_date("monthly", 1, {second, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 10}, event_date("monthly", 1, {second, sunday} , {1983, 1, 1}, {1983, 4, 1})),   
    %% long span third
    ?assertEqual({1983, 4, 18}, event_date("monthly", 1, {third, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 19}, event_date("monthly", 1, {third, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 20}, event_date("monthly", 1, {third, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 21}, event_date("monthly", 1, {third, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 15}, event_date("monthly", 1, {third, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 16}, event_date("monthly", 1, {third, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 17}, event_date("monthly", 1, {third, sunday} , {1983, 1, 1}, {1983, 4, 1})),   
    %% long span fourth
    ?assertEqual({1983, 4, 25}, event_date("monthly", 1, {fourth, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 26}, event_date("monthly", 1, {fourth, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 27}, event_date("monthly", 1, {fourth, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 28}, event_date("monthly", 1, {fourth, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 22}, event_date("monthly", 1, {fourth, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 23}, event_date("monthly", 1, {fourth, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 24}, event_date("monthly", 1, {fourth, sunday} , {1983, 1, 1}, {1983, 4, 1})),
    %% long span fifth
    ?assertEqual({1983, 5, 2}, event_date("monthly", 1, {fifth, monday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 5, 3}, event_date("monthly", 1, {fifth, tuesday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 5, 4}, event_date("monthly", 1, {fifth, wensday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 5, 5}, event_date("monthly", 1, {fifth, thursday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 29}, event_date("monthly", 1, {fifth, friday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 4, 30}, event_date("monthly", 1, {fifth, saturday}, {1983, 1, 1}, {1983, 4, 1})),
    ?assertEqual({1983, 5, 1}, event_date("monthly", 1, {fifth, sunday} , {1983, 1, 1}, {1983, 4, 1})).

monthly_date_recurrence_test() ->
    %% basic increment
    lists:foreach(fun(D) ->                          
                          ?assertEqual({2011, 1, D + 1}, event_date("monthly", 1, [D + 1], {2011, 1, 1}, {2011, 1, D}))
                  end, lists:seq(1, 30)),
    lists:foreach(fun(D) ->                          
                          ?assertEqual({2011, 6, D + 1}, event_date("monthly", 1, [D + 1], {2011, 6, 1}, {2011, 6, D}))
                  end, lists:seq(1, 29)),
    %% increment over month boundary   
    ?assertEqual({2011, 2, 1}, event_date("monthly", 1, [1], {2011, 1, 1}, {2011, 1, 31})),
    ?assertEqual({2011, 7, 1}, event_date("monthly", 1, [1], {2011, 6, 1}, {2011, 6, 30})),
    %% increment over year boundary    
    ?assertEqual({2011, 1, 1}, event_date("monthly", 1, [1], {2010, 1, 1}, {2010, 12, 31})),
    %% leap year (into)
    ?assertEqual({2008, 2, 29}, event_date("monthly", 1, [29], {2008, 1, 1}, {2008, 2, 28})),
    %% leap year (over)
    ?assertEqual({2008, 3, 1}, event_date("monthly", 1, [1], {2008, 1, 1}, {2008, 2, 29})),
    %% shift start date (no impact)
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, [2], {2008, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, [2], {2009, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 1, [2], {2010, 1, 1}, {2011, 1, 1})),
    %% multiple dates
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 2})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 3})),
    ?assertEqual({2011, 1, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 4})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 5})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 6})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 7})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 8})),
    ?assertEqual({2011, 1, 10}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 9})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 10})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 11})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 12})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 13})),
    ?assertEqual({2011, 1, 15}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 14})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 15})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 16})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 17})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 18})),
    ?assertEqual({2011, 1, 20}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 19})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 20})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 21})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 22})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 23})),
    ?assertEqual({2011, 1, 25}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 24})),
    ?assertEqual({2011, 2, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 25})),
    ?assertEqual({2011, 2, 5}, event_date("monthly", 1, [5,10,15,20,25], {2011, 1, 1}, {2011, 1, 26})),
    %% even step (small)
    ?assertEqual({2011, 3, 2}, event_date("monthly", 2, [2], {2011, 1, 1}, {2011, 1, 2})),
    ?assertEqual({2011, 5, 2}, event_date("monthly", 2, [2], {2011, 1, 1}, {2011, 3, 2})),
    ?assertEqual({2011, 7, 2}, event_date("monthly", 2, [2], {2011, 1, 1}, {2011, 5, 2})),
    ?assertEqual({2011, 6, 2}, event_date("monthly", 2, [2], {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 8, 2}, event_date("monthly", 2, [2], {2011, 6, 1}, {2011, 6, 2})),
    %% odd step (small)
    ?assertEqual({2011, 4, 2}, event_date("monthly", 3, [2], {2011, 1, 1}, {2011, 1, 2})),
    ?assertEqual({2011, 7, 2}, event_date("monthly", 3, [2], {2011, 1, 1}, {2011, 4, 2})),
    ?assertEqual({2011, 10, 2}, event_date("monthly", 3, [2], {2011, 1, 1}, {2011, 7, 2})),
    ?assertEqual({2011, 6, 2}, event_date("monthly", 3, [2], {2011, 6, 1}, {2011, 6, 1})),
    ?assertEqual({2011, 9, 2}, event_date("monthly", 3, [2], {2011, 6, 1}, {2011, 6, 2})),
    %% shift start date
    ?assertEqual({2011, 2, 2}, event_date("monthly", 3, [2], {2007, 5, 1}, {2011, 1, 1})),
    ?assertEqual({2011, 3, 2}, event_date("monthly", 3, [2], {2008, 6, 2}, {2011, 1, 1})),
    ?assertEqual({2011, 1, 2}, event_date("monthly", 3, [2], {2009, 7, 3}, {2011, 1, 1})),
    ?assertEqual({2011, 2, 2}, event_date("monthly", 3, [2], {2010, 8, 4}, {2011, 1, 1})).

-endif.
