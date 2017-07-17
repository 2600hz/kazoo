%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc Represent a date, and perform various manipulations on it.
%%%
%%% @contributors
%%%     Mark Magnusson
%%%     Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_date).

-include("kazoo_stdlib/include/kz_types.hrl").

%% Date object functions
-export([
         from_gregorian_seconds/2
        ,from_iso_week/1

        ,find_next_weekday/2
        ,normalize/1
        ,relative_difference/2
        ]).

%% Utility Functions
-export([
         ordinal_to_position/1
        ,wday_to_dow/1
        ,dow_to_wday/1
        ]).

%%--------------------------------------------------------------------
%% @doc Convert a gregorian seconds integer to kz_date taking into consideration timezone
%% @end
%%--------------------------------------------------------------------
-spec from_gregorian_seconds(non_neg_integer(), ne_binary()) -> kz_date().
from_gregorian_seconds(Seconds, TZ) when is_integer(Seconds) ->
    {Date, _} = localtime:utc_to_local(
                  calendar:gregorian_seconds_to_datetime(Seconds)
                                      ,kz_term:to_list(TZ)
                 ),
    Date.

%%--------------------------------------------------------------------
%% @doc
%% Caclulates the date of a given ISO 8601 week
%% @end
%%--------------------------------------------------------------------
-spec from_iso_week(kz_iso_week()) -> kz_date().
from_iso_week({Year, Week}) ->
    Jan1 = calendar:date_to_gregorian_days(Year, 1, 1),
    Offset = 4 - calendar:day_of_the_week(Year, 1, 4),
    Days =
        case Offset =:= 0 of
            'true' -> Jan1 + ( Week * 7 );
            'false' ->
                Jan1 + Offset + ( ( Week - 1 ) * 7 )
        end,
    calendar:gregorian_days_to_date(Days).

%%--------------------------------------------------------------------
%% @doc
%% Normalizes dates, for example corrects for months that are given
%% with more days then they have (ie: {2011, 1, 36} -> {2011, 2, 5}).
%% I have been refering to this as 'spanning a month/year border'
%% @end
%%--------------------------------------------------------------------
-spec normalize(kz_date()) -> kz_date().
normalize({Y, 13, D}) ->
    normalize({Y + 1, 1, D});
normalize({Y, 0, D}) ->
    normalize({Y - 1, 12, D});
normalize({Y, M, D}) when M > 12 ->
    normalize({Y + 1, M - 12, D});
normalize({Y, M, D}) when M < 1 ->
    normalize({Y - 1, M + 12, D});
normalize({Y, M, D}) when D < 1 ->
    {Y1, M1, _} = normalize({Y, M - 1, 1}),
    D0 = calendar:last_day_of_the_month(Y1, M1),
    normalize({Y1, M1, D + D0});
normalize({Y, M, D}=Date) ->
    case calendar:last_day_of_the_month(Y, M) of
        Days when D > Days ->
            normalize({Y, M + 1, D - Days});
        _ ->
            Date
    end.

%%--------------------------------------------------------------------
%% @doc Calculate when the second date is in time, relative to the first
%% @end
%%--------------------------------------------------------------------
-spec relative_difference(kz_datetime(), kz_datetime()) -> 'future' | 'equal' | 'past'.
relative_difference(Date1, Date2) ->
    case calendar:time_difference(Date1, Date2) of
        {D, _} when D > 0 -> 'future';
        {D, _} when D < 0 -> 'past';
        {0, {0, 0, 0}} -> 'equal';
        {0, _} -> 'future'
    end.

%%--------------------------------------------------------------------
%% @doc
%% Calculates the date of the next occurance of a weekday from the given
%% start date.
%%
%% NOTICE!
%% It is possible for this function to cross month/year boundaries.
%% @end
%%--------------------------------------------------------------------
-spec find_next_weekday(kz_date(), binary()) -> kz_date().
find_next_weekday({Y, M, D}, Weekday) ->
    RefDOW = wday_to_dow(Weekday),
    case calendar:day_of_the_week({Y, M, D}) of
        %% Today is the DOW we wanted, calculate for next week
        RefDOW ->
            normalize({Y, M, D + 7});
        %% If the DOW has not occured this week yet
        DOW when RefDOW > DOW ->
            normalize({Y, M, D + (RefDOW - DOW)});
        %% If the DOW occurance has already happend, calculate
        %%   for the next week using the current DOW as a reference
        DOW ->
            normalize({Y, M, D + ( 7 - DOW ) + RefDOW})
    end.
%%--------------------------------------------------------------------
%% @doc
%% Convert the ordinal words to cardinal numbers representing
%% the position
%% @end
%%--------------------------------------------------------------------
-spec ordinal_to_position(binary()) -> 0..4.
ordinal_to_position(<<"first">>) -> 0;
ordinal_to_position(<<"second">>) -> 1;
ordinal_to_position(<<"third">>) -> 2;
ordinal_to_position(<<"fourth">>) -> 3;
ordinal_to_position(<<"fifth">>) -> 4.

%%--------------------------------------------------------------------
%% @doc
%% Map the days of the week to cardinal numbers representing the
%% position, in accordance with ISO 8601
%% @end
%%--------------------------------------------------------------------
-spec wday_to_dow(binary()) -> kz_daynum().
wday_to_dow(<<"monday">>) -> 1;
wday_to_dow(<<"tuesday">>) -> 2;
wday_to_dow(<<"wednesday">>) -> 3;
wday_to_dow(<<"wensday">>) -> 3;
wday_to_dow(<<"thursday">>) -> 4;
wday_to_dow(<<"friday">>) -> 5;
wday_to_dow(<<"saturday">>) -> 6;
wday_to_dow(<<"sunday">>) -> 7.

%%--------------------------------------------------------------------
%% @doc
%% Map the position of a week day to its textual representation.
%% @end
%%--------------------------------------------------------------------
-spec dow_to_wday(kz_daynum()) -> binary().
dow_to_wday(1) -> <<"monday">>;
dow_to_wday(2) -> <<"tuesday">>;
dow_to_wday(3) -> <<"wednesday">>;
dow_to_wday(4) -> <<"thursday">>;
dow_to_wday(5) -> <<"friday">>;
dow_to_wday(6) -> <<"saturday">>;
dow_to_wday(7) -> <<"sunday">>.

