-module(timezone_convert).
-author("nisbus").
-include("../include/tz_database.hrl").
-export([timezone_to_timezone/3]).
 
timezone_to_timezone(DateTime, "UTC", ToTz) ->
    D = convert_db_to_timezonedict(),
    TzName = lookup_timezone(ToTz, D),
    localtime:utc_to_local(DateTime, TzName);
timezone_to_timezone(DateTime, FromTz, "UTC") ->
    D = convert_db_to_timezonedict(),
    TzName = lookup_timezone(FromTz, D),
    localtime:local_to_utc(DateTime, TzName);
timezone_to_timezone(DateTime, FromTz, ToTz) ->
    D = convert_db_to_timezonedict(),
    FromName = lookup_timezone(FromTz, D),
    ToName = lookup_timezone(ToTz, D),
    localtime:local_to_local(DateTime, FromName, ToName).

lookup_timezone(Tz,D) ->
    case dict:find(Tz, D) of
	{ok, Value} ->
	    Value;
	error ->
	    erlang:error("Timezone not found (~p) in ~p~n",[Tz,D])
    end.
    
convert_db_to_timezonedict()->
    lists:foldl(fun({TzName, {TZone,_},_,{_,_},{_,_},_,{_,_},_,{_,_}}, Acc) ->
			dict:store(TZone, TzName, Acc);
		   (_, Acc) ->
			Acc
		end, dict:new(), ?tz_database).
