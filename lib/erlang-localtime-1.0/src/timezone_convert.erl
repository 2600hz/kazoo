-module(timezone_convert).
-author("nisbus").
-import(localtime).
-include("../include/tz_database.hrl").
-export([timezone_to_timezone/3]).

timezone_to_timezone(DateTime, FromTz, ToTz) ->
    D = convert_db_to_timezonedict(),
    case FromTz of
	"UTC" ->%%From is UTC
	    TzName = lookup_timezone(ToTz,D),
	    %io:format("Converting UTC ~p to ~p ~n",[DateTime,TzName]),
	    localtime:utc_to_local(DateTime,TzName);
	_ ->
	    case ToTz of
		"UTC" ->%% To is UTC
		    TzName = lookup_timezone(FromTz,D),
		    %io:format("Converting ~p, ~p to UTC~n",[DateTime, TzName]),
		    localtime:local_to_utc(DateTime,TzName);
		_ ->%%Neither is UTC
		    FromName = lookup_timezone(FromTz, D),
		    ToName = lookup_timezone(ToTz,D),
		    localtime:local_to_local(DateTime, FromName,ToName)
	    end
    end.

lookup_timezone(Tz,D) ->
    case dict:find(Tz,D) of
	{ok,Value} ->
	    Value;
	error ->
	    erlang:error("Timezone not found (~p) in ~p~n",[Tz,D])
    end.
    
convert_db_to_timezonedict()->
    lists:foldl(fun(X, AccIn) ->
			  {TzName,{TZone,_},_,{_,_},{_,_},_,{_,_},_,{_,_}} = X,
			  dict:store(TZone,TzName,AccIn)
	      end,dict:new(),?tz_database).
