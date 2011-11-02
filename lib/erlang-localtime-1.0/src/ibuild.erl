-module(ibuild).

-export([build_index/0]).

-include("tz_database.hrl").

build_tzlist(TzName, Name, Dict) ->
   case dict:find(Name, Dict) of
      error ->
         dict:store(Name, [TzName], Dict);
      {ok, TzNames} ->
         dict:store(Name, TzNames ++ [TzName], Dict)
   end.

build_index() ->
   F = fun({TzName,{Name,_},{DName,_},_,_,_,_,_,_}, Acc) ->
         NewDict = build_tzlist(TzName, Name, Acc),
         build_tzlist(TzName, DName, NewDict);
      ({TzName,{Name,_},undef,_,_,_,_,_,_}, Acc) ->
         build_tzlist(TzName, Name, Acc)
   end,
   I = lists:foldl(F, dict:new(), ?tz_database),
   {ok, File} = file:open("tz_index.hrl", [write]),
   io:fwrite(File, "-define(tz_index, ~p).", [I]).
