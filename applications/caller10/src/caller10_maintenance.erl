%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(caller10_maintenance).

-export([is_running/0]).

is_running() ->
    case lists:keyfind('caller10', 1, application:which_applications()) of
        'false' -> io:format("Caller10 is not currently running on this node~n", []);
        {_App, _Desc, _Vsn} ->
            io:format("Caller10 (~s) is running~n", [_Vsn])
    end.

