%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_maintenance).

-export([is_running/0]).

is_running() ->
    case lists:keyfind('konami', 1, application:which_applications()) of
        'false' -> io:format("Konami is not currently running on this node~n", []);
        {_App, _Desc, _Vsn} ->
            io:format("Konami (~s) is running~n", [_Vsn])
    end.

