%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for preforming the ping requests for Monitor Agent Ping
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_icmp).

-export([ping/1, ping/2]).
-export([ping_test/1, ping_test/2]).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-define(RESULT_FIELDS, ["Target","TX","RX","Loss","Time","Min","Avg","Max","Mdev","Host"]).
-define(PING_CMD, "echo -n `echo -n $(ping -i 0.5 -W 1 -n -q -c ~p ~p) | cut -d' ' -f3,13,16,18,22,26 | sed -r 's/\\/| /,/g' | sed 's/[^0-9.,]//g'`").
-define(FAIL_LOSS, 2).

ping(Dest) ->
    ping(Dest, 1).

ping(Dest, Count) ->
    Cmd = io_lib:format(?PING_CMD, [Count, Dest]),
    Result = string:tokens(os:cmd(Cmd), ", "),
    format_log(info, "MONITOR_ICMP(~p): Ping to ~p returned ~p", [self(), Dest, Result]),
    case Result of
        [] -> 
            [{"Error", "Agent could not execute ping"}];
        [E] -> 
            [{"Error", E}];
        _ -> 
            create_proplist(?RESULT_FIELDS, Result, [])
    end.

ping_test(Dest) ->
    ping_test(Dest, 1).

ping_test(Dest, Count) ->
    Prop = ping(Dest, Count),
    Loss = get_value("Loss", Prop, "100"),
    case list_to_integer(Loss) of
       LossInt when LossInt < ?FAIL_LOSS -> 
            lists:merge(Prop, [{"Success", true}]);
       _ -> 
            lists:merge(Prop, [{"Success", false}])
    end.  

create_proplist(_, [], Prop) ->
    Prop;
create_proplist([], _, Prop) ->
    Prop;
create_proplist([KeyH|KeyT], [ValH|ValT], Prop) ->
    create_proplist(KeyT, ValT, [{KeyH, ValH}|Prop]).
