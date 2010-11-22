%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for preforming the ping requests for Monitor Agent Ping
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_icmp).

-export([reachable/1, reachable/2]).
-export([ping/1, ping/2]).
-export([ping_test/1, ping_test/2]).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-define(RESULT_FIELDS, ["Target","TX","RX","Loss","Time","Min","Avg","Max","Mdev","Host"]).

reachable(Dest) ->
    reachable(Dest, 1).

reachable(Dest, Count) ->
    Cmd = io_lib:format("ping -c ~p ~p &>/dev/null; echo $?", [Count, Dest]),
    os:cmd(Cmd) == "0\n".

ping(Dest) ->
    ping(Dest, 1).

ping(Dest, Count) ->
    Cmd = io_lib:format("echo -n `echo -n $(ping -i 0.5 -W 1 -n -q -c ~p ~p) | cut -d' ' -f3,13,16,18,22,26 | sed -r 's/\\/| /,/g' | sed 's/[^0-9.,]//g'`", [Count, Dest]),
    Result = string:tokens(os:cmd(Cmd), ","),
    case length(Result) of
        0 -> [{"Error", "Agent could not execute ping"}];
        1 -> [{"Error", lists:nth(1, Result)}];
        _ -> create_proplist(?RESULT_FIELDS, Result, [])
    end.

ping_test(Dest) ->
    ping_test(Dest, 1).

ping_test(Dest, Count) ->
    Prop = ping(Dest, Count),
    Loss = get_value("Loss", Prop, "100"),
    case list_to_integer(Loss) of
       LossInt when LossInt < 2 -> lists:merge(Prop, [{"Success", true}]);
       _ -> lists:merge(Prop, [{"Success", false}])
    end.  

create_proplist(_, [], Prop) ->
    Prop;
create_proplist([], _, Prop) ->
    Prop;
create_proplist([KeyH|KeyT], [ValH|ValT], Prop) ->
    create_proplist(KeyT, ValT, [{KeyH, ValH}|Prop]).
