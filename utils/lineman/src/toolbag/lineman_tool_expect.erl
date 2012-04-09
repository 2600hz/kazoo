%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_tool_expect).

-include_lib("lineman/src/lineman.hrl").

-export([set_parameter/2]).
-export([prepare/2]).
-export([execute/2]).

-spec set_parameter/2 :: (string(), #xmlElement{}) -> 'ok'.
set_parameter(_Name, _Parameter) -> ok.

-spec prepare/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(Xml, Workorder) ->
    Binding = lineman_util:xml_attribute("binding", Xml),
    lager:info("~p binding to '~s'", [self(), Binding]),
    lineman_bindings:bind(Binding, self(), 0),
    Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(Xml, Workorder) ->
    Args = lineman_util:xml_attribute("args", Xml),
    Timeout = lineman_util:xml_integer_attribute("timeout", Xml, 1000),
    receive
        Anything -> 
            io:format("~p~n", [Anything])
    after
        Timeout -> 
            throw(<<"expected reply not received before timeout">>)
    end,
    Workorder.
            

