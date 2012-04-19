%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_tool_sleep).

-include_lib("lineman/src/lineman.hrl").

-export([set_parameter/2]).
-export([prepare/2]).
-export([execute/2]).

-spec set_parameter/2 :: (string(), #xmlElement{}) -> 'ok'.
set_parameter(_Name, _Parameter) -> ok.

-spec prepare/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(_Xml, Workorder) -> Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(Xml, Workorder) ->
    Time = lineman_util:xml_integer_attribute("time", Xml, 1000),
    lager:debug("sleep for ~p", [Time]),
    timer:sleep(Time),
    Workorder.
