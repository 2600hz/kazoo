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
    lager:debug("~p binding to '~s'", [self(), Binding]),
    lineman_bindings:bind(Binding, self(), 0),
    Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(Xml, Workorder) ->
    Timeout = lineman_util:xml_integer_attribute("timeout", Xml, 1000),
    receive
        {binding, _, Received} -> 
            Type = lineman_util:xml_attribute("type", Xml, <<"line">>),
            Expected = lineman_util:xml_content(Xml),
            compare(Type, Received, Expected, Xml, Workorder)
    after
        Timeout -> 
            throw(<<"expected reply not received before timeout">>)
    end,
    Workorder.

-spec compare/5 :: (ne_binary(), term(), term(), xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
compare(<<"line">>, Received, Expected, _, Workorder) ->
    R = wh_util:to_list(Received),
    [begin 
         Error = io_lib:format("could not find expected string: ~p", [E]),
         throw(wh_util:to_binary(Error))
     end 
     || E <- string:tokens(wh_util:to_list(Expected), "\n")
        ,string:str(R, E) =:= 0
    ],
    Workorder;
compare(_, _, _, _, _) ->
    throw(<<"invalid expect type attribute">>).
