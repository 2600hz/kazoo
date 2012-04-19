%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_tool_couchdb).

-include_lib("lineman/src/lineman.hrl").

-export([set_parameter/2]).
-export([prepare/2]).
-export([execute/2]).

-spec set_parameter/2 :: (string(), xml()) -> 'ok'.
set_parameter(_Name, _Xml) -> ok.

-spec prepare/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(_Xml, Workorder) -> Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(_Xml, Workorder) -> Workorder.

