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
-export([prepare/1]).
-export([execute/1]).

-spec set_parameter/2 :: (string(), #xmlElement{}) -> 'ok'.
set_parameter(_Name, _Parameter) -> ok.

-spec prepare/1 :: (#xmlElement{}) -> 'ok'.
prepare(_Step) -> ok.

-spec execute/1 :: (#xmlElement{}) -> 'ok'.
execute(_Step) -> ok.
