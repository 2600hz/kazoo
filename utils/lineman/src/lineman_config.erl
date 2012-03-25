%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(lineman_config).

-export([get/1, get/2]).

get(Key) ->
    get(Key, undefined).

get(_Key, Default) ->
    Default.
