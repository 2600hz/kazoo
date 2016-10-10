%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%%
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx_util).

-export([creds/0]).

-include("knm.hrl").

-define(USER, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"user">>)).
-define(TOKEN, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"token">>)).


-spec creds() -> {ne_binary(), ne_binary()}.
creds() ->
    {?USER, ?TOKEN}.
