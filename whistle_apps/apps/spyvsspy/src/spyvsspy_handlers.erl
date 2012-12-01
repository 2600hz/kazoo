%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(spyvsspy_handlers).

-export([handle_eavesdrop_req/2
        ]).

-include("spyvsspy.hrl").

handle_eavesdrop_req(_JObj, _Props) ->
    ok.
