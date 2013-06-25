%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnipresence_handlers).

-export([handle_event/2
        ]).

-include("omnipresence.hrl").

handle_event(_JObj, _Props) ->
    'ok'.
