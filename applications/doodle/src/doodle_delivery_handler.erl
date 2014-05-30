%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_delivery_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    lager:info("DOODLE ~p",[JObj]),
    'true' = wapi_sms:delivery_v(JObj),
    _ = wh_util:put_callid(JObj),
    
    'ok'.
