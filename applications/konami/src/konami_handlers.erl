%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_handlers).

-export([handle_metaflow/2]).

-include("konami.hrl").

handle_metaflow(JObj, _Props) ->
    lager:debug("metaflow: ~p", [JObj]),
    'true' = wapi_dialplan:metaflow_v(JObj),
    lager:debug("and its valid"),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>)),
    lager:debug("call: ~s", [whapps_call:control_queue(Call)]).
