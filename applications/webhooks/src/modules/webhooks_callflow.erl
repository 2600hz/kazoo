%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_callflow).

-include("../webhooks.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_notifications:webhook_v(JObj),
    Hook = webhooks_util:from_json(wh_json:get_value(<<"Hook">>, JObj)),
    Data = wh_json:get_value(<<"Data">>, JObj),
    webhooks_util:fire_hooks(Data, [Hook]).