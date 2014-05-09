%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_handlers).

-export([handle_config_change/2]).

-include("ts.hrl").

handle_config_change(JObj, _Props) ->
    lager:info("Trunkstore doc change detected: ~p", [JObj]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    _ = wh_cache:filter(fun({lookup_user_flags, _, _, AcctId} = Key, _) when AcctId =:= AccountId ->
                               wh_cache:erase(Key),
                               'true';
                           (_, _) -> 'false'
                        end
                       ),
    'ok'.

