%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Kazoo API Helpers.
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To only check the validity, use the API call's corresponding *_v/1 function.
%%% This will parse the proplist and return a boolean() if the proplist is valid
%%% for creating a JSON message.
%%%
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi).

%% API
-export([delivery_message/2
        ,encode_pid/1, encode_pid/2
        ]).

-include_lib("kz_amqp_util.hrl").

-spec event_category(kz_json:object()) -> atom().
event_category(JObj) ->
    kz_term:to_atom(kz_api:event_category(JObj), 'true').

-spec event_name(kz_json:object()) -> atom().
event_name(JObj) ->
    kz_term:to_atom(kz_api:event_name(JObj), 'true').

-spec delivery_message(JObj, kz_term:proplist()) ->
                              {{kz_term:ne_binary(), kz_term:ne_binary(), {#'P_basic'{}, #'basic.deliver'{}}}
                              ,{atom(), atom()}
                              ,JObj
                              }
                                  when JObj :: kz_json:object().
delivery_message(JObj, Props) ->
    Basic = props:get_value('basic', Props),
    Deliver = #'basic.deliver'{exchange=Exchange, routing_key=RK} = props:get_value('deliver', Props),
    {{Exchange, RK, {Basic, Deliver}}
    ,{event_category(JObj), event_name(JObj)}
    ,JObj
    }.

-spec encode_pid(kz_term:ne_binary()) -> kz_term:ne_binary().
encode_pid(Queue) ->
    encode_pid(Queue, self()).

-spec encode_pid(kz_term:ne_binary(), pid()) -> kz_term:ne_binary().
encode_pid(Queue, Pid) ->
    list_to_binary(["pid://", kz_term:to_binary(Pid), "/", Queue]).
