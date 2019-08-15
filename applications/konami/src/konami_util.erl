%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_util).

-export([listen_on_other_leg/2]).
-export([send_hangup_req/1]).
-export([send_break_req/1]).

-include("konami.hrl").

-spec listen_on_other_leg(kapps_call:call(), kz_term:ne_binaries()) -> 'ok'.
listen_on_other_leg(Call, Events) ->
    API = [{<<"Application-Name">>, <<"noop">>}
          ,{<<"B-Leg-Events">>, Events}
          ,{<<"Insert-At">>, <<"now">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    kapps_call_command:send_command(API, Call).

-spec send_hangup_req(kz_term:ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"hangup">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

-spec send_break_req(kz_term:ne_binary()) -> 'ok'.
send_break_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"break">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to break ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).
