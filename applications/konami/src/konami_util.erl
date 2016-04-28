%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_util).

-export([listen_on_other_leg/2]).
-export([send_hangup_req/1]).
-export([send_break_req/1]).

-include("konami.hrl").

-spec listen_on_other_leg(whapps_call:call(), ne_binaries()) -> 'ok'.
listen_on_other_leg(Call, Events) ->
    API = [{<<"Application-Name">>, <<"noop">>}
           ,{<<"B-Leg-Events">>, Events}
           ,{<<"Insert-At">>, <<"now">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    whapps_call_command:send_command(API, Call).

-spec send_hangup_req(ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"hangup">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1).

-spec send_break_req(ne_binary()) -> 'ok'.
send_break_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"break">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to break ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1).