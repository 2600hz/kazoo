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

