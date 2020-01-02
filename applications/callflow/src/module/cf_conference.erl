%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_conference).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Command =
        props:filter_undefined(
          [{<<"Call">>, kapps_call:to_json(Call)}
          ,{<<"Conference-ID">>, kz_json:get_ne_binary_value(<<"id">>, Data)}
          ,{<<"Moderator">>, kz_json:get_binary_boolean(<<"moderator">>, Data)}
          ,{<<"Play-Welcome">>, kz_json:is_true([<<"welcome_prompt">>, <<"play">>], Data, 'true')}
          ,{<<"Play-Welcome-Media">>, kz_json:get_ne_value([<<"welcome_prompt">>, <<"media_id">>], Data)}
          ,{<<"Conference-Doc">>, kz_json:get_json_value(<<"config">>, Data)}
          ,{<<"Play-Exit-Tone">>, kz_json:get_ne_value([<<"play_exit_tone">>], Data)}
          ,{<<"Play-Entry-Tone">>, kz_json:get_ne_value([<<"play_entry_tone">>], Data)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ]),
    kapi_conference:publish_discovery_req(Command),
    cf_exe:control_usurped(Call).
