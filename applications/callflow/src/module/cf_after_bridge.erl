%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Sets `[park|transfer|hangup]_after_bridge' variable.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd> Possible values: `park', `transfer', `hangup'</dd>
%%%
%%%   <dt>`data'</dt>
%%%   <dd>Some extension number, the number to transfer to, if the `action'
%%%   is `transfer', otherwise a boolean indicating should do the action.</dd>
%%% </dl>
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_after_bridge).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    PostBridgeAction = kz_json:get_value(<<"action">>, Data),
    PostBridgeData = kz_json:get_value(<<"data">>, Data),
    lager:info("injecting ~s(~p)", [PostBridgeAction, PostBridgeData]),
    Action = build_action(PostBridgeAction, PostBridgeData),
    kapps_call_command:set(Action, kz_json:new(), Call),
    cf_exe:continue(Call).

-spec build_action(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
build_action(<<"park">>, ShouldPark) ->
    kz_json:from_list([{<<"Park-After-Pickup">>, kz_term:is_true(ShouldPark)}]);
build_action(<<"hangup">>, ShouldHangup) ->
    kz_json:from_list([{<<"Hangup-After-Pickup">>, kz_term:is_true(ShouldHangup)}]);
build_action(<<"transfer">>, ToExtension) when is_binary(ToExtension) ->
    kz_json:from_list([{<<"Transfer-After-Pickup">>, ToExtension}]);
build_action(_Cmd, _Data) ->
    lager:info("unknown command: ~s(~p)", [_Cmd, _Data]),
    kz_json:new().
