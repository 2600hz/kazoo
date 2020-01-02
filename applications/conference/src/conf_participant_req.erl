%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_participant_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Options) ->
    'true' = kapi_conference:add_participant_v(JObj),
    Call = kapps_call:from_json(JObj),
    _ = kapps_call_command:set(kz_json:from_list([{<<"Is-Conference">>, <<"true">>}]), 'undefined', Call),
    _ = kapps_call:put_callid(Call),
    case conf_participant_sup:start_participant(Call) of
        {'ok', Srv} ->
            Conference = kapps_conference:from_json(JObj),
            conf_participant:set_conference(Conference, Srv),
            lager:info("added participant at ~p", [Srv]);
        _Else ->
            lager:info("failed to add participant: ~p", [_Else])
    end.
