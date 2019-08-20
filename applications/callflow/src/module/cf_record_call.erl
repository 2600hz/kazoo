%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handles starting/stopping a call recording.
%%% @author James Aimonetti
%%% @author Sponsored by Velvetech LLC, Implemented by SIPLABS LLC
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_record_call).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data0, Call) ->
    Label = kz_json:get_ne_binary_value(<<"label">>, Data0, kapps_call:kvs_fetch('cf_flow_name', Call)),
    Origin = <<"callflow : ", Label/binary>>,
    Data = kz_json:set_value(<<"origin">>, Origin, Data0),
    cf_exe:continue(
      handle(Data, Call, get_action(Data))
     ).

-spec handle(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> kapps_call:call().
handle(Data, Call, <<"start">>) ->
    ShouldFollowTransfer = kz_json:is_true(<<"should_follow_transfer">>, Data, 'true'),
    Call1 = kapps_call:kvs_store('recording_follow_transfer', ShouldFollowTransfer, Call),
    lager:info("starting call recording via action (follow transfer: ~s)", [ShouldFollowTransfer]),
    cf_exe:update_call(kapps_call:start_recording(Data, Call1));

handle(_Data, Call, <<"stop">>) ->
    cf_exe:update_call(kapps_call:stop_recording(Call)).

-spec get_action(kz_json:object()) -> kz_term:ne_binary().
get_action(Data) ->
    case kz_json:get_ne_binary_value(<<"action">>, Data) of
        <<"stop">> -> <<"stop">>;
        _ -> <<"start">>
    end.
