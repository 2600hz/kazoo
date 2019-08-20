%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Send specified DTMF sequence.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`digits'</dt>
%%%   <dd>What sequence to send.</dd>
%%%
%%%   <dt>`duration_ms'</dt>
%%%   <dd><strong>Optional: </strong>duration, in milliseconds, to send DTMF.</dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_send_dtmf).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DTMFs = kz_json:get_value(<<"digits">>, Data),
    Duration = kz_json:get_binary_value(<<"duration_ms">>, Data, <<"2000">>),

    kapps_call_command:send_dtmf(DTMFs, Duration, Call),
    lager:debug("sent '~s' @ '~s' duration", [DTMFs, Duration]),

    cf_exe:continue(Call).
