%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Detects if a call is fax user(s).
%%% @author James Aimonetti
%%% @author Ben Wann
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_fax_detect).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(DEFAULT_FAX_DETECT_DURATION, 5).
-define(FAX_DETECT_DURATION, kapps_config:get_integer(?CF_CONFIG_CAT, <<"fax_detect_duration_s">>, ?DEFAULT_FAX_DETECT_DURATION)).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("detecting fax"),
    Duration = kz_json:get_integer_value(<<"duration">>, Data, ?FAX_DETECT_DURATION),
    case kapps_call_command:fax_detection(<<"inbound">>, Duration, Call) of
        'true' ->
            lager:debug("fax detected"),
            cf_exe:continue(<<"ON_FAX">>, Call);
        'false' ->
            lager:debug("fax not detected"),
            cf_exe:continue(<<"ON_VOICE">>, Call)
    end.
