%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc "data":{
%%%   "alert_info":{{value}},
%%% }
%%%
%%%
%%% @author Sergey Safarov <s.safarov@gmail.com>, Sponsored by Audian
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_set_alert_info).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Value = kz_json:get_ne_binary_value(<<"alert_info">>, Data),
    set_variable(Value, Call),
    Call1 = kapps_call:set_custom_channel_var(<<"Alert_info">>, Value, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).

-spec set_variable(kz_term:api_ne_binary(), kapps_call:call()) -> 'ok'.
set_variable('undefined', _Call) ->
    lager:warning("can not set alert_info without value!");
set_variable(Value, Call) ->
    lager:debug("set Alert-Info to ~s", [Value]),
    Var = kz_json:from_list([{<<"Alert-Info">>, Value}]),
    kapps_call_command:set('undefined', Var, Call).
