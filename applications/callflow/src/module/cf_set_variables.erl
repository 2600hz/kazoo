%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Set or export multiple Custom Channel variables.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`custom_application_vars'</dt>
%%%   <dd>A JSON object of the Key/Value of the variable/value to set or export.</dd>
%%%
%%%   <dt>`export'</dt>
%%%   <dd>`boolean()', should export instead of set or not.</dd>
%%% </dl>
%%%
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_set_variables).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CAVs = get_custom_application_vars(Data),
    Export = kz_json:is_true(<<"export">>, Data),
    Call1 = cf_exe:update_call(set_variables(Export, CAVs, Call)),
    cf_exe:continue(Call1).

-spec get_custom_application_vars(kz_json:object()) -> kz_term:proplist().
get_custom_application_vars(Data) ->
    JObj = kz_json:get_json_value(<<"custom_application_vars">>, Data, kz_json:new()),
    kz_json:to_proplist(JObj).

-spec set_variables(boolean(), kz_term:proplist(), kapps_call:call()) -> kapps_call:call().
set_variables('true', CAVs, Call) ->
    lager:debug("exporting custom app vars: ~p", [CAVs]),
    kapps_call:set_custom_application_vars(CAVs, Call, 'true');
set_variables('false', CAVs, Call) ->
    lager:debug("setting custom app vars: ~p", [CAVs]),
    kapps_call:set_custom_application_vars(CAVs, Call, 'false').
