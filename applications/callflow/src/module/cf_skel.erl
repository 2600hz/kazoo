%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Base module for Callflow action.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`param_1'</dt>
%%%   <dd>Use param_1 to make a call</dd>
%%%
%%%   <dt>`param_2'</dt>
%%%   <dd>Description.</dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_skel).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    %% Data is the "data" object from the JSON payload
    %% Call is the current kapps_call record

    %% Give control back to cf_exe process
    cf_exe:continue(Call).
