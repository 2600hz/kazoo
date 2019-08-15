%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc EDR output formatter.
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_formatter).

-include("edr.hrl").

%%%-----------------------------------------------------------------------------
%%% Callbacks
%%%-----------------------------------------------------------------------------
-callback(format_event(Options :: kz_json:object(), Event :: edr_event()) -> kz_term:ne_binary()).
