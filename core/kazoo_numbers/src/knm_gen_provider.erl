%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc When implementing provider modules, these callbacks are a must!
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_gen_provider).

-include("knm.hrl").

-callback save(knm_phone_number:record()) ->
    knm_phone_number:record().

-callback delete(knm_phone_number:record()) ->
    knm_phone_number:record().
