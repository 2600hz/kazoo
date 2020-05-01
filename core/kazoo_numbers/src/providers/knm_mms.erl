%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle im features
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_mms).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([available/1
        ,available/3
        ]).
-export([settings/1]).


-include("knm.hrl").

-define(IM_TYPE, 'mms').

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    knm_im:save(?IM_TYPE, PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    knm_im:delete(?IM_TYPE, PN).


-spec available(knm_phone_number:record()) -> boolean().
available(PN) ->
    knm_im:available(?IM_TYPE, PN).

-spec available(kz_term:ne_binary() | module(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().
available(Carrier, State, AccountId) ->
    knm_im:available(?IM_TYPE, Carrier, State, AccountId).

-spec settings(knm_phone_number:record()) -> kz_json:object().
settings(PN) ->
    knm_im:settings(?IM_TYPE, PN).
