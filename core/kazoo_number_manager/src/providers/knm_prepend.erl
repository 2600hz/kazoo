%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle prepend feature
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_prepend).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_PREPEND).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_prepend(Number);
save(Number, _State) ->
    delete(Number).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else -> knm_providers:deactivate_feature(Number, ?KEY)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?KEY).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_prepend(knm_number:knm_number()) -> knm_number:knm_number().
update_prepend(Number) ->
    CurrentPrepend = feature(Number),
    PhoneNumber = knm_number:phone_number(Number),
    Prepend = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PhoneNumber)),
    NotChanged = kz_json:are_equal(CurrentPrepend, Prepend),
    case kz_term:is_empty(Prepend) of
        'true' ->
            knm_providers:deactivate_feature(Number, ?KEY);
        'false' when NotChanged  ->
            Number;
        'false' ->
            case kz_json:is_true(?PREPEND_ENABLED, Prepend) of
                'false' -> knm_providers:deactivate_feature(Number, ?KEY);
                'true' -> knm_providers:activate_feature(Number, {?KEY, Prepend})
            end
    end.
