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
-module(knm_sms).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([enabled/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_SMS).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    State = knm_phone_number:state(PN),
    save(PN, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_IN_SERVICE) ->
    update_sms(PN);
save(PN, _State) ->
    delete(PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    case knm_phone_number:feature(PN, ?KEY) of
        'undefined' -> PN;
        _Else -> knm_providers:deactivate_feature(PN, ?KEY)
    end.

-spec enabled(knm_phone_number:record()) -> boolean().
enabled(PN) ->
    Feature = knm_phone_number:feature(PN, ?KEY),
    case kz_term:is_empty(Feature) of
        'true' -> 'false';
        'false' -> kz_json:is_true(?FEATURE_ENABLED, Feature)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_sms(knm_phone_number:record()) -> knm_phone_number:record().
update_sms(PN) ->
    CurrentFeature = knm_phone_number:feature(PN, ?KEY),
    Feature = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentFeature, Feature),
    case kz_term:is_empty(Feature) of
        'true' ->
            knm_providers:deactivate_feature(PN, ?KEY);
        'false' when NotChanged  ->
            PN;
        'false' ->
            case kz_json:is_true(?FEATURE_ENABLED, Feature) of
                'false' -> knm_providers:deactivate_feature(PN, ?KEY);
                'true' -> knm_providers:activate_feature(PN, {?KEY, Feature})
            end
    end.
