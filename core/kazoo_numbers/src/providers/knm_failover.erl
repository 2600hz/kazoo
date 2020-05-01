%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handle failover provisioning
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_failover).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_FAILOVER).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the failover route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    save(PN, knm_phone_number:state(PN)).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_IN_SERVICE) -> update_failover(PN);
save(PN, _) -> delete(PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the failover route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    case knm_phone_number:feature(PN, ?KEY) =:= 'undefined' of
        'true' -> PN;
        'false' -> knm_providers:deactivate_feature(PN, ?KEY)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_failover(knm_phone_number:record()) -> knm_phone_number:record().
update_failover(PN) ->
    Private = knm_phone_number:feature(PN, ?KEY),
    Public = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(Private, Public),
    case kz_term:is_empty(Public) of
        'true' -> knm_providers:deactivate_feature(PN, ?KEY);
        'false' when NotChanged -> PN;
        'false' -> knm_providers:activate_feature(PN, {?KEY, Public})
    end.
