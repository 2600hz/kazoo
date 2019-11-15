%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
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

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(N) ->
    save(N, knm_phone_number:state(knm_number:phone_number(N))).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(N, ?NUMBER_STATE_IN_SERVICE) -> update_failover(N);
save(N, _) -> delete(N).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the failover route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(N) ->
    case feature(N) =:= 'undefined' of
        'true' -> N;
        'false' -> knm_providers:deactivate_feature(N, ?KEY)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(N) ->
    knm_phone_number:feature(knm_number:phone_number(N), ?KEY).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_failover(knm_number:knm_number()) -> knm_number:knm_number().
update_failover(N) ->
    Private = feature(N),
    Public = kz_json:get_ne_value(?KEY, knm_phone_number:doc(knm_number:phone_number(N))),
    NotChanged = kz_json:are_equal(Private, Public),
    case kz_term:is_empty(Public) of
        true -> knm_providers:deactivate_feature(N, ?KEY);
        false when NotChanged -> N;
        false -> knm_providers:activate_feature(N, {?KEY, Public})
    end.
