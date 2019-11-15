%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle failover provisioning
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_force_outbound).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_FORCE_OUTBOUND).


-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(N) ->
    case knm_phone_number:state(knm_number:phone_number(N)) of
        ?NUMBER_STATE_IN_SERVICE -> update(N);
        _ -> delete(N)
    end.

-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(N) ->
    case feature(N) of
        'undefined' -> N;
        _F -> knm_providers:deactivate_feature(N, ?KEY)
    end.

%% Internals

-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(N) ->
    knm_phone_number:feature(knm_number:phone_number(N), ?KEY).

-spec update(knm_number:knm_number()) -> knm_number:knm_number().
update(N) ->
    Private = feature(N),
    Public0 = kz_json:get_ne_value(?KEY, knm_phone_number:doc(knm_number:phone_number(N))),
    Public = case kz_term:is_boolean(Public0) of
                 'false' -> 'undefined';
                 'true' -> kz_term:is_true(Public0)
             end,
    NotChanged = kz_json:are_equal(Private, Public),
    case kz_term:is_empty(Public) of
        'true' -> knm_providers:deactivate_feature(N, ?KEY);
        'false' when NotChanged -> N;
        'false' -> knm_providers:activate_feature(N, {?KEY, Public})
    end.
