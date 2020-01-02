%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
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


-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    case knm_phone_number:state(PN) of
        ?NUMBER_STATE_IN_SERVICE -> update(PN);
        _ -> delete(PN)
    end.

-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    case knm_phone_number:feature(PN, ?KEY) of
        'undefined' -> PN;
        _F -> knm_providers:deactivate_feature(PN, ?KEY)
    end.

-spec update(knm_phone_number:record()) -> knm_phone_number:record().
update(PN) ->
    Private = knm_phone_number:feature(PN, ?KEY),
    Public0 = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    Public = case kz_term:is_boolean(Public0) of
                 'false' -> 'undefined';
                 'true' -> kz_term:is_true(Public0)
             end,
    NotChanged = kz_json:are_equal(Private, Public),
    case kz_term:is_empty(Public) of
        'true' -> knm_providers:deactivate_feature(PN, ?KEY);
        'false' when NotChanged -> PN;
        'false' -> knm_providers:activate_feature(PN, {?KEY, Public})
    end.
