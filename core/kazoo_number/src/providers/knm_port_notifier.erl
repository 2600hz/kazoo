%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handle publishing notification events for new port requests
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_port_notifier).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_PORT).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% produce notifications if the porting object changes
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(N) ->
    PN = knm_number:phone_number(N),
    State = kzd_phone_numbers:pvt_state(knm_phone_number:doc(PN)),
    save(N, knm_phone_number:state(PN), State).

-spec save(knm_number:knm_number(), kz_term:ne_binary(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Port = case feature(Number) of
               undefined -> kz_json:new();
               Data -> Data
           end,
    _ = publish_ported(Number, Port),
    Number;
save(Number, _CurrentState, ?NUMBER_STATE_PORT_IN) ->
    maybe_port_feature(Number);
save(Number, _CurrentState, _State) ->
    Number.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    knm_providers:deactivate_feature(Number, ?KEY).

-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?KEY).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_port_feature(knm_number:knm_number()) -> knm_number:knm_number().
maybe_port_feature(Number) ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case kz_json:get_ne_value(?KEY, Doc) of
        'undefined' -> knm_providers:deactivate_feature(Number, ?KEY);
        Port ->
            Number1 = knm_providers:activate_feature(Number, {?KEY, Port}),
            maybe_port_changed(Number1, Port)
    end.

-spec maybe_port_changed(knm_number:knm_number(), kz_json:object()) ->
                                knm_number:knm_number().
maybe_port_changed(Number, Port) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_port_changed(Number, Port, DryRun).

-spec maybe_port_changed(knm_number:knm_number(), kz_json:object(), boolean()) ->
                                knm_number:knm_number().
maybe_port_changed(Number, _Port, 'true') -> Number;
maybe_port_changed(Number, Port, 'false') ->
    CurrentPort = feature(Number),
    case kz_json:are_equal(CurrentPort, Port) of
        'true' -> Number;
        'false' ->
            lager:debug("port information has been changed: ~s", [kz_json:encode(Port)]),
            _ = publish_port_update(Number, Port),
            Number
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_port_update(knm_number:knm_number(), kz_json:object()) -> 'ok'.
publish_port_update(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
             ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Port">>, Port}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_port_request/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_ported(knm_number:knm_number(), kz_json:object()) -> 'ok'.
publish_ported(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
             ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Port">>, Port}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_ported/1).
