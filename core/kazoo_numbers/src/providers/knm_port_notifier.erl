%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
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
-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    State = kzd_phone_numbers:pvt_state(knm_phone_number:doc(PN)),
    save(PN, knm_phone_number:state(PN), State).

-spec save(knm_phone_number:record(), kz_term:ne_binary(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Port = case knm_phone_number:feature(PN, ?KEY) of
               'undefined' -> kz_json:new();
               Data -> Data
           end,
    publish_ported(PN, Port),
    PN;
save(PN, _CurrentState, ?NUMBER_STATE_PORT_IN) ->
    maybe_port_feature(PN);
save(PN, _CurrentState, _State) ->
    PN.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    knm_providers:deactivate_feature(PN, ?KEY).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_port_feature(knm_phone_number:record()) -> knm_phone_number:record().
maybe_port_feature(PN) ->
    Doc = knm_phone_number:doc(PN),
    case kz_json:get_ne_value(?KEY, Doc) of
        'undefined' -> knm_providers:deactivate_feature(PN, ?KEY);
        Port ->
            PN1 = knm_providers:activate_feature(PN, {?KEY, Port}),
            maybe_port_changed(PN1, Port)
    end.

-spec maybe_port_changed(knm_phone_number:record(), kz_json:object()) ->
          knm_phone_number:record().
maybe_port_changed(PN, Port) ->
    DryRun = knm_phone_number:dry_run(PN),
    maybe_port_changed(PN, Port, DryRun).

-spec maybe_port_changed(knm_phone_number:record(), kz_json:object(), boolean()) ->
          knm_phone_number:record().
maybe_port_changed(PN, _Port, 'true') -> PN;
maybe_port_changed(PN, Port, 'false') ->
    CurrentPort = knm_phone_number:feature(PN, ?KEY),
    case kz_json:are_equal(CurrentPort, Port) of
        'true' -> PN;
        'false' ->
            lager:debug("port information has been changed: ~s", [kz_json:encode(Port)]),
            publish_port_update(PN, Port),
            PN
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_port_update(knm_phone_number:record(), kz_json:object()) -> 'ok'.
publish_port_update(PhoneNumber, Port) ->
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
-spec publish_ported(knm_phone_number:record(), kz_json:object()) -> 'ok'.
publish_ported(PhoneNumber, Port) ->
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
             ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Port">>, Port}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_ported/1).
