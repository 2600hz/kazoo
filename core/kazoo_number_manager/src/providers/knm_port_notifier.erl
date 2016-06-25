%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle publishing notification events for new port requests
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_port_notifier).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

-include_lib("kazoo_number_manager/src/knm.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl"). %% PORT_KEY

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the porting object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary(), ne_binary()) ->
                  knm_number:knm_number().
save(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    CurrentState = knm_phone_number:state(PhoneNumber),
    Doc = knm_phone_number:doc(PhoneNumber),
    State = kz_json:get_first_defined([?PVT_STATE, ?PVT_STATE_LEGACY], Doc),
    save(Number, CurrentState, State).

save(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Features = knm_phone_number:features(knm_number:phone_number(Number)),
    Port = kz_json:get_ne_value(?PORT_KEY, Features, kz_json:new()),
    _ = publish_ported(Number, Port),
    Number;
save(Number, _CurrentState, ?NUMBER_STATE_PORT_IN) ->
    maybe_port_feature(Number);
save(Number, _CurrentState, _State) ->
    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    knm_number:knm_number().
delete(Number) ->
    knm_services:deactivate_feature(Number, ?PORT_KEY).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:knm_number()) -> boolean().
has_emergency_services(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_port_feature(knm_number:knm_number()) ->
                                knm_number:knm_number().
maybe_port_feature(Number) ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case kz_json:get_ne_value([?PVT_FEATURES, ?PORT_KEY], Doc) of
        'undefined' ->
            knm_services:deactivate_feature(Number, ?PORT_KEY);
        Port ->
            Number1 = knm_services:activate_feature(Number, ?PORT_KEY),
            maybe_port_changed(Number1, Port)
    end.

-spec maybe_port_changed(knm_number:knm_number(), kz_json:object()) ->
                                knm_number:knm_number().
-spec maybe_port_changed(knm_number:knm_number(), kz_json:object(), boolean()) ->
                                knm_number:knm_number().
maybe_port_changed(Number, Port) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_port_changed(Number, Port, DryRun).

maybe_port_changed(Number, _Port, 'true') -> Number;
maybe_port_changed(Number, Port, 'false') ->
    CurrentPort = knm_phone_number:feature(knm_number:phone_number(Number), ?PORT_KEY),
    case kz_json:are_identical(CurrentPort, Port) of
        'true' -> Number;
        'false' ->
            lager:debug("port information has been changed: ~s"
                        ,[kz_json:encode(Port)]
                       ),
            _ = publish_port_update(Number, Port),
            Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_port_update(knm_number:knm_number(), kz_json:object()) -> 'ok'.
publish_port_update(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>,  knm_phone_number:assigned_to(PhoneNumber)}
              ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
              ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
              ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
              ,{<<"Port">>, Port}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapi_notifications:publish_port_request(Notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_ported(knm_number:knm_number(), kz_json:object()) -> 'ok'.
publish_ported(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>,  knm_phone_number:assigned_to(PhoneNumber)}
              ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
              ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
              ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
              ,{<<"Port">>, Port}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapi_notifications:publish_ported(Notify).
