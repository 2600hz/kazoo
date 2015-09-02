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

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(PORT_KEY, <<"port">>).
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the porting object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number_return().
-spec save(knm_number:knm_number(), ne_binary(), ne_binary()) ->
                  knm_number_return().
save(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    CurrentState = knm_phone_number:state(PhoneNumber),
    Doc = knm_phone_number:doc(PhoneNumber),
    State = wh_json:get_ne_value(?PVT_STATE, Doc),
    save(Number, CurrentState, State).

save(Number, ?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_IN_SERVICE) ->
    Features = knm_phone_number:features(knm_number:phone_number(Number)),
    Port = wh_json:get_ne_value(?PORT_KEY, Features, wh_json:new()),
    _ = publish_ported(Number, Port),
    {'ok', Number};
save(Number, _CurrentState, ?NUMBER_STATE_PORT_IN) ->
    maybe_port_feature(Number);
save(Number, _CurrentState, _State) ->
    {'ok', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    {'ok', knm_number:knm_number()}.
delete(Number) ->
    knm_services:deactivate_feature(Number, ?PORT_KEY).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_port_feature(knm_number:knm_number()) ->
                                {'ok', knm_number:knm_number()}.
maybe_port_feature(Number) ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case wh_json:get_ne_value([?PVT_FEATURES, ?PORT_KEY], Doc) of
        'undefined' ->
            knm_services:deactivate_feature(Number, ?PORT_KEY);
        Port ->
            {'ok', Number1} = knm_services:activate_feature(Number, ?PORT_KEY),
            maybe_port_changed(Number1, Port)
    end.

-spec maybe_port_changed(knm_number:knm_number(), wh_json:object()) ->
                                {'ok', knm_number:knm_number()}.
-spec maybe_port_changed(knm_number:knm_number(), wh_json:object(), boolean()) ->
                                {'ok', knm_number:knm_number()}.
maybe_port_changed(Number, Port) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_port_changed(Number, Port, DryRun).

maybe_port_changed(Number, _Port, 'true') -> {'ok', Number};
maybe_port_changed(Number, Port, 'false') ->
    CurrentPort = knm_phone_number:feature(knm_number:phone_number(Number), ?PORT_KEY),
    case wh_json:are_identical(CurrentPort, Port) of
        'true' -> {'ok', Number};
        'false' ->
            lager:debug("port information has been changed: ~s"
                        ,[wh_json:encode(Port)]
                       ),
            _ = publish_port_update(Number, Port),
            {'ok', Number}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_port_update(knm_number:knm_number(), wh_json:object()) -> 'ok'.
publish_port_update(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>,  knm_phone_number:assigned_to(PhoneNumber)}
              ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?LOCAL_CARRIER}
              ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
              ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
              ,{<<"Port">>, Port}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_port_request(Notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_ported(knm_number:knm_number(), wh_json:object()) -> 'ok'.
publish_ported(Number, Port) ->
    PhoneNumber = knm_number:phone_number(Number),
    Notify = [{<<"Account-ID">>,  knm_phone_number:assigned_to(PhoneNumber)}
              ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?LOCAL_CARRIER}
              ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
              ,{<<"Authorized-By">>, knm_phone_number:auth_by(PhoneNumber)}
              ,{<<"Port">>, Port}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_ported(Notify).
