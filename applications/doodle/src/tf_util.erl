%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_util).

-include("doodle.hrl").

-export([build_im_endpoint/3
        ,delivery_status/1
        ]).

-type build_error() :: 'endpoint_disabled' | 'do_not_disturb'.
-type delivery_status() :: 'delivered' | 'failed'.

-export_type([build_error/0
             ,delivery_status/0
             ]).

-define(SIP_MSG_DEVICES, [<<"sip_device">>, <<"softphone">>]).

-spec build_im_endpoint(kz_json:object(), kz_json:object(), kapps_im:im()) -> {'ok', kz_json:objects()} | {'error', build_error()}.
build_im_endpoint(Endpoint, Properties, Im) ->
    case should_create_endpoint(Endpoint, Properties, Im) of
        'ok' -> {'ok', create_im_endpoints(Endpoint, Properties, Im)};
        {'error', _}=E -> E
    end.

-spec should_create_endpoint(kz_json:object(), kz_json:object(), kapps_im:im()) -> 'ok' | {'error', build_error()}.
should_create_endpoint(Endpoint, Properties, Call) ->
    case evaluate_rules_for_creation(Endpoint, Properties, Call) of
        {Endpoint, Properties, Call} -> 'ok';
        {'error', _}=Error -> Error
    end.

-spec evaluate_rules_for_creation(kz_json:object(), kz_json:object(), kapps_im:im()) -> create_ep_acc().
evaluate_rules_for_creation(Endpoint, Properties, Call) ->
    Routines = [fun maybe_endpoint_disabled/3
               ,fun maybe_do_not_disturb/3
               ],
    lists:foldl(fun should_create_endpoint_fold/2
               ,{Endpoint, Properties, Call}
               ,Routines
               ).

-type create_ep_acc() :: {kz_json:object(), kz_json:object(), kapps_im:im()} | {'error', any()}.
-type ep_routine_v() :: fun((kz_json:object(), kz_json:object(), kapps_im:im()) -> 'ok' | _).

-spec should_create_endpoint_fold(ep_routine_v(), create_ep_acc()) -> create_ep_acc().
should_create_endpoint_fold(Routine, {Endpoint, Properties, Call}=Acc)
  when is_function(Routine, 3) ->
    try Routine(Endpoint, Properties, Call) of
        'ok' -> Acc;
        Error -> Error
    catch
        ?CATCH('throw', Error, ST) ->
            ?LOGSTACK(ST),
            Error;
        ?CATCH(_E, _R, ST) ->
            lager:debug("exception ~p:~p", [_E, _R]),
            ?LOGSTACK(ST),
            {'error', 'exception'}
    end;
should_create_endpoint_fold(_Routine, Error) -> Error.

-spec maybe_endpoint_disabled(kz_json:object(), kz_json:object(), kapps_im:im()) -> 'ok' | {'error', 'endpoint_disabled'}.
maybe_endpoint_disabled(Endpoint, _Properties, _Im) ->
    case kz_json:is_false(<<"enabled">>, Endpoint) of
        'false' -> 'ok';
        'true' -> {'error', 'endpoint_disabled'}
    end.

-spec maybe_do_not_disturb(kz_json:object(), kz_json:object(),  kapps_im:im()) -> 'ok' | {'error', 'do_not_disturb'}.
maybe_do_not_disturb(Endpoint, _Properties, _Im) ->
    DND = kz_json:get_json_value(<<"do_not_disturb">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"enabled">>, DND) of
        'false' -> 'ok';
        'true' -> {'error', 'do_not_disturb'}
    end.

-spec create_im_endpoints(kz_json:object(), kz_json:object(), kapps_im:im()) -> kz_json:objects().
create_im_endpoints(Endpoint, Properties, Im) ->
    create_im_endpoints(kzd_endpoint:type(Endpoint), Endpoint, Properties, Im).

-spec create_im_endpoints(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_im:im()) -> kz_json:objects().
create_im_endpoints(<<"device">>, Endpoint, Properties, Im) ->
    [create_im_endpoint(Endpoint, Properties, Im)];
create_im_endpoints(<<"user">>, Endpoint, Properties, Im) ->
    OwnerId = kzd_endpoint:id(Endpoint),
    EndpointIds = [kz_doc:id(EP) || EP
                                        <- kz_attributes:owned_by_docs(OwnerId, kapps_im:account_id(Im))
                                        ,<<"device">> =:= kz_doc:type(EP)
                                        ,lists:member(kzd_devices:device_type(EP), ?SIP_MSG_DEVICES)
                  ],
    EPs = [kz_endpoint:get(EndpointId, kapps_im:account_id(Im)) || EndpointId <- EndpointIds],
    [create_im_endpoint(EP, Properties, Im) || {'ok', EP} <- EPs];
create_im_endpoints(_, _Endpoint, _Properties, _Im) -> [].

-spec create_im_endpoint(kz_json:object(), kz_json:object(), kapps_im:im()) -> kz_json:object().
create_im_endpoint(Endpoint, _Properties, Im) ->
    kz_json:from_list(
      [{<<"To-Username">>, kzd_devices:sip_username(Endpoint)}
      ,{<<"To-Realm">>, kzd_devices:sip_realm(Endpoint, kapps_im:account_realm(Im))}
      ,{<<"To-DID">>, kapps_im:to(Im)}
      ,{<<"Endpoint-ID">>, kzd_endpoint:id(Endpoint)}
      ,{<<"Invite-Format">>, kzd_devices:sip_invite_format(Endpoint)}
      ]).

-spec delivery_status(kz_term:api_object()) -> delivery_status().
delivery_status(JObj) ->
    DeliveryCode = kz_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = kz_json:get_value(<<"Status">>, JObj),
    delivery_status(DeliveryCode, Status).

-spec delivery_status(kz_term:api_binary(), kz_term:api_binary()) -> delivery_status().
delivery_status(<<"sip:", Code/binary>>, Status) -> delivery_status(Code, Status);
delivery_status(<<"200">>, _) -> 'delivered';
delivery_status(<<"202">>, _) -> 'delivered';
delivery_status(_, <<"Success">>) -> 'delivered';
delivery_status(_, _) -> 'failed'.
