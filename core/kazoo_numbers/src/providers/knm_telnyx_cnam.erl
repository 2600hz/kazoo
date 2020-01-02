%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_telnyx_cnam).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PhoneNumber) ->
    State = knm_phone_number:state(PhoneNumber),
    save(PhoneNumber, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PhoneNumber, ?NUMBER_STATE_RESERVED) ->
    handle(PhoneNumber);
save(PhoneNumber, ?NUMBER_STATE_IN_SERVICE) ->
    handle(PhoneNumber);
save(PhoneNumber, ?NUMBER_STATE_PORT_IN) ->
    handle(PhoneNumber);
save(PhoneNumber, _State) ->
    PhoneNumber.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    toggle_inbound(PN, 'false'),
    knm_providers:deactivate_features(PN
                                     ,[?FEATURE_CNAM_INBOUND
                                      ,?FEATURE_CNAM_OUTBOUND
                                      ,?FEATURE_CNAM
                                      ]
                                     ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(knm_phone_number:record()) -> knm_phone_number:record().
handle(PhoneNumber) ->
    support_depreciated_cnam(
      handle_inbound_cnam(
        handle_outbound_cnam(
          PhoneNumber
         )
       )
     ).

-spec handle_outbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_outbound_cnam(PhoneNumber) ->
    IsDryRun = knm_phone_number:dry_run(PhoneNumber),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            knm_providers:deactivate_feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> PhoneNumber;
        NewCNAM when IsDryRun ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            knm_providers:activate_feature(PhoneNumber, {?FEATURE_CNAM_OUTBOUND, NewCNAM});
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            PhoneNumber1 = knm_providers:activate_feature(PhoneNumber, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            set_outbound(PhoneNumber1, NewCNAM),
            publish_cnam_update(PhoneNumber1),
            PhoneNumber1
    end.

-spec handle_inbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_inbound_cnam(PhoneNumber) ->
    CurrentInboundCNAM = kz_json:is_true(?CNAM_INBOUND_LOOKUP
                                        ,knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND)),
    InboundCNAM = kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP]
                                 ,knm_phone_number:doc(PhoneNumber)
                                 ),
    NotChanged = CurrentInboundCNAM =:= InboundCNAM,
    case InboundCNAM of
        'false' when NotChanged -> PhoneNumber;
        'false' ->
            toggle_inbound(PhoneNumber, 'false'),
            knm_providers:deactivate_features(PhoneNumber, [?FEATURE_CNAM_INBOUND
                                                           ,?CNAM_INBOUND_LOOKUP
                                                           ]);
        'true' when NotChanged -> PhoneNumber;
        'true' ->
            toggle_inbound(PhoneNumber, 'true'),
            FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, 'true'}]),
            knm_providers:activate_feature(PhoneNumber, {?FEATURE_CNAM_INBOUND, FeatureData})
    end.

-spec support_depreciated_cnam(knm_phone_number:record()) -> knm_phone_number:record().
support_depreciated_cnam(PhoneNumber) ->
    knm_providers:deactivate_feature(PhoneNumber, ?FEATURE_CNAM).

-spec toggle_inbound(knm_phone_number:record(), boolean()) -> 'ok'.
toggle_inbound(PhoneNumber, ShouldEnable) ->
    case knm_phone_number:dry_run(PhoneNumber) of
        'true' -> 'ok';
        'false' ->
            Key = <<"enable_caller_id_name">>,
            Body = kz_json:from_list([{Key, ShouldEnable}]),
            Rep = knm_telnyx_util:req('put', ["numbers", knm_telnyx_util:did(PhoneNumber)], Body),
            ShouldEnable = kz_json:is_true(Key, Rep),
            'ok'
    end.

-spec set_outbound(knm_phone_number:record(), kz_term:ne_binary()) -> 'ok'.
set_outbound(Number, NewCNAM) ->
    Key = <<"cnam_listing_details">>,
    Body = kz_json:from_list([{Key, NewCNAM}
                             ,{<<"enable_caller_id_name">>, 'true'}
                             ]),
    Rep = knm_telnyx_util:req('put', ["numbers", knm_telnyx_util:did(Number)], Body),
    NewCNAM = kz_json:get_ne_binary_value(Key, Rep),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_cnam_update(knm_phone_number:record()) -> 'ok'.
%% FIXME: maybe move TEST guard to ampq polisher. This way we could actually test
%% the message is building properly and validate.
-ifndef(TEST).
publish_cnam_update(PN) ->
    Feature = knm_phone_number:feature(PN, ?FEATURE_CNAM),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PN)}
             ,{<<"Number-State">>, knm_phone_number:state(PN)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PN))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PN)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
-else.
publish_cnam_update(_Number) -> 'ok'.
-endif.
