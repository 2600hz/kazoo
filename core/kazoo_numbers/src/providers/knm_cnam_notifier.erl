%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_cnam_notifier).
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
save(PN) ->
    State = knm_phone_number:state(PN),
    save(PN, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_RESERVED) ->
    handle(PN);
save(PN, ?NUMBER_STATE_IN_SERVICE) ->
    handle(PN);
save(PN, ?NUMBER_STATE_PORT_IN) ->
    handle(PN);
save(PN, _State) ->
    PN.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    Features = [?FEATURE_CNAM_INBOUND
               ,?FEATURE_CNAM_OUTBOUND
               ,?FEATURE_CNAM
               ],
    knm_providers:deactivate_features(PN, Features).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(knm_phone_number:record()) -> knm_phone_number:record().
handle(PN) ->
    support_depreciated_cnam(
      handle_inbound_cnam(
        handle_outbound_cnam(
          PN
         )
       )
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_outbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_outbound_cnam(PhoneNumber) ->
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            knm_providers:deactivate_feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> PhoneNumber;
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            PhoneNumber1 = knm_providers:activate_feature(PhoneNumber, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            publish_cnam_update(PhoneNumber1),
            PhoneNumber1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_inbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_inbound_cnam(PN) ->
    Doc = knm_phone_number:doc(PN),
    Feature = knm_phone_number:feature(PN, ?FEATURE_CNAM_INBOUND),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        'false' ->
            knm_providers:deactivate_features(PN, [?FEATURE_CNAM_INBOUND
                                                  ,?CNAM_INBOUND_LOOKUP
                                                  ]);
        'true' ->
            case kz_json:is_true(?CNAM_INBOUND_LOOKUP, Feature) of
                'true' -> PN;
                'false' ->
                    FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, 'true'}]),
                    knm_providers:activate_feature(PN, {?FEATURE_CNAM_INBOUND, FeatureData})
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec support_depreciated_cnam(knm_phone_number:record()) -> knm_phone_number:record().
support_depreciated_cnam(PN) ->
    knm_providers:deactivate_feature(PN, ?FEATURE_CNAM).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec publish_cnam_update(knm_phone_number:record()) -> 'ok'.
publish_cnam_update(PN) ->
    DryRun = knm_phone_number:dry_run(PN),
    publish_cnam_update(PN, DryRun).

-spec publish_cnam_update(knm_phone_number:record(), boolean()) -> 'ok'.
publish_cnam_update(_PN, 'true') -> 'ok';
publish_cnam_update(PN, 'false') ->
    Feature = knm_phone_number:feature(PN, ?FEATURE_CNAM_OUTBOUND),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PN)}
             ,{<<"Number-State">>, knm_phone_number:state(PN)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PN))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PN)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
