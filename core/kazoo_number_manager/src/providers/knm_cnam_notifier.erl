%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
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

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_RESERVED) ->
    handle(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle(Number);
save(Number, _State) ->
    Number.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    knm_providers:deactivate_features(Number
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
-spec handle(knm_number:knm_number()) -> knm_number:knm_number().
handle(Number) ->
    support_depreciated_cnam(
      handle_inbound_cnam(
        handle_outbound_cnam(
          Number
         )
       )
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_outbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_outbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            knm_providers:deactivate_feature(Number, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> Number;
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            Number1 = knm_providers:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            _ = publish_cnam_update(Number1),
            Number1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_inbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_inbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        false ->
            knm_providers:deactivate_features(Number, [?FEATURE_CNAM_INBOUND
                                                      ,?CNAM_INBOUND_LOOKUP
                                                      ]);
        'true' ->
            case kz_json:is_true(?CNAM_INBOUND_LOOKUP, Feature) of
                'true' -> Number;
                'false' ->
                    FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
                    knm_providers:activate_feature(Number, {?FEATURE_CNAM_INBOUND, FeatureData})
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec support_depreciated_cnam(knm_number:knm_number()) -> knm_number:knm_number().
support_depreciated_cnam(Number) ->
    knm_providers:deactivate_feature(Number, ?FEATURE_CNAM).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec publish_cnam_update(knm_number:knm_number()) -> 'ok'.
publish_cnam_update(Number) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    publish_cnam_update(Number, DryRun).

-spec publish_cnam_update(knm_number:knm_number(), boolean()) -> 'ok'.
publish_cnam_update(_Number, 'true') -> 'ok';
publish_cnam_update(Number, 'false') ->
    PhoneNumber = knm_number:phone_number(Number),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PhoneNumber))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
