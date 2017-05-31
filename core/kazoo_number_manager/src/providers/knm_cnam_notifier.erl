%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_cnam_notifier).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    handle(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle(Number);
save(Number, _State) ->
    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    knm_services:deactivate_features(Number
                                    ,[?FEATURE_CNAM_INBOUND
                                     ,?FEATURE_CNAM_OUTBOUND
                                     ,?FEATURE_CNAM
                                     ]
                                    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle(knm_number:knm_number()) -> knm_number:knm_number().
handle(Number) ->
    support_depreciated_cnam(
      handle_inbound_cnam(
        handle_outbound_cnam(
          Number
         )
       )
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_outbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            knm_services:deactivate_feature(Number, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> Number;
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            Number1 = knm_services:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            _ = publish_cnam_update(Number1),
            Number1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_inbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        false ->
            knm_services:deactivate_features(Number, [?FEATURE_CNAM_INBOUND
                                                     ,?CNAM_INBOUND_LOOKUP
                                                     ]);
        'true' ->
            case kz_json:is_true(?CNAM_INBOUND_LOOKUP, Feature) of
                'true' -> Number;
                'false' ->
                    FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
                    knm_services:activate_feature(Number, {?FEATURE_CNAM_INBOUND, FeatureData})
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_cnam(knm_number:knm_number()) -> knm_number:knm_number().
support_depreciated_cnam(Number) ->
    knm_services:deactivate_feature(Number, ?FEATURE_CNAM).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(knm_number:knm_number()) -> 'ok'.
-spec publish_cnam_update(knm_number:knm_number(), boolean()) -> 'ok'.
publish_cnam_update(Number) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    publish_cnam_update(Number, DryRun).

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
    kapi_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
