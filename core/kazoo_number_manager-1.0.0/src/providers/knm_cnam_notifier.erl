%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_cnam_notifier).

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) ->
                  knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle_outbound_cnam(Number);
save(Number, _State) ->
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
    knm_services:deactivate_features(Number, [?FEATURE_INBOUND_CNAM
                                              ,?FEATURE_OUTBOUND_CNAM
                                              ,?FEATURE_CNAM
                                             ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(knm_number:knm_number()) ->
                                  knm_number:knm_number().
handle_outbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Features = knm_phone_number:features(PhoneNumber),

    CurrentCNAM = wh_json:get_ne_value([?FEATURE_CNAM, ?KEY_DISPLAY_NAME], Features),
    case wh_json:get_ne_value([?PVT_FEATURES, ?FEATURE_CNAM, ?KEY_DISPLAY_NAME], Doc) of
        'undefined' ->
            Number1 = knm_services:deactivate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            Number1 = knm_services:deactivate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            handle_inbound_cnam(Number1);
        _Else ->
            Number1 = knm_services:activate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            _ = publish_cnam_update(Number1),
            handle_inbound_cnam(Number1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(knm_number:knm_number()) ->
                                 knm_number:knm_number().
handle_inbound_cnam(Number) ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    Number1 =
        case wh_json:is_true([?PVT_FEATURES, ?FEATURE_CNAM, ?KEY_INBOUND_LOOKUP], Doc) of
            'false' -> knm_services:deactivate_feature(Number, ?KEY_INBOUND_LOOKUP);
            'true' ->  knm_services:activate_feature(Number, ?FEATURE_INBOUND_CNAM)
        end,
    support_depreciated_cnam(Number1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_cnam(knm_number:knm_number()) ->
                                      knm_number:knm_number().
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
    Features = knm_phone_number:features(PhoneNumber),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
              ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
              ,{<<"Number">>, knm_phone_number:number(PhoneNumber)}
              ,{<<"Acquired-For">>, knm_phone_number:auth_by(PhoneNumber)}
              ,{<<"Cnam">>, wh_json:get_value(?FEATURE_CNAM, Features, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
