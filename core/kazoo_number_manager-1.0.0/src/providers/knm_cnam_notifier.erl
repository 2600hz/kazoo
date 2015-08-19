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

-define(FEATURE_OUTBOUND_CNAM, <<"outbound_cnam">>).
-define(FEATURE_INBOUND_CNAM, <<"inbound_cnam">>).
-define(FEATURE_CNAM, <<"cnam">>).

-define(KEY_DISPLAY_NAME, <<"display_name">>).
-define(KEY_INBOUND_LOOKUP, <<"inbound_lookup">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_phone_number:knm_number()) -> number_return().
-spec save(knm_phone_number:knm_number(), ne_binary()) -> number_return().
save(Number) ->
    State = knm_phone_number:state(Number),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle_outbound_cnam(Number);
save(Number, _State) -> {'ok', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number:knm_number()) ->
                    {'ok', knm_phone_number:knm_number()}.
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
-spec handle_outbound_cnam(knm_phone_number:knm_number()) -> number_return().
handle_outbound_cnam(Number) ->
    Doc = knm_phone_number:doc(Number),
    Features = knm_phone_number:features(Number),
    CurrentCNAM = wh_json:get_ne_value([?FEATURE_CNAM, ?KEY_DISPLAY_NAME], Features),
    case wh_json:get_ne_value([?PVT_FEATURES, ?FEATURE_CNAM, ?KEY_DISPLAY_NAME], Doc) of
        'undefined' ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            handle_inbound_cnam(Number1);
        _Else ->
            {'ok', Number1} = knm_services:activate_feature(Number, ?FEATURE_OUTBOUND_CNAM),
            _ = publish_cnam_update(Number1),
            handle_inbound_cnam(Number1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(knm_phone_number:knm_number()) ->
                                 {'ok', knm_phone_number:knm_number()}.
handle_inbound_cnam(Number) ->
    Doc = knm_phone_number:doc(Number),
    {'ok', Number1} =
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
-spec support_depreciated_cnam(knm_phone_number:knm_number()) ->
                                      {'ok', knm_phone_number:knm_number()}.
support_depreciated_cnam(Number) ->
    knm_services:deactivate_feature(Number, ?FEATURE_CNAM).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(knm_phone_number:knm_number()) -> 'ok'.
-spec publish_cnam_update(knm_phone_number:knm_number(), boolean()) -> 'ok'.
publish_cnam_update(Number) ->
    DryRun = knm_phone_number:dry_run(Number),
    publish_cnam_update(Number, DryRun).

publish_cnam_update(_Number, 'true') -> 'ok';
publish_cnam_update(Number, 'false') ->
    Features = knm_phone_number:features(Number),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(Number)}
              ,{<<"Number-State">>, knm_phone_number:state(Number)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(Number) =:= ?LOCAL_CARRIER}
              ,{<<"Number">>, knm_phone_number:number(Number)}
              ,{<<"Acquired-For">>, knm_phone_number:auth_by(Number)}
              ,{<<"Cnam">>, wh_json:get_value(?FEATURE_CNAM, Features, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
