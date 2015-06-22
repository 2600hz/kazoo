%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
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
-spec save(number()) -> number_return().
-spec save(number(), ne_binary()) -> number_return().
save(Number) ->
    State = knm_phone_number:state(Number),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    update_cnam_features(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_cnam_features(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    update_cnam_features(Number);
save(Number, _State) -> {'ok', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(number()) -> number_return().
delete(Number) ->
    Features = knm_phone_number:features(Number),
    UpdatedFeatures = wh_json:delete_keys([<<"inbound_cnam">>, <<"outbound_cnam">>, <<"cnam">>], Features),
    {'ok', knm_phone_number:set_features(Number, UpdatedFeatures)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_cnam_features(number()) -> number_return().
update_cnam_features(Number) ->
    handle_outbound_cnam(Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(number()) -> number_return().
handle_outbound_cnam(Number) ->
    Doc = knm_phone_number:doc(Number),
    Features = knm_phone_number:features(Number),
    CurrentCNAM = wh_json:get_ne_value([<<"features">>, <<"cnam">>, <<"display_name">>], Doc),
    case wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], Features) of
        'undefined' ->
            Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(<<"outbound_cnam">>, Features)),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(<<"outbound_cnam">>, Features)),
            handle_inbound_cnam(Number1);
        _Else ->
            Number1 = number:activate_feature(<<"outbound_cnam">>, Number),
            _ = publish_cnam_update(Number1),
            handle_inbound_cnam(Number1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(number()) -> number_return().
handle_inbound_cnam(Number) ->
    Features = knm_phone_number:features(Number),
    Number1 =
        case wh_json:is_true([<<"cnam">>, <<"inbound_lookup">>], Features) of
            'false' -> knm_phone_number:set_features(Number, wh_json:delete_key(<<"inbound_lookup">>, Features));
            'true' ->  number:activate_feature(<<"inbound_cnam">>, Number)
        end,
    support_depreciated_cnam(Number1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_cnam(number()) -> number_return().
support_depreciated_cnam(Number) ->
    Features = knm_phone_number:features(Number),
    Number1 = knm_phone_number:set_features(Number, wh_json:delete_key(<<"cnam">>, Features)),
    {'ok', Number1}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(number()) -> 'ok'.
-spec publish_cnam_update(number(), boolean()) -> 'ok'.
publish_cnam_update(Number) ->
    DryRun = knm_phone_number:dry_run(Number),
    publish_cnam_update(Number, DryRun).

publish_cnam_update(_Number, 'true') -> 'ok';
publish_cnam_update(Number, 'false') ->
    Features = knm_phone_number:features(Number),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(Number)}
              ,{<<"Number-State">>, knm_phone_number:state(Number)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(Number) =:= 'local'}
              ,{<<"Number">>, knm_phone_number:number(Number)}
              ,{<<"Acquired-For">>, knm_phone_number:auth_by(Number)}
              ,{<<"Cnam">>, wh_json:get_value(<<"cnam">>, Features, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
