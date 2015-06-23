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
-spec save(number()) -> number_return().
-spec save(number(), ne_binary()) -> number_return().
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
-spec delete(number()) -> number_return().
delete(Number) ->
    {'ok', knm_services:deactivate_features(Number, [<<"inbound_cnam">>, <<"outbound_cnam">>, <<"cnam">>])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    CurrentCNAM = wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], Features),
    case wh_json:get_ne_value([?PVT_FEATURES, <<"cnam">>, <<"display_name">>], Doc) of
        'undefined' ->
            Number1 = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            Number1 = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        _Else ->
            Number1 = knm_services:activate_feature(Number, <<"outbound_cnam">>),
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
    Doc = knm_phone_number:doc(Number),
    Number1 =
        case wh_json:is_true([?PVT_FEATURES, <<"cnam">>, <<"inbound_lookup">>], Doc) of
            'false' -> knm_services:deactivate_feature(Number, <<"inbound_lookup">>);
            'true' ->  knm_services:activate_feature(Number, <<"inbound_cnam">>)
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
    {'ok', knm_services:deactivate_feature(Number, <<"cnam">>)}.


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
