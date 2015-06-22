%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_services).

-export([activate_feature/2]).
-export([deactivate_feature/2]).
-export([deactivate_features/2]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(number(), ne_binary()) -> number().
activate_feature(Number, Feature) ->
    knm_phone_number:set_feature(Number, Feature, wh_json:new()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_feature(number(), ne_binary()) -> number().
deactivate_feature(Number, Feature) ->
    Features = knm_phone_number:features(Number),
    knm_phone_number:set_features(Number, wh_json:delete_key(Feature, Features)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_features(number(), ne_binaries()) -> number().
deactivate_features(Number, Features) ->
    Feats = knm_phone_number:features(Number),
    knm_phone_number:set_features(Number, wh_json:delete_keys(Features, Feats)).