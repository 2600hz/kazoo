%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% Handle failover provisioning
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_failover).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_FAILOVER).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the failover route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_failover(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the failover route
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) =:= 'undefined' of
        'true' -> Number;
        'false' -> knm_services:deactivate_feature(Number, ?KEY)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?KEY).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_failover(knm_number:knm_number()) -> knm_number:knm_number().
update_failover(Number) ->
    CurrentFailover = feature(Number),
    PhoneNumber = knm_number:phone_number(Number),
    Failover = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PhoneNumber)),
    NotChanged = kz_json:are_equal(CurrentFailover, Failover),
    case kz_util:is_empty(Failover) of
        'true' ->
            knm_services:deactivate_feature(Number, ?KEY);
        'false' when NotChanged ->
            Number;
        'false' ->
            knm_services:activate_feature(Number, {?KEY, Failover})
    end.
