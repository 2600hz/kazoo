%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%% Handle failover provisioning
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_failover).

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(FAILOVER_KEY, <<"failover">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the failover route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(number()) -> number_return().
-spec save(number(), ne_binary()) -> number_return().
save(Number) ->
    State = knm_phone_number:state(Number),
    save(Number, State).

save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_failover(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the failover route
%% @end
%%--------------------------------------------------------------------
-spec delete(number()) -> number_return().
delete(Number) ->
    case knm_phone_number:feature(Number, ?FAILOVER_KEY) of
        'undefined' -> {'ok', Number};
        _Else ->
            {'ok', knm_services:deactivate_feature(Number, ?FAILOVER_KEY)}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_failover(number()) -> number_return().
maybe_update_failover(Number) ->
    Features = knm_phone_number:features(Number),
    CurrentFailover = wh_json:get_ne_value(?FAILOVER_KEY, Features),

    Doc = knm_phone_number:doc(Number),
    Failover = wh_json:get_ne_value([?PVT_FEATURES ,?FAILOVER_KEY], Doc),

    NotChanged = wh_json:are_identical(CurrentFailover, Failover),

    case wh_util:is_empty(Failover) of
        'true' ->
            {'ok', knm_services:deactivate_feature(Number, ?FAILOVER_KEY)};
        'false' when NotChanged ->
            {'ok', knm_services:deactivate_feature(Number, ?FAILOVER_KEY)};
        'false' ->
            {'ok', knm_services:activate_feature(Number, ?FAILOVER_KEY)}
    end.
