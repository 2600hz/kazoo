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
-spec save(knm_number:knm_number()) ->
                  {'ok', knm_number:knm_number()}.
-spec save(knm_number:knm_number(), ne_binary()) ->
                  {'ok', knm_number:knm_number()}.
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
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
-spec delete(knm_number:knm_number()) ->
                    {'ok', knm_number:knm_number()}.
delete(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?FAILOVER_KEY) of
        'undefined' -> {'ok', Number};
        _Else ->
            knm_services:deactivate_feature(Number, ?FAILOVER_KEY)
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
-spec maybe_update_failover(knm_number:knm_number()) ->
                                   knm_number_return().
maybe_update_failover(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentFailover = wh_json:get_ne_value(?FAILOVER_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    Failover = wh_json:get_ne_value([?PVT_FEATURES ,?FAILOVER_KEY], Doc),

    NotChanged = wh_json:are_identical(CurrentFailover, Failover),

    case wh_util:is_empty(Failover) of
        'true' ->
            knm_services:deactivate_feature(Number, ?FAILOVER_KEY);
        'false' when NotChanged ->
            knm_services:deactivate_feature(Number, ?FAILOVER_KEY);
        'false' ->
            knm_services:activate_feature(Number, ?FAILOVER_KEY)
    end.
