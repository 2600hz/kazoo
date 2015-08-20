%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_prepend).

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(PREPEND_KEY, <<"prepend">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number_return().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number_return().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_prepend(Number);
save(Number, _State) ->
  delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    {'ok', knm_number:knm_number()}.
delete(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?PREPEND_KEY) of
        'undefined' ->
            {'ok', Number};
        _Else ->
            knm_services:deactivate_feature(Number, ?PREPEND_KEY)
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
-spec maybe_update_prepend(knm_number:knm_number()) ->
                                  knm_number_return().
maybe_update_prepend(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentPrepend = wh_json:get_ne_value(?PREPEND_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    Prepend = wh_json:get_ne_value([?PVT_FEATURES ,?PREPEND_KEY], Doc),

    NotChanged = wh_json:are_identical(CurrentPrepend, Prepend),

    case wh_util:is_empty(Prepend) of
        'true' ->
            knm_services:deactivate_feature(Number, ?PREPEND_KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?PREPEND_KEY);
        'false' ->
            case wh_json:is_true(<<"enabled">>, Prepend) of
                'false' ->
                    knm_services:deactivate_feature(Number, ?PREPEND_KEY);
                'true' ->
                    knm_services:activate_feature(Number, ?PREPEND_KEY)
            end
    end.
