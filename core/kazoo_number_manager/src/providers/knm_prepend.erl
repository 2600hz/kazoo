%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_prepend).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

-include("knm.hrl").

-define(PREPEND_KEY, <<"prepend">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) ->
                  knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_prepend(Number);
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
                    knm_number:knm_number().
delete(Number) ->
    case knm_phone_number:feature(knm_number:phone_number(Number), ?PREPEND_KEY) of
        'undefined' -> Number;
        _Else -> knm_services:deactivate_feature(Number, ?PREPEND_KEY)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:knm_number()) -> boolean().
has_emergency_services(_Number) -> 'false'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_prepend(knm_number:knm_number()) ->
                            knm_number:knm_number().
update_prepend(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    CurrentPrepend = kz_json:get_ne_value(?PREPEND_KEY, Features),

    Doc = knm_phone_number:doc(PhoneNumber),
    Prepend = kz_json:get_ne_value([?PVT_FEATURES ,?PREPEND_KEY], Doc),

    NotChanged = kz_json:are_identical(CurrentPrepend, Prepend),

    case kz_util:is_empty(Prepend) of
        'true' ->
            knm_services:deactivate_feature(Number, ?PREPEND_KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?PREPEND_KEY);
        'false' ->
            case kz_json:is_true(<<"enabled">>, Prepend) of
                'false' ->
                    knm_services:deactivate_feature(Number, ?PREPEND_KEY);
                'true' ->
                    knm_services:activate_feature(Number, ?PREPEND_KEY)
            end
    end.
