%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number_states).

-export([to_reserved/1]).

-include("knm.hrl").

-type kn() :: knm_number:knm_number().

-spec to_reserved(kn()) ->
                         kn().
to_reserved(Number) ->
    to_reserved(Number, number_state(Number)).

to_reserved(Number, ?NUMBER_STATE_RESERVED) ->
    Routines = [fun not_assigning_to_self/1
                ,fun is_auth_by_authorized/1
                ,fun update_reserve_history/1
               ],
    apply_transitions(Number, Routines);
to_reserved(Number, ?NUMBER_STATE_DISCOVERY) ->
    Number;
to_reserved(Number, ?NUMBER_STATE_AVAILABLE) ->
    Number;
to_reserved(Number, ?NUMBER_STATE_IN_SERVICE) ->
    Number;
to_reserved(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_RESERVED).

-spec not_assigning_to_self(kn()) ->
                                   kn().
not_assigning_to_self(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),

    case knm_phone_number:assign_to(PhoneNumber) of
        'undefined' -> knm_errors:unauthorized();
        AssignedTo -> knm_errors:no_change_required(Number);
        _AssignTo -> Number
    end.

-spec is_auth_by_authorized(kn()) -> kn().
is_auth_by_authorized(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    AuthBy = knm_phone_number:auth_by(PhoneNumber),

    case is_authorized_operation(AssignedTo, AuthBy)
        orelse is_authorized_operation(AuthBy, AssignedTo)
    of
        'true' -> Number;
        'false' -> knm_errors:unauthorized()
    end.

-spec update_reserve_history(kn()) -> kn().
update_reserve_history(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    knm_number:set_phone_number(
      Number
      ,knm_phone_number:add_reserve_history(PhoneNumber, AssignTo)
     ).

-type transition() :: fun((kn()) -> kn()).
-type transitions() :: [transition()].

-spec apply_transitions(kn(), transitions()) ->
                               kn().
apply_transitions(Number, Routines) ->
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines).

-spec is_authorized_operation(ne_binary(), ne_binary()) -> boolean().
is_authorized_operation(CheckFor, InAccount) ->
     wh_util:is_in_account_hierarchy(
       CheckFor
       ,InAccount
      ).

-spec number_state(kn()) -> ne_binary().
number_state(Number) ->
    knm_phone_number:state(
      knm_number:phone_number(
        Number
       )
     ).
