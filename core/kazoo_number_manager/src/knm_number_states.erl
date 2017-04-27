%%--------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number_states).

-export([to_reserved/1
        ,to_in_service/1
        ,to_aging/1
        ,to_port_in/1
        ,to_state/2, to_state/3
        ]).

-include("knm.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type kn() :: knm_number:knm_number().

-spec to_state(ne_binary() | kn(), ne_binary()) -> kn().
-spec to_state(ne_binary() | kn(), ne_binary(), kz_proplist()) -> kn().
to_state(DID, ToState) ->
    to_state(DID, ToState, knm_number_options:default()).
to_state(?NE_BINARY = DID, ToState, Options) ->
    case knm_number:get(DID, Options) of
        {'error', E} -> knm_errors:unspecified(E, DID);
        {'ok', Number} -> change_state(Number, ToState)
    end;
to_state(Number, ToState, _Options) ->
    change_state(Number, ToState).

-spec change_state(kn(), ne_binary()) -> kn().
change_state(Number, ?NUMBER_STATE_RESERVED) ->
    to_reserved(Number);
change_state(Number, ?NUMBER_STATE_IN_SERVICE) ->
    to_in_service(Number);
change_state(Number, ?NUMBER_STATE_AVAILABLE) ->
    to_available(Number);
change_state(Number, ?NUMBER_STATE_AGING) ->
    to_aging(Number);
change_state(Number, ?NUMBER_STATE_PORT_IN) ->
    to_port_in(Number);
change_state(Number, State) ->
    lager:debug("unhandled state change to ~p", [State]),
    knm_errors:invalid_state_transition(Number, knm_phone_number:state(Number), State).

-spec to_port_in(kn()) -> kn().
-spec to_port_in(kn(), ne_binary()) -> kn().
to_port_in(Number) ->
    _ = fail_if_mdn(Number, ?NUMBER_STATE_PORT_IN),
    to_port_in(Number, number_state(Number)).

to_port_in(Number, ?NUMBER_STATE_PORT_IN) ->
    Routines = [fun move_to_port_in_state/1
               ],
    apply_transitions(Number, Routines);
to_port_in(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_PORT_IN).

-spec to_aging(kn()) -> kn().
-spec to_aging(kn(), ne_binary()) -> kn().
to_aging(Number) ->
    _ = fail_if_mdn(Number, ?NUMBER_STATE_AGING),
    to_aging(Number, number_state(Number)).

to_aging(Number, ?NUMBER_STATE_AGING) ->
    Routines = [fun move_to_aging_state/1
               ],
    apply_transitions(Number, Routines);
to_aging(Number, ?NUMBER_STATE_RELEASED) ->
    Routines = [fun move_to_aging_state/1
               ],
    apply_transitions(Number, Routines);
to_aging(Number, ?NUMBER_STATE_AVAILABLE) ->
    Routines = [fun move_to_aging_state/1
               ],
    apply_transitions(Number, Routines);
to_aging(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_AGING).

-spec to_available(kn()) -> kn().
-spec to_available(kn(), ne_binary()) -> kn().
to_available(Number) ->
    _ = fail_if_mdn(Number, ?NUMBER_STATE_AVAILABLE),
    to_available(Number, number_state(Number)).

to_available(Number, ?NUMBER_STATE_AVAILABLE) ->
    Routines = [fun authorize/1
               ,fun move_to_available_state/1
               ,fun knm_services:activate_phone_number/1
               ],
    apply_transitions(Number, Routines);
to_available(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_AVAILABLE).

-spec to_reserved(kn()) -> kn().
-spec to_reserved(kn(), ne_binary()) -> kn().
to_reserved(Number) ->
    _ = fail_if_mdn(Number, ?NUMBER_STATE_RESERVED),
    to_reserved(Number, number_state(Number)).

to_reserved(Number, ?NUMBER_STATE_RESERVED) ->
    Routines = [fun not_assigning_to_self/1
               ,fun is_auth_by_authorized/1
               ,fun update_reserve_history/1
               ,fun move_to_reserved_state/1
               ,fun knm_services:activate_phone_number/1
               ,fun knm_carriers:acquire/1
               ],
    apply_transitions(Number, Routines);
to_reserved(Number, ?NUMBER_STATE_DISCOVERY) ->
    Routines = [fun authorize/1
               ,fun update_reserve_history/1
               ,fun move_to_reserved_state/1
               ,fun knm_services:activate_phone_number/1
               ,fun knm_carriers:acquire/1
               ],
    apply_transitions(Number, Routines);
to_reserved(Number, ?NUMBER_STATE_AVAILABLE) ->
    Routines = [fun authorize/1
               ,fun update_reserve_history/1
               ,fun move_to_reserved_state/1
               ,fun knm_services:activate_phone_number/1
               ],
    apply_transitions(Number, Routines);
to_reserved(Number, ?NUMBER_STATE_IN_SERVICE) ->
    Routines = [fun authorize/1
               ,fun update_reserve_history/1
               ,fun move_to_reserved_state/1
               ],
    apply_transitions(Number, Routines);
to_reserved(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_RESERVED).

-spec to_in_service(kn()) -> kn().
-spec to_in_service(kn(), ne_binary()) -> kn().
to_in_service(Number) ->
    to_in_service(Number, number_state(Number)).

to_in_service(Number, ?NUMBER_STATE_IN_SERVICE) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    case knm_phone_number:assigned_to(PhoneNumber) of
        AssignTo -> Number;
        _AssignedTo ->
            Routines = [fun in_service_from_in_service_authorize/1
                       ,fun move_to_in_service_state/1
                       ],
            apply_transitions(Number, Routines)
    end;
to_in_service(Number, ?NUMBER_STATE_DISCOVERY) ->
    Routines = [fun (N) -> fail_if_mdn(N, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_DISCOVERY) end
               ,fun authorize/1
               ,fun move_to_in_service_state/1
               ,fun knm_services:activate_phone_number/1
               ,fun knm_carriers:acquire/1
               ],
    apply_transitions(Number, Routines);
to_in_service(Number, ?NUMBER_STATE_PORT_IN) ->
    Routines = [fun (N) -> fail_if_mdn(N, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_PORT_IN) end
               ,fun authorize/1
               ,fun move_to_in_service_state/1
               ],
    apply_transitions(Number, Routines);
to_in_service(Number, ?NUMBER_STATE_AVAILABLE) ->
    %% Everyone MUST be allowed to buy available
    %% External carriers MUST NOT be contacted
    Routines = [fun (N) -> fail_if_mdn(N, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_AVAILABLE) end
               ,fun move_to_in_service_state/1
               ,fun knm_services:activate_phone_number/1
               ],
    apply_transitions(Number, Routines);
to_in_service(Number, ?NUMBER_STATE_RESERVED) ->
    Routines = [fun (N) -> fail_if_mdn(N, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED) end
               ,fun in_service_from_reserved_authorize/1
               ,fun move_to_in_service_state/1
               ],
    apply_transitions(Number, Routines);
to_in_service(Number, State) ->
    knm_errors:invalid_state_transition(Number, State, ?NUMBER_STATE_IN_SERVICE).

-spec authorize(kn()) -> kn().
authorize(N) ->
    case knm_phone_number:is_authorized(knm_number:phone_number(N)) of
        true -> N;
        false -> knm_errors:unauthorized()
    end.

-ifdef(TEST).
-define(ACCT_HIERARCHY(AuthBy, AssignTo, _)
       ,AuthBy =:= ?MASTER_ACCOUNT_ID
        andalso AssignTo =:= ?RESELLER_ACCOUNT_ID
       ).
-else.
-define(ACCT_HIERARCHY(AuthBy, AssignTo, Bool)
       ,kz_util:is_in_account_hierarchy(AuthBy, AssignTo, Bool)
       ).
-endif.

-spec in_service_from_reserved_authorize(kn()) -> kn().
in_service_from_reserved_authorize(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    AuthBy = knm_phone_number:auth_by(PhoneNumber),
    Sudo = ?KNM_DEFAULT_AUTH_BY =:= AuthBy,
    case ?ACCT_HIERARCHY(AssignedTo, AssignTo, 'true')
        andalso (
          Sudo
          orelse ?ACCT_HIERARCHY(AssignedTo, AuthBy, 'true')
          orelse ?ACCT_HIERARCHY(AuthBy, AssignedTo, 'false')
         )
    of
        'false' -> knm_errors:unauthorized();
        'true' ->
            Sudo
                andalso lager:info("bypassing auth"),
            Number
    end.

-spec in_service_from_in_service_authorize(kn()) -> kn().
in_service_from_in_service_authorize(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    AuthBy = knm_phone_number:auth_by(PhoneNumber),
    Sudo = ?KNM_DEFAULT_AUTH_BY =:= AuthBy,
    case Sudo
        orelse ?ACCT_HIERARCHY(AssignTo, AuthBy, 'true')
        orelse ?ACCT_HIERARCHY(AuthBy, AssignTo, 'false')
    of
        'false' -> knm_errors:unauthorized();
        'true' ->
            Sudo
                andalso lager:info("bypassing auth"),
            Number
    end.

-spec not_assigning_to_self(kn()) -> kn().
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
    Sudo = ?KNM_DEFAULT_AUTH_BY =:= AuthBy,
    case Sudo
        orelse 'undefined' =:= AssignedTo
        orelse is_authorized_operation(AssignedTo, AuthBy)
        orelse is_authorized_operation(AuthBy, AssignedTo)
    of
        'true' ->
            Sudo
                andalso lager:info("bypassing auth"),
            Number;
        'false' -> knm_errors:unauthorized()
    end.

-spec update_reserve_history(kn()) -> kn().
update_reserve_history(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    PN = knm_phone_number:add_reserve_history(AssignTo, PhoneNumber),
    knm_number:set_phone_number(Number, PN).

-spec move_to_port_in_state(kn()) -> kn().
move_to_port_in_state(Number) ->
    move_number_to_state(Number, ?NUMBER_STATE_PORT_IN).

-spec move_to_aging_state(kn()) -> kn().
move_to_aging_state(Number) ->
    move_number_to_state(Number, ?NUMBER_STATE_AGING).

-spec move_to_available_state(kn()) -> kn().
move_to_available_state(Number) ->
    move_number_to_state(Number, ?NUMBER_STATE_AVAILABLE).

-spec move_to_reserved_state(kn()) -> kn().
move_to_reserved_state(Number) ->
    move_number_to_state(Number, ?NUMBER_STATE_RESERVED).

-spec move_to_in_service_state(kn()) -> kn().
move_to_in_service_state(Number) ->
    move_number_to_state(Number, ?NUMBER_STATE_IN_SERVICE).

-spec move_number_to_state(kn(), ne_binary()) -> kn().
move_number_to_state(Number, ToState) ->
    PhoneNumber = knm_number:phone_number(Number),
    {'ok', PN} = move_phone_number_to_state(PhoneNumber, ToState),
    knm_number:set_phone_number(Number, PN).

-spec move_phone_number_to_state(knm_phone_number:knm_phone_number(), ne_binary()) ->
                                        knm_phone_number_return().
move_phone_number_to_state(PN, ToState=?NUMBER_STATE_AVAILABLE) ->
    knm_phone_number:setters(PN, [{fun knm_phone_number:set_state/2, ToState}]);
move_phone_number_to_state(PN, ToState) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    move_phone_number_to_state(PN, ToState, AssignedTo).

-spec move_phone_number_to_state(knm_phone_number:knm_phone_number(), ne_binary(), api_binary()) ->
                                        knm_phone_number_return().
move_phone_number_to_state(PhoneNumber, ToState, 'undefined') ->
    Setters =
        [{fun knm_phone_number:set_assigned_to/2, knm_phone_number:assign_to(PhoneNumber)}
        ,{fun knm_phone_number:set_state/2, ToState}
        ],
    knm_phone_number:setters(PhoneNumber, Setters);
move_phone_number_to_state(PhoneNumber, ToState, AssignedTo) ->
    AssignTo = knm_phone_number:assign_to(PhoneNumber),
    move_phone_number_to_state(PhoneNumber, ToState, AssignedTo, AssignTo).

move_phone_number_to_state(PhoneNumber, ToState, AssignTo, AssignTo) ->
    Routines = [{fun knm_phone_number:set_state/2, ToState}
               ],
    knm_phone_number:setters(PhoneNumber, Routines);
move_phone_number_to_state(PhoneNumber, ToState, AssignedTo, AssignTo) ->
    Setters = [{fun knm_phone_number:set_assigned_to/2, AssignTo}
              ,{fun knm_phone_number:set_state/2, ToState}
              ],
    knm_phone_number:setters(PhoneNumber, Setters).

-type transition() :: fun((kn()) -> kn()).
-type transitions() :: [transition()].

-spec apply_transitions(kn(), transitions()) -> kn().
apply_transitions(Number, Routines) ->
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines).

-spec is_authorized_operation(ne_binary(), ne_binary()) -> boolean().
is_authorized_operation(CheckFor, InAccount) ->
    kz_util:is_in_account_hierarchy(CheckFor, InAccount).

-spec number_state(kn()) -> ne_binary().
number_state(Number) ->
    knm_phone_number:state(
      knm_number:phone_number(Number)).

is_mdn(N) ->
    ?CARRIER_MDN =:= knm_phone_number:module_name(knm_number:phone_number(N)).

fail_if_mdn(N, ToState) ->
    is_mdn(N)
        andalso knm_errors:invalid_state_transition(N, <<"'MDN'">>, ToState),
    N.

fail_if_mdn(N, FromState, ToState) ->
    is_mdn(N)
        andalso knm_errors:invalid_state_transition(N, FromState, ToState),
    N.
