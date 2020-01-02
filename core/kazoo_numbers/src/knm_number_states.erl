%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_number_states).

-export([to_options_state/1]).

-include("knm.hrl").

-define(TO_STATE2(ToStateF, T0),
        knm_pipe:merge_okkos(
          [begin
               %% FIXME: opaque
               NewT0 = T0#{'todo' => [], 'succeeded' => PNs},
               knm_pipe:do(fun (T) -> ToStateF(T, State) end, NewT0)
           end
           || {State, PNs} <- group_by_state(T0),
              [] =/= PNs
          ])).

-spec to_options_state(knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
to_options_state(T=#{'options' := Options}) ->
    TargetState = knm_number_options:state(Options),
    lager:debug("attempting to change to state ~s", [TargetState]),
    change_state(T, TargetState).

-spec change_state(knm_pipe:collection(), kz_term:ne_binary()) -> knm_pipe:collection().
change_state(T, ?NUMBER_STATE_RESERVED) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_RESERVED) end
                  ,fun to_reserved/1
                  ]);
change_state(T, ?NUMBER_STATE_IN_SERVICE) ->
    to_in_service(T);
change_state(T, ?NUMBER_STATE_AVAILABLE) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_AVAILABLE) end
                  ,fun to_available/1
                  ]);
change_state(T, ?NUMBER_STATE_AGING) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_AGING) end
                  ,fun to_aging/1
                  ]);
change_state(T, ?NUMBER_STATE_PORT_IN) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_PORT_IN) end
                  ,fun to_port_in/1
                  ]);
%% FIXME: opaque
change_state(T=#{'todo' := PNs}, _State) ->
    lager:debug("unhandled state change to ~p", [_State]),
    Error = knm_errors:to_json('invalid_state_transition'),
    knm_pipe:set_failed(T, PNs, Error).

-spec to_port_in(knm_pipe:collection()) -> knm_pipe:collection().
to_port_in(T0) -> ?TO_STATE2('to_port_in', T0).

to_port_in(T, ?NUMBER_STATE_PORT_IN) ->
    knm_pipe:pipe(T
                 ,[fun move_to_port_in_state/1
                  ]);
to_port_in(T, State) ->
    invalid_state_transition(T, State, ?NUMBER_STATE_PORT_IN).

-spec to_aging(knm_pipe:collection()) -> knm_pipe:collection().
to_aging(T0) -> ?TO_STATE2('to_aging', T0).

to_aging(T, ?NUMBER_STATE_AGING) ->
    knm_pipe:pipe(T
                 ,[fun move_to_aging_state/1
                  ]);
to_aging(T, ?NUMBER_STATE_RELEASED) ->
    knm_pipe:pipe(T
                 ,[fun move_to_aging_state/1
                  ]);
to_aging(T, ?NUMBER_STATE_AVAILABLE) ->
    knm_pipe:pipe(T
                 ,[fun move_to_aging_state/1
                  ]);
to_aging(T, State) ->
    invalid_state_transition(T, State, ?NUMBER_STATE_AGING).

-spec to_available(knm_pipe:collection()) -> knm_pipe:collection().
to_available(T0) -> ?TO_STATE2('to_available', T0).

to_available(T, ?NUMBER_STATE_AVAILABLE) ->
    knm_pipe:pipe(T
                 ,[fun authorize/1
                  ,fun move_to_available_state/1
                  ]);
to_available(T, State) ->
    invalid_state_transition(T, State, ?NUMBER_STATE_AVAILABLE).

-spec to_reserved(knm_pipe:collection()) -> knm_pipe:collection().
to_reserved(T0) -> ?TO_STATE2('to_reserved', T0).

to_reserved(T, ?NUMBER_STATE_RESERVED) ->
    knm_pipe:pipe(T
                 ,[fun not_assigning_to_self/1
                  ,fun authorize/1
                  ,fun update_reserve_history/1
                  ,fun move_to_reserved_state/1
                  ,fun knm_carriers:acquire/1
                  ]);
to_reserved(T, ?NUMBER_STATE_DISCOVERY) ->
    knm_pipe:pipe(T
                 ,[fun authorize/1
                  ,fun update_reserve_history/1
                  ,fun move_to_reserved_state/1
                  ,fun knm_carriers:acquire/1
                  ]);
to_reserved(T, ?NUMBER_STATE_AVAILABLE) ->
    knm_pipe:pipe(T
                 ,[fun authorize/1
                  ,fun update_reserve_history/1
                  ,fun move_to_reserved_state/1
                  ]);
to_reserved(T, ?NUMBER_STATE_IN_SERVICE) ->
    knm_pipe:pipe(T
                 ,[fun authorize/1
                  ,fun update_reserve_history/1
                  ,fun move_to_reserved_state/1
                  ]);
to_reserved(T, State) ->
    invalid_state_transition(T, State, ?NUMBER_STATE_RESERVED).

-spec to_in_service(knm_pipe:collection()) -> knm_pipe:collection().
to_in_service(T0) -> ?TO_STATE2('to_in_service', T0).

%% FIXME: opaque
to_in_service(T=#{'todo' := PNs}, ?NUMBER_STATE_IN_SERVICE) ->
    {Yes, No} = lists:partition(fun is_assigned_to_assignto/1, PNs),
    Ta = knm_pipe:set_succeeded(T, Yes),
    Tb = knm_pipe:pipe(knm_pipe:set_todo(T, No)
                      ,[fun authorize/1
                       ,fun move_to_in_service_state/1
                       ]),
    knm_pipe:merge_okkos(Ta, Tb);
to_in_service(T, ?NUMBER_STATE_DISCOVERY) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_DISCOVERY) end
                  ,fun authorize/1
                  ,fun move_to_in_service_state/1
                  ,fun knm_carriers:acquire/1
                  ]);
to_in_service(T, ?NUMBER_STATE_PORT_IN) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_PORT_IN) end
                  ,fun authorize/1
                  ,fun move_to_in_service_state/1
                  ]);
to_in_service(T, ?NUMBER_STATE_AVAILABLE) ->
    %% Everyone MUST be allowed to buy available
    %% External carriers MUST NOT be contacted
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_AVAILABLE) end
                  ,fun move_to_in_service_state/1
                  ]);
to_in_service(T, ?NUMBER_STATE_AGING) ->
    %% Allow everyone to rebuy aging numbers in case the number is accidentally deleted
    %% External carriers MUST NOT be contacted
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_AGING) end
                  ,fun move_to_in_service_state/1
                  ]);
to_in_service(T, ?NUMBER_STATE_RESERVED) ->
    knm_pipe:pipe(T
                 ,[fun (T0) -> fail_if_mdn(T0, ?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED) end
                  ,fun authorize_subaccount/1
                  ,fun move_to_in_service_state/1
                  ]);
to_in_service(T, State) ->
    invalid_state_transition(T, State, ?NUMBER_STATE_IN_SERVICE).

-spec authorize_subaccount(knm_pipe:collection()) -> knm_pipe:collection().
authorize_subaccount(T) ->
    knm_pipe:do(fun knm_phone_number:is_reserved_from_parent/1, T).

-spec authorize(knm_pipe:collection()) -> knm_pipe:collection().
authorize(T) ->
    knm_pipe:do(fun knm_phone_number:is_authorized/1, T).

-spec not_assigning_to_self(knm_phone_number:record()) -> knm_phone_number:record();
                           (knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
not_assigning_to_self(T0=#{'todo' := PNs}) ->
    F = fun (PN, T) ->
                case knm_pipe:attempt(fun not_assigning_to_self/1, [PN]) of
                    {'ok', NewPN} -> knm_pipe:set_succeeded(T, NewPN);
                    {'error', R} -> knm_pipe:set_failed(T, PN, R)
                end
        end,
    lists:foldl(F, T0, PNs);
not_assigning_to_self(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case knm_phone_number:assign_to(PN) of
        'undefined' -> knm_errors:unauthorized();
        AssignedTo -> knm_errors:no_change_required(PN);
        _AssignTo -> PN
    end.

-spec update_reserve_history(knm_pipe:collection()) -> knm_pipe:collection().
update_reserve_history(T) ->
    knm_pipe:do(fun knm_phone_number:push_reserve_history/1, T).

move_to_port_in_state(T) ->
    move_number_to_state(T, ?NUMBER_STATE_PORT_IN).
move_to_aging_state(T) ->
    move_number_to_state(T, ?NUMBER_STATE_AGING).
move_to_available_state(T) ->
    move_number_to_state(T, ?NUMBER_STATE_AVAILABLE).
move_to_reserved_state(T) ->
    move_number_to_state(T, ?NUMBER_STATE_RESERVED).
move_to_in_service_state(T) ->
    move_number_to_state(T, ?NUMBER_STATE_IN_SERVICE).

-spec move_number_to_state(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record();
                          (knm_pipe:collection(), kz_term:ne_binary()) -> knm_pipe:collection().
%% FIXME: opaque
move_number_to_state(T=#{'todo' := PNs}, ToState) ->
    NewPNs = [move_number_to_state(PN, ToState) || PN <- PNs],
    knm_pipe:set_succeeded(T, NewPNs);
move_number_to_state(PN, ToState) ->
    {'ok', NewPN} = move_phone_number_to_state(PN, ToState),
    NewPN.

-spec move_phone_number_to_state(knm_phone_number:record(), kz_term:ne_binary()) ->
          knm_phone_number:return().
move_phone_number_to_state(PN, ToState=?NUMBER_STATE_AVAILABLE) ->
    knm_phone_number:setters(PN, [{fun knm_phone_number:set_state/2, ToState}]);
move_phone_number_to_state(PN, ToState) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    move_phone_number_to_state(PN, ToState, AssignedTo).

-spec move_phone_number_to_state(knm_phone_number:record(), kz_term:ne_binary(), kz_term:api_binary()) ->
          knm_phone_number:return().
move_phone_number_to_state(PN, ToState, 'undefined') ->
    Setters =
        [{fun knm_phone_number:set_assigned_to/2, knm_phone_number:assign_to(PN)}
        ,{fun knm_phone_number:set_state/2, ToState}
        ],
    knm_phone_number:setters(PN, Setters);
move_phone_number_to_state(PN, ToState, AssignedTo) ->
    AssignTo = knm_phone_number:assign_to(PN),
    move_phone_number_to_state(PN, ToState, AssignedTo, AssignTo).

move_phone_number_to_state(PN, ToState, AssignTo, AssignTo) ->
    Routines = [{fun knm_phone_number:set_state/2, ToState}
               ],
    knm_phone_number:setters(PN, Routines);
move_phone_number_to_state(PN, ToState, _AssignedTo, AssignTo) ->
    Setters = [{fun knm_phone_number:set_assigned_to/2, AssignTo}
              ,{fun knm_phone_number:set_state/2, ToState}
              ],
    knm_phone_number:setters(PN, Setters).


-spec invalid_state_transition(knm_pipe:collection(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> knm_pipe:collection().
%% FIXME: opaque
invalid_state_transition(T=#{'todo' := PNs}, FromState, ToState) ->
    {'error',A,B,C} = (catch knm_errors:invalid_state_transition('undefined', FromState, ToState)),
    Reason = knm_errors:to_json(A, B, C),
    knm_pipe:set_failed(T, PNs, Reason).

%% FIXME: opaque
fail_if_mdn(T=#{'todo' := PNs}, ToState) ->
    case lists:partition(fun is_mdn/1, PNs) of
        {[], _} -> knm_pipe:set_succeeded(T, PNs);
        {MDNs, OtherPNs} ->
            Ta = knm_pipe:set_succeeded(T, OtherPNs),
            Tb = invalid_state_transition(knm_pipe:set_todo(T, MDNs), <<"'MDN'">>, ToState),
            knm_pipe:merge_okkos(Ta, Tb)
    end.

%% FIXME: opaque
fail_if_mdn(T=#{'todo' := PNs}, FromState, ToState) ->
    case lists:partition(fun is_mdn/1, PNs) of
        {[], _} -> knm_pipe:set_succeeded(T, PNs);
        {MDNs, OtherPNs} ->
            Ta = knm_pipe:set_succeeded(T, OtherPNs),
            Tb = invalid_state_transition(knm_pipe:set_todo(T, MDNs), FromState, ToState),
            knm_pipe:merge_okkos(Ta, Tb)
    end.

is_mdn(PN) ->
    ?CARRIER_MDN =:= knm_phone_number:module_name(PN).

is_assigned_to_assignto(PN) ->
    knm_phone_number:assign_to(PN)
        =:= knm_phone_number:assigned_to(PN).

%% FIXME: opaque
-spec group_by_state(knm_pipe:collection()) -> [{kz_term:ne_binary(), knm_pipe:succeeded()}].
group_by_state(#{'todo' := PNs}) ->
    F = fun (PN, Acc) ->
                State = knm_phone_number:state(PN),
                Acc#{State => [PN | maps:get(State, Acc, [])]}
        end,
    maps:to_list(lists:foldl(F, #{}, PNs)).
