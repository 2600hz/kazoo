%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

%%% @doc This module defines the `proper_fsm' behaviour, useful for testing
%%% systems that can be modeled as finite state machines. That is, a finite
%%% collection of named states and transitions between them. `{@module}' is
%%% closely related to {@link proper_statem} and is, in fact, implemented in
%%% terms of that. Testcases generated using `{@module}' will be on precisely
%%% the same form as testcases generated using {@link proper_statem}. The
%%% difference lies in the way the callback modules are specified.
%%% The relation between {@link proper_statem} and `{@module}' is similar
%%% to the one between `gen_server' and `gen_fsm' in OTP libraries.
%%%
%%% Due to name conflicts with functions automatically imported from
%%% {@link proper_statem}, a fully qualified call is needed in order to
%%% use the  <a href="#index">API functions </a> of `{@module}'.
%%%
%%% === The states of the finite state machine ===
%%% Following the convention used in `gen_fsm behaviour', the state is
%%% separated into a `StateName::'{@type state_name()} and some
%%% `StateData::'{@type state_data()}. `StateName' is used to denote a state
%%% of the finite state machine and `StateData' is any relevant information
%%% that has to be stored in the model state. States are fully
%%% represented as tuples `{StateName, StateData}'.
%%%
%%% `StateName' is usually an atom (i.e. the name of the state), but can also
%%% be a tuple. In the latter case, the first element of the tuple must be an
%%% atom specifying the name of the state, whereas the rest of the elements can
%%% be arbitrary terms specifying state attributes. For example, when
%%% implementing the fsm of an elevator which can reach N different floors, the
%%% `StateName' for each floor could be `{floor,K}, 1 <= K <= N'.<br/>
%%% `StateData' can be an arbitrary term, but is usually a record.
%%%
%%% === Transitions between states ===
%%% A transition ({@type transition()}) is represented as a tuple
%%% `{TargetState, {call,M,F,A}}'. This means that performing the specified
%%% symbolic call at the current state of the fsm will lead to `TargetState'.
%%% The atom `history' can be used as `TargetState' to denote that a transition
%%% does not change the current state of the fsm.
%%%
%%% === The callback functions ===
%%% The following functions must be exported from the callback module
%%% implementing the finite state machine:
%%% <ul>
%%% <li> `initial_state() ::' {@type state_name()}
%%%   <p>Specifies the initial state of the finite state machine. As with
%%%   `proper_statem:initial_state/0', its result should be deterministic.
%%%   </p></li>
%%% <li> `initial_state_data() ::' {@type state_data()}
%%%   <p>Specifies what the state data should initially contain. Its result
%%%   should be deterministic</p></li>
%%% <li> `StateName(S::'{@type state_data()}`) ::'
%%%        `['{@type transition()}`]'
%%%   <p>There should be one instance of this function for each reachable
%%%   state `StateName' of the finite state machine. In case `StateName' is a
%%%   tuple the function takes a different form, described just below. The
%%%   function returns a list of possible transitions ({@type transition()})
%%%   from the current state.
%%%   At command generation time, the instance of this function with the same
%%%   name as the current state's name is called to return the list of possible
%%%   transitions. Then, PropEr will randomly choose a transition and,
%%%   according to that, generate the next symbolic call to be included in the
%%%   command sequence. However, before the call is actually included, a
%%%   precondition that might impose constraints on `StateData' is checked.<br/>
%%%   Note also that PropEr detects transitions that would raise an exception
%%%   of class `<error>' at generation time (not earlier) and does not choose
%%%   them. This feature can be used to include conditional transitions that
%%%   depend on the `StateData'.</p></li>
%%% <li> `StateName(Attr1::term(), ..., AttrN::term(),
%%%                 S::'{@type state_data()}`) ::'
%%%        `['{@type transition()}`]'
%%%   <p>There should be one instance of this function for each reachable state
%%%   `{StateName,Attr1,...,AttrN}' of the finite state machine. The function
%%%   has similar beaviour to `StateName/1', described above.</p></li>
%%% <li> `weight(From::'{@type state_name()}`,
%%%              Target::'{@type state_name()}`,
%%%              Call::'{@type symb_call()}`) :: integer()'
%%%   <p>This is an optional callback. When it is not defined (or not exported),
%%%   transitions are chosen with equal probability. When it is defined, it
%%%   assigns an integer weight to transitions from `From' to `Target'
%%%   triggered by symbolic call `Call'. In this case, each transition is chosen
%%%   with probability proportional to the weight assigned.</p></li>
%%% <li> `precondition(From::'{@type state_name()}`,
%%%                    Target::'{@type state_name()}`,
%%%                    StateData::'{@type state_data()}`,
%%%                    Call::'{@type symb_call()}`) :: boolean()'
%%%   <p>Similar to `proper_statem:precondition/2'. Specifies the
%%%   precondition that should hold about `StateData' so that `Call' can be
%%%   included in the command sequence. In case precondition doesn't hold, a
%%%   new transition is chosen using the appropriate `StateName/1' generator.
%%%   It is possible for more than one transitions to be triggered by the same
%%%   symbolic call and lead to different target states. In this case, at most
%%%   one of the target states may have a true precondition. Otherwise, PropEr
%%%   will not be able to detect which transition was chosen and an exception
%%%   will be raised.</p></li>
%%% <li> `postcondition(From::'{@type state_name()}`,
%%%                     Target::'{@type state_name()}`,
%%%                     StateData::'{@type state_data()}`,
%%%                     Call::'{@type symb_call()}`,
%%%                     Res::'{@type result()}`) :: boolean()'
%%%   <p>Similar to `proper_statem:postcondition/3'. Specifies the
%%%   postcondition that should hold about the result `Res' of the evaluation
%%%   of `Call'.</p></li>
%%% <li> `next_state_data(From::'{@type state_name()}`,
%%%                       Target::'{@type state_name()}`,
%%%                       StateData::'{@type state_data()}`,
%%%                       Res::'{@type result()}`,
%%%                       Call::'{@type symb_call()}`) ::'
%%%        {@type state_data()}
%%%   <p>Similar to `proper_statem:next_state/3'. Specifies how the
%%%   transition from `FromState' to `Target' triggered by `Call' affects the
%%%   `StateData'. `Res' refers to the result of `Call' and can be either
%%%   symbolic or dynamic.</p></li>
%%% </ul>
%%%
%%% === The property used ===
%%% This is an example of a property that can be used to test a
%%% finite state machine specification:
%%%
%%% ```prop_fsm() ->
%%%        ?FORALL(Cmds, proper_fsm:commands(?MODULE),
%%%                begin
%%%                    {_History, _State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
%%%                    cleanup(),
%%%                    Result =:= ok
%%%                end).'''
%%% @end

-module(proper_fsm).

-export([behaviour_info/1]).
-export([commands/1, commands/2, run_commands/2, run_commands/3,
	 state_names/1]).
-export([command/1, precondition/2, next_state/3, postcondition/3]).
-export([target_states/4]).

-include("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symb_var()   :: proper_statem:symb_var().
-type symb_call()  :: proper_statem:symb_call().
-type fsm_result() :: proper_statem:statem_result().
-type state_name()   :: atom() | tuple().
%% @type state_data()
-type state_data()   :: term().
-type fsm_state()    :: {state_name(),state_data()}.
-type transition()   :: {state_name(),symb_call()}.
-type command()      :: {'set',symb_var(),symb_call()}
			| {'init',fsm_state()}.
-type command_list() :: [command()].
%% @type cmd_result()
-type cmd_result()   :: term().
-type history()      :: [{fsm_state(),cmd_result()}].
-type tmp_command()  ::   {'init',state()}
		        | {'set',symb_var(),symb_call()}.

-record(state, {name :: state_name(),
		data :: state_data(),
		mod  :: mod_name()}).
-type state() :: #state{}.


%% -----------------------------------------------------------------------------
%% Proper_fsm behaviour
%% ----------------------------------------------------------------------------

%% @doc Specifies the callback functions that should be exported from a module
%% implementing the `proper_fsm' behaviour.

-spec behaviour_info('callbacks') -> [{fun_name(),arity()}].
behaviour_info(callbacks) ->
    [{initial_state,0},
     {initial_state_data,0},
     {precondition,4},
     {postcondition,5},
     {next_state_data,5}];
behaviour_info(_Attribute) ->
    undefined.


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc A special PropEr type which generates random command sequences,
%% according to a finite state machine specification. The function takes as
%% input the name of a callback module, which contains the fsm specification.
%% The initial state is computed by <br/>
%% `{Mod:initial_state/0, Mod:initial_state_data/0}'.

-spec commands(mod_name()) -> proper_types:type().
commands(Mod) ->
    ?LET([_|Cmds],
	 proper_statem:commands(?MODULE, initial_state(Mod)),
	 Cmds).

%% @doc Similar to {@link commands/1}, but generated command sequences always
%% start at a given state. In this case, the first command is always <br/>
%% `{init, InitialState = {Name,Data}}' and is used to correctly initialize the
%% state every time the command sequence is run (i.e. during normal execution,
%% while shrinking and when checking a counterexample).

-spec commands(mod_name(), fsm_state()) -> proper_types:type().
commands(Mod, {Name,Data} = InitialState) ->
    State = #state{name = Name, data = Data, mod = Mod},
    ?LET([_|Cmds],
	 proper_statem:commands(?MODULE, State),
	 [{init,InitialState}|Cmds]).

%% @doc Evaluates a given symbolic command sequence `Cmds' according to the
%% finite state machine specified in `Mod'. The result is a triple of the
%% form<br/> `{History, FsmState, Result}', similar to
%% {@link proper_statem:run_commands/2}.

-spec run_commands(mod_name(), command_list()) ->
         {history(),fsm_state(),fsm_result()}.
run_commands(Mod, Cmds) ->
    run_commands(Mod, Cmds, []).

%% @doc Similar to {@link run_commands/2}, but also accepts an environment
%% used for symbolic variable evaluation, exactly as described in
%% {@link proper_statem:run_commands/3}.

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
         {history(),fsm_state(),fsm_result()}.
run_commands(Mod, Cmds, Env) ->
    Cmds1 = tmp_commands(Mod, Cmds),
    {H,S,Res} = proper_statem:run_commands(?MODULE, Cmds1, Env),
    History = [{{Name,Data},R} || {#state{name = Name, data = Data},R} <- H],
    State = {S#state.name, S#state.data},
    {History, State, Res}.

%% @doc Extracts the names of the states from a given command execution history.
%% It is useful in combination with functions such as {@link proper:aggregate/2}
%% in order to collect statistics about state transitions during command
%% execution.

-spec state_names(history()) -> [state_name()].
state_names(History) ->
    [SName || {{SName,_},_Res} <- History].


%% -----------------------------------------------------------------------------
%% Proper_statem bahaviour callback functions
%% -----------------------------------------------------------------------------

-spec initial_state(mod_name()) -> state().
initial_state(Mod) ->
    S_name = Mod:initial_state(),
    S_data = Mod:initial_state_data(),
    #state{name = S_name, data = S_data, mod = Mod}.

%% @private
-spec command(state()) -> proper_types:type().
command(#state{name = From, data = Data, mod = Mod}) ->
    choose_transition(Mod, From, get_transitions(Mod, From, Data)).

%% @private
-spec precondition(state(), symb_call()) -> boolean().
precondition(#state{name = From, data = Data, mod = Mod}, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    case [To || To <- Targets,
		Mod:precondition(From, cook_history(From, To), Data, Call)] of
	[]   ->
	    false;
	[_T] ->
	    true;
	_ ->
	    io:format(
	      "\nError: The transition from \"~w\" state triggered by ~w "
	      "call leads to multiple target states.\nUse the precondition/5 "
              "callback to specify which target state should be chosen.\n",
	      [From, get_mfa(Call)]),
	    erlang:error(too_many_targets)
    end.

%% @private
-spec next_state(state(), symb_var() | cmd_result(), symb_call()) -> state().
next_state(S = #state{name = From, data = Data, mod = Mod} , Var, Call) ->
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    S#state{name = To,
	    data = Mod:next_state_data(From, To, Data, Var, Call)}.

%% @private
-spec postcondition(state(), symb_call(), cmd_result()) -> boolean().
postcondition(#state{name = From, data = Data, mod = Mod}, Call, Res) ->
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    Mod:postcondition(From, To, Data, Call, Res).


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec tmp_commands(mod_name(), command_list()) -> [tmp_command()].
tmp_commands(Mod, Cmds) ->
    case Cmds of
	[{init, {Name,Data}}|Rest] ->
	    I = #state{name = Name, data = Data, mod = Mod},
	    [{init,I}|Rest];
	Rest ->
	    I = initial_state(Mod),
	    [{init,I}|Rest]
    end.

-spec get_transitions(mod_name(), state_name(), state_data()) ->
         [transition()].
get_transitions(Mod, StateName, Data) ->
    case StateName of
	From when is_atom(From) ->
	    Mod:From(Data);
	From when is_tuple(From) ->
	    Fun = element(1, From),
	    Args = tl(tuple_to_list(From)),
	    apply(Mod, Fun, Args ++ [Data])
    end.

-spec choose_transition(mod_name(), state_name(), [transition()]) ->
         proper_types:type().
choose_transition(Mod, From, T_list) ->
    case is_exported(Mod, {weight,3}) of
	false ->
	    choose_uniform_transition(T_list);
	true ->
	    choose_weighted_transition(Mod, From, T_list)
    end.

-spec choose_uniform_transition([transition()]) -> proper_types:type().
choose_uniform_transition(T_list) ->
    List = [CallGen || {_,CallGen} <- T_list],
    proper_types:safe_union(List).

-spec choose_weighted_transition(mod_name(), state_name(), [transition()]) ->
         proper_types:type().
choose_weighted_transition(Mod, From, T_list) ->
    List = [{Mod:weight(From, cook_history(From, To), CallGen), CallGen}
	    || {To,CallGen} <- T_list],
    proper_types:safe_weighted_union(List).

-spec cook_history(state_name(), state_name()) -> state_name().
cook_history(From, history) -> From;
cook_history(_, To)         -> To.

-spec is_exported(mod_name(), {fun_name(),arity()}) -> boolean().
is_exported(Mod, Fun) ->
    lists:member(Fun, Mod:module_info(exports)).

-spec transition_target(mod_name(), state_name(), state_data(), symb_call()) ->
         state_name().
transition_target(Mod, From, Data, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    [To] = [T || T <- Targets,
		 Mod:precondition(From, cook_history(From, T), Data, Call)],
    To.

%% @private
-spec target_states(mod_name(), state_name(), state_data(), symb_call()) ->
         [state_name()].
target_states(Mod, From, StateData, Call) ->
    find_target(get_transitions(Mod, From, StateData), Call, []).

-spec find_target([transition()], symb_call(), [state_name()]) ->
         [state_name()].
find_target([], _, Accum) -> Accum;
find_target(Transitions, Call, Accum) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_compatible(Call, CallGen) of
	true  -> find_target(Rest, Call, [Target|Accum]);
	false -> find_target(Rest, Call, Accum)
    end.

-spec is_compatible(symb_call(), symb_call()) -> boolean().
is_compatible({call,M,F,A1}, {call,M,F,A2})
  when length(A1) =:= length(A2) ->
    true;
is_compatible(_, _) ->
    false.

-spec get_mfa(symb_call()) -> mfa().
get_mfa({call,M,F,A}) -> {M,F,length(A)}.
