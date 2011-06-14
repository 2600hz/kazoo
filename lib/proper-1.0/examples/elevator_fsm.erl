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

-module(elevator_fsm).
-behaviour(gen_fsm).
-behaviour(proper_fsm).

-include_lib("proper/include/proper.hrl").

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).
-export([initial_state/0, initial_state_data/0, precondition/4,
	 next_state_data/5, postcondition/5]).
-compile(export_all).

-record(state, {floor  = 0 :: non_neg_integer(),  %% current floor
		people = 0 :: non_neg_integer(),  %% people inside the elevator
		num_floors :: non_neg_integer(),  %% number of floors in the building
		limit      :: pos_integer()}).    %% max number of people allowed

-record(test_state, {people     = 0  :: non_neg_integer(),
		     num_floors = 5  :: non_neg_integer(),
		     max_people = 10 :: pos_integer()}).


%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------

test() ->
    test(100).

test(Tests) ->
    proper:quickcheck(?MODULE:prop_elevator(), [{numtests,Tests}]).

start_link(Info) ->
    gen_fsm:start_link({local,elevator}, ?MODULE, Info, []).

stop() ->
    gen_fsm:sync_send_all_state_event(elevator, stop).

up() ->
    gen_fsm:send_event(elevator, up).

down() ->
    gen_fsm:send_event(elevator, down).

which_floor() ->
    gen_fsm:sync_send_event(elevator, which_floor).

%% N people try to get on the elevator
get_on(N) ->
    gen_fsm:sync_send_event(elevator, {get_on,N}).

%% N people get off the elevator (assuming at least N people are inside)
get_off(N) ->
    gen_fsm:send_event(elevator, {get_off,N}).


%%--------------------------------------------------------------------
%%% Gen_fsm callbacks
%%--------------------------------------------------------------------

init(Info) ->
    {NumFloors, Limit} = Info,
    {ok, basement, #state{num_floors = NumFloors, limit = Limit}}.

basement(up, S) ->
    case S#state.num_floors > 0 of
	true ->
	    {next_state, floor, S#state{floor = 1}};
	false ->
	    {next_state, basement, S}
    end;
basement(down, S) ->
    {next_state, basement, S};
basement({get_off,N}, S) ->
    People = S#state.people,
    {next_state, basement, S#state{people = People-N}}.

floor(up, S) ->
    Floor = S#state.floor,
    NumFloors = S#state.num_floors,
    case NumFloors > Floor of
	true ->
	    {next_state, floor, S#state{floor = Floor+1}};
	false ->
	    {next_state, floor, S}
    end;
floor(down, S) ->
    case S#state.floor of
	1 ->
	    {next_state, basement, S#state{floor = 0}};
	Floor when Floor > 1 ->
	    {next_state, floor, S#state{floor = Floor-1}}
    end;
floor({get_off,N}, S) ->
    People = S#state.people,
    {next_state, floor, S#state{people = People-N}}.

basement(which_floor, From, S) ->
    gen_fsm:reply(From, S#state.floor),
    {next_state, basement, S};
basement({get_on,N}, From, S) ->
    People = S#state.people,
    case People+N =< S#state.limit of
	true ->
	    gen_fsm:reply(From, People+N),
	    {next_state, basement, S#state{people = People+N}};
	false ->
	    gen_fsm:reply(From, People),
	    {next_state, basement, S}
    end.

floor(which_floor, From, S) ->
    gen_fsm:reply(From, S#state.floor),
    {next_state, floor, S}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(stop, _, _, _) ->
    {stop,normal,ok,[]}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% PropEr elevator specification
%%--------------------------------------------------------------------

initial_state() -> fsm_basement.

initial_state_data() -> #test_state{}.

fsm_basement(S) ->
    [{history,{call,?MODULE,down,[]}},
     {history,{call,?MODULE,which_floor,[]}},
     {history,{call,?MODULE,get_on,[people(S)]}},
     {history,{call,?MODULE,get_off,[people(S)]}},
     {{fsm_floor,1},{call,?MODULE,up,[]}},
     {history,{call,?MODULE,up,[]}}].

fsm_floor(N, S) ->
    [{{fsm_floor,N-1},{call,?MODULE,down,[]}} || N > 1] ++
    [{fsm_basement,{call,?MODULE,down,[]}} || N =:= 1] ++
    [{history,{call,?MODULE,which_floor,[]}},
     {history,{call,?MODULE,get_off,[people(S)]}},
     {{fsm_floor,N+1},{call,?MODULE,up,[]}},
     {history,{call,?MODULE,up,[]}}].

precondition(fsm_basement, {fsm_floor,1}, S, {call,_,up,[]}) ->
    S#test_state.num_floors > 0;
precondition(fsm_basement, fsm_basement, S, {call,_,up,[]}) ->
    S#test_state.num_floors =:= 0;
precondition({fsm_floor,N}, {fsm_floor,M}, S, {call,_,up,[]})
  when M =:= N + 1 ->
    S#test_state.num_floors > N;
precondition({fsm_floor,N}, {fsm_floor,N}, S, {call,_,up,[]}) ->
    S#test_state.num_floors =:= N;
precondition({fsm_floor,_}, {fsm_floor,_}, _S, {call,_,up,[]}) ->
    false;
precondition(_, _, S, {call,_,get_off,[N]}) ->
    N =< S#test_state.people;
precondition(_, _, _, _) ->
    true.

next_state_data(_, _, S, _, {call,_,get_off,[N]}) ->
    S#test_state{people = S#test_state.people - N};
next_state_data(_, _, S, _, {call,_,get_on,[N]}) ->
    People = S#test_state.people,
    case S#test_state.max_people < People + N of
	true -> S;
	false -> S#test_state{people = People + N}
    end;
next_state_data(_, _, S, _, _) ->
    S.

postcondition(_, _, S, {call,_,get_on,[N]}, R) ->
    People = S#test_state.people,
    case S#test_state.max_people < People + N of
	true -> R =:= People;
	false -> R =:= N + People
    end;
postcondition(fsm_basement, fsm_basement, _, {call,_,which_floor,[]}, 0) ->
    true;
postcondition({fsm_floor,N}, {fsm_floor,N}, _, {call,_,which_floor,[]}, N) ->
    true;
postcondition(_, _, _, {call,_,which_floor,[]}, _) ->
    false;
postcondition(_, _, _, _, R) ->
    R == ok.

prop_elevator() ->
    ?FORALL(
       {NumFloors,MaxPeople}, {num_floors(), max_people()},
       begin
	   Initial = {fsm_basement,
		      #test_state{num_floors = NumFloors,
				  max_people = MaxPeople,
				  people = 0}},
	   ?FORALL(
	      Cmds, more_commands(5, proper_fsm:commands(?MODULE, Initial)),
	      begin
		  ?MODULE:start_link({NumFloors,MaxPeople}),
		  {H,S,Res} = proper_fsm:run_commands(?MODULE, Cmds),
		  ?MODULE:stop(),
		  ?WHENFAIL(
		     io:format("H: ~w\nS: ~w\nR: ~w\n", [H,S,Res]),
		     aggregate(zip(proper_fsm:state_names(H),
				   command_names(Cmds)),
			       Res == ok))
	      end)
       end).

people(S) ->
    ?SUCHTHAT(X, pos_integer(), X =< S#test_state.max_people).

max_people() ->
    noshrink(integer(5, 20)).

num_floors() ->
    noshrink(integer(1, 4)).
