-module(error_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-record(state, {step = 0}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[integer()]},
	   {call,?MODULE,bar,[]}]).

precondition(_, _) ->
    true.

next_state(#state{step=Step}, _, _) ->
    #state{step=Step+1}.

postcondition(_, _, _) ->
    true.

foo(I) ->
    case I > 10 of
	false -> ok;
	true  -> throw(badarg)
    end.

bar() -> 42.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
