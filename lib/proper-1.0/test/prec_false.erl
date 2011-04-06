-module(prec_false).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-record(state, {step = 0}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[]},
	   {call,?MODULE,bar,[]}]).

precondition(#state{step=Step}, _) ->
    Step < 5.

next_state(#state{step=Step}, _, _) ->
    #state{step=Step+1}.

postcondition(_, _, _) ->
    true.

foo() -> ok.
bar() -> 42.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
