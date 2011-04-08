-module(post_false).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-record(state, {step = 0}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[]},
	   {call,?MODULE,bar,[]}]).

precondition(_, _) ->
    true.

next_state(#state{step=Step}, _, _) ->
    #state{step=Step+1}.

postcondition(#state{step=Step}, _, _) ->
    Step < 5.

foo() -> ok.
bar() -> 42.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
