-module(ets_counter).
-include_lib("proper/include/proper.hrl").

-compile(export_all).
-define(KEYS, lists:seq(1,10)).

ets_inc(Key, Inc) ->
    case ets:lookup(counter, Key) of
	[] ->
	    timer:sleep(1),
	    ets:insert(counter, {Key,Inc}),
	    Inc;
	[{Key,OldValue}] ->
	    NewValue = OldValue + Inc,
	    ets:insert(counter, {Key,NewValue}),
	    NewValue
    end.

prop_ets_counter() ->
    ?FORALL(Commands, parallel_commands(?MODULE),
	    begin
		set_up(),
		{Seq,P,Result} = run_parallel_commands(?MODULE, Commands),
		clean_up(),
		?WHENFAIL(io:format("Seq: ~w\nPar: ~w\nRes: ~w\n",
				    [Seq, P, Result]),
			  Result =:= ok)
	    end).

set_up() ->
    ets:new(counter, [public, named_table]).

clean_up() ->
    catch ets:delete(counter).

key() ->
    elements(?KEYS).

initial_state() -> [].

precondition(_S, _C) ->
    true.

command(_S) ->
    {call,?MODULE,ets_inc,[key(),non_neg_integer()]}.

postcondition(S, {call,_,ets_inc,[Key, Inc]}, Res) ->
    case proplists:is_defined(Key, S) of
	true ->
	    OldValue = proplists:get_value(Key, S),
	    Res =:= OldValue + Inc;
	false ->
	    Res =:= Inc
    end.
	
next_state(S, _Res, {call,_,ets_inc,[Key, Inc]}) ->
    case proplists:is_defined(Key, S) of
	 true ->
	     OldValue = proplists:get_value(Key, S),
	     NewValue = OldValue + Inc,
	     [{Key,NewValue}|proplists:delete(Key, S)];
	 false ->
	     [{Key,Inc}|S]
    end.
