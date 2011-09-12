-module(trycatch).

-export([test/0, test/1]).

-include_lib("whistle/include/wh_log.hrl").

-define(BIG_WAIT, 50).
-define(SMALL_WAIT, 25).

test() ->
    test(1000).

test(MaxLoops) ->
    ?LOG("starting loop with 1000 timeout"),
    spawn(fun() -> loop(MaxLoops, ?BIG_WAIT, 0) end).

loop(0, _, MaxHeap) ->
    ?LOG("looping, done. Max heap: ~p", [MaxHeap]);
loop(Tries, _Timeout, MaxHeap) ->
    put(callid, wh_util:to_binary(Tries)),

    {_, Heap} = erlang:process_info(self(), total_heap_size),
    ?LOG("total_heap: ~p", [Heap]),

    NewMax = case Heap > MaxHeap of
		 true -> Heap;
		 false -> MaxHeap
	     end,

    %% timer:sleep(Timeout),
    try
	case random:uniform(100) of
	    X when X < 10 -> ?LOG("exception!"), throw(waa);
	    _ -> ?LOG("looping"), loop(Tries-1, ?BIG_WAIT, NewMax)
	end
    catch
	_E:_R ->
	    ?LOG("Caught exception"),
	    {_, Heap1} = erlang:process_info(self(), total_heap_size),
	    ?LOG("total_heap: ~p", [Heap]),

	    NewMax1 = case Heap1 > MaxHeap of
			 true -> Heap1;
			 false -> MaxHeap
		     end,

	    loop(Tries-1, ?SMALL_WAIT, NewMax1)
    end.
