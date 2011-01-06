#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a wrapping and overwriting log
%% =============================================================================
main(_) ->
	etap:plan(unknown),
	
	log_roller_cache:start_link(),
	
	etap_exception:lives_ok(fun() ->
		Num = 100,
		Text = "Quisque non metus at justo gravida gravida ",
		Cache = log_roller_cache:add(10240),
		[log_roller_cache:put(Cache, integer_to_list(I), list_to_binary(Text ++ integer_to_list(I))) || I <- lists:seq(1,Num)],
		[begin
			etap:is(binary_to_list(log_roller_cache:get(Cache, integer_to_list(I))), Text ++ integer_to_list(I), "cache value matches")
		 end || I <- lists:seq(1, Num)],
		etap:is(log_roller_cache:items(Cache), 100, "num cache items match ok"),
		ok
	end, "correct values returned from cache"),
	
	etap_exception:lives_ok(fun() ->
		Cache = log_roller_cache:add(320),
		[log_roller_cache:put(Cache, integer_to_list(I), <<I:8/integer>>) || I <- lists:seq(1, 25)],
		etap:is(log_roller_cache:get(Cache, "1"), undefined, "first value overwritten"),
		[etap:is(log_roller_cache:get(Cache, integer_to_list(I)), <<I:8/integer>>, "cache value matches") || I <- lists:seq(2, 25)],
		etap:is(log_roller_cache:items(Cache), 24, "num cache items match ok"),
		ok
	end, "correct values returned from cache"),
	
    etap:end_tests().
	
	