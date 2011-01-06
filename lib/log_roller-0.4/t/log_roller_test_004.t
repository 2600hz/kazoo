#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a wrapping and overwriting log
%% =============================================================================
main(_) ->
    etap:plan(unknown),

	log_roller_test:load_apps(),
	put(log_dir, log_roller_test:rnd_dir()),
	application:set_env(log_roller_server, log_dir, get(log_dir)),
	application:set_env(log_roller_server, cache_size, 250000),
	application:set_env(log_roller_server, maxbytes, 250000),
	application:set_env(log_roller_server, maxfiles, 5),
	log_roller_test:start_apps(),
	
	Num = 1000,
	Text = "Quisque non metus at justo gravida gravida. Vivamus ullamcorper eros sed dui. In ultrices dui vel leo. Duis nisi massa, vestibulum sed, mattis quis, mollis sit amet, urna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer velit nunc, ultrices vitae, sagittis sit amet, euismod in, leo. Sed bibendum, ipsum at faucibus vulputate, est ipsum mollis odio, quis iaculis justo purus non nisl. Aenean tellus nisl, pellentesque in, consectetur non, vestibulum sit amet, nibh. Donec diam. Quisque eros. Etiam dictum tellus et ante. Donec fermentum lectus non augue. Maecenas justo. Aenean et metus ac nunc pharetra congue. Mauris rhoncus justo vitae tortor. Sed ornare tristique neque. In eu enim auctor sem tincidunt vestibulum. Aliquam erat volutpat. Nulla et diam ac magna porttitor molestie. Vestibulum massa erat, tristique sed, venenatis et, sagittis in, mauris.",

	io:format("sending logs~n"),
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],

	%% NOTE: since info messages are being sent to the log_roller handler asynchronously
	%% and then cached in the disk_log gen_server we must both wait for the disk_log to 
	%% receive them all and then flush the disk_log cache before trying to read from disk
	io:format("waiting for write to disk~n"),
	log_roller_test:wait_for_queue_to_empty(),
	ok = log_roller_disk_logger:sync(default),
	
	io:format("fetching~n"),
	etap_exception:lives_ok(fun() ->
		etap:is(length(lrb:fetch(default, [{max, Num}])), Num, "fetched correct number of results"),
		etap:is(length(lrb:fetch(default, [{max, Num}])), Num, "fetched correct number of results"),
		ok
	end, "fetch log"),
	
	application:stop(log_roller_server),
	application:start(log_roller_server),
	
	io:format("sending logs~n"),
	[error_logger:error_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],
	
	io:format("waiting for write to disk~n"),
	log_roller_test:wait_for_queue_to_empty(),
	ok = log_roller_disk_logger:sync(default),
	
	io:format("fetching~n"),
	etap_exception:lives_ok(fun() ->
		etap:is(length(lrb:fetch(default, [{types, [error]}, {max, Num}])), Num, "fetched correct number of results"),
		ok
	end, "fetch log"),
	
	ok = log_roller_test:teardown_test(),
	
	%stop_watch:print(),
	
    etap:end_tests().
