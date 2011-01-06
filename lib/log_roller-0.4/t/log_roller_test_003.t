#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a large log set that wraps, but does not overlap on itself
%% =============================================================================
main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	log_roller_test:load_apps(),
	put(log_dir, log_roller_test:rnd_dir()),
	application:set_env(log_roller_server, log_dir, get(log_dir)),
	application:set_env(log_roller_server, cache_size, 9000000),
	application:set_env(log_roller_server, maxbytes, 9000000),
	application:set_env(log_roller_server, maxfiles, 10),
	
	application:start(log_roller_server),
	
	etap:is(length(fetch(default, [{cache, false}, {timeout, 2000}], [])), 0, "empty log returns 0 terms ok"),
	
	application:start(log_roller),
			
	error_logger:error_msg("Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy"),
	Text = "Quisque non metus at justo gravida gravida. Vivamus ullamcorper eros sed dui. In ultrices dui vel leo. Duis nisi massa, vestibulum sed, mattis quis, mollis sit amet, urna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer velit nunc, ultrices vitae, sagittis sit amet, euismod in, leo. Sed bibendum, ipsum at faucibus vulputate, est ipsum mollis odio, quis iaculis justo purus non nisl. Aenean tellus nisl, pellentesque in, consectetur non, vestibulum sit amet, nibh. Donec diam. Quisque eros. Etiam dictum tellus et ante. Donec fermentum lectus non augue. Maecenas justo. Aenean et metus ac nunc pharetra congue. Mauris rhoncus justo vitae tortor. Sed ornare tristique neque. In eu enim auctor sem tincidunt vestibulum. Aliquam erat volutpat. Nulla et diam ac magna porttitor molestie. Vestibulum massa erat, tristique sed, venenatis et, sagittis in, mauris.",
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,1000)],

	%% NOTE: since info messages are being sent to the log_roller handler asynchronously
	%% and then cached in the disk_log gen_server we must both wait for the disk_log to 
	%% receive them all and then flush the disk_log cache before trying to read from disk
	io:format("waiting for write to disk~n"),
	log_roller_test:wait_for_queue_to_empty(),
	ok = log_roller_disk_logger:sync(default),

	%etap:is(length(log_roller_raw:read(default)), 1002, "raw ok"),
	
	%etap:is(length(fetch(default, [{cache, false}], [])), 1002, "fetch ok"),
	
	etap:is(length(fetch(default, [{cache, true}], [])), 1002, "fetch ok"),
	
	{_,_,_,_,_,_,_,Cache} = lrb:disk_logger_by_name(default, lrb:disk_loggers()),
	CacheSize = log_roller_cache:size(Cache),
	DiskSize = proplists:get_value(no_current_bytes, disk_log:info(default), 0),
	Diff = (CacheSize/DiskSize),
	etap:ok(Diff < 1.01 andalso Diff > 0.99, "cache size matches ok"), 
	
 	ok = log_roller_test:teardown_test(),

	etap:end_tests().

fetch(undefined, _, Acc) -> Acc;
fetch(Continuation, Opts, Acc) ->
	{Cont, Results} = lrb:fetch(Continuation, Opts),
	fetch(Cont, Opts, lists:append(Acc, Results)).