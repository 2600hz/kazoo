#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% =============================================================================
main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	log_roller_test:load_apps(),
	put(log_dir, log_roller_test:rnd_dir()),
	application:set_env(log_roller_server, log_dir, get(log_dir)),
	application:set_env(log_roller_server, maxbytes, 256),
	application:set_env(log_roller_server, maxfiles, 3),
	log_roller_test:start_apps(),
	
	[error_logger:info_msg("ABCD ~w~n", [I]) || I <- lists:seq(1,22)],
	
	log_roller_test:wait_for_queue_to_empty(),
	ok = log_roller_disk_logger:sync(default),
	
	Logs = log_roller_raw:read(default),
	Logs1 = [{A,B} || [A,{log_entry,_,info,nonode@nohost,B}] <- Logs],
	etap:is(Logs1, [{3,"ABCD 16\n"},
	                {3,"ABCD 15\n"},
	                {3,"ABCD 14\n"},
	                {2,"ABCD 22\n"},
	                {2,"ABCD 21\n"},
	                {2,"ABCD 20\n"},
	                {1,"ABCD 19\n"},
	                {1,"ABCD 18\n"},
	                {1,"ABCD 17\n"}], "logs match ok"),
	
    etap:end_tests().