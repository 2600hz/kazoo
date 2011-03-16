%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(indexer).
-export([start/0, stop/0, search/1, cold_start/0]).

-import(lists, [map/2]).

%% indexer:cold_start().
%% indexer:start().
%% indexer:stop().
%% indexer:search(Str) -> [File]

cold_start() ->
    indexer_server:cold_start(output_dir(),  dirs_to_index()).

%% Note /home/joe/bigIndex Must be created first

output_dir()    -> "/home/joe/bigIndex".
dirs_to_index() -> ["/home/joe/pre2003/erl.supported"].


start() ->
    indexer_server:start(output_dir()),
    spawn_link(fun() -> worker() end).


search(Str) ->
    indexer_server:search(Str).

stop() ->
    io:format("Scheduling a stop~n"),
    indexer_server:schedule_stop().


worker() ->
    possibly_stop(),
    case indexer_server:next_dir() of
	{ok, Dir} ->
	    Files = indexer_misc:files_in_dir(Dir),
	    index_these_files(Files),
	    indexer_server:checkpoint(),
	    possibly_stop(),
	    sleep(10000),
	    worker();
	done ->
	    true
    end.


possibly_stop() ->
    case indexer_server:should_i_stop() of
	true ->
	    io:format("Stopping~n"),
	    indexer_server:stop(),
	    exit(stopped);
    	false ->
	    void
    end.


index_these_files(Files) ->
    Ets = indexer_server:ets_table(),
    OutDir = filename:join(indexer_server:outdir(), "index"),
    F1 = fun(Pid, File) -> indexer_words:words_in_file(Pid, File, Ets) end,
    F2 = fun(Key, Val, Acc) -> handle_result(Key, Val, OutDir, Acc) end,
    indexer_misc:mapreduce(F1, F2, 0, Files).

handle_result(Key, Vals, OutDir, Acc) ->
    add_to_file(OutDir, Key, Vals),
    Acc + 1.



add_to_file(OutDir, Word, Is) ->
    L1 = map(fun(I) -> <<I:32>> end, Is),
    OutFile = filename:join(OutDir, Word),
    case file:open(OutFile, [write,binary,raw,append]) of
	{ok, S} ->
	    file:pwrite(S, 0, L1),
	    file:close(S);
	{error, E} ->
	      exit({ebadFileOp, OutFile, E})
    end.


sleep(T) ->
    receive
    after T -> true
    end.
