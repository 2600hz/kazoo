%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(wordcount).
-import(lists, [all/2, filter/2, foreach/2, map/2,reverse/1]).
-export([dir/1, wordcount/1]).

dir(Dir) ->
    HashTable = new_hash_table(),
    Files = files_in_dir(Dir),
    foreach(
      fun(File) ->
	      Words = words_in_file(File),
	      io:format("File:~p #words=~p~n",[File,length(Words)]),
	      foreach(fun(Word) -> 
			      insert(Word, HashTable) 
		      end,
		      Words)
      end, Files),
    show_hash_table(HashTable),
    save_hashtable_on_disk(HashTable),
    ets:delete(HashTable).


files_in_dir(Dir) ->
    {ok, Things} = file:list_dir(Dir),
    filter(fun filelib:is_regular/1, Things).


words_in_file(File) ->
    {ok, Bin} = file:read_file(File),
    W = string:tokens(binary_to_list(Bin),
		      ":\s\r\n\t.,;-+()[]{}~*=%&#\"!-_"),
    W1 = filter(fun is_word/1, W),
    map(fun to_lower/1, W1).


to_lower(Word) -> map(fun lib_misc:downcase_char/1, Word).
    
is_word(L) -> all(fun is_lower/1, L).

is_lower(X) when $a =< X, X =< $z -> true;
is_lower(_) -> false.


new_hash_table() ->
    ets:new(wordCount, []).  %% (1)

insert(Word, HashTable) ->
    case ets:lookup(HashTable, Word) of  %% (2)
	[] ->                            %% (3)
	    ets:insert(HashTable, {Word,1});
	[{_,N}] ->                       %% (4)
	    ets:insert(HashTable, {Word,N+1})
    end.

show_hash_table(HashTable) ->		  
    Info = ets:info(HashTable),            %% (5)
    io:format("#table Info=~p~n",[Info]),
    L = ets:tab2list(HashTable),           %% (6)
    L1 = reverse(lists:keysort(2, L)),
    io:format("Common words =~p~n", [lists:sublist(L1, 10)]).



save_hashtable_on_disk(HashTable) ->
    {ok, wordcount} = dets:open_file(wordcount, 
				     [{file,["./wordcount.dets"]}]),
    ets:to_dets(HashTable, wordcount), 
    ok = dets:close(wordcount).


wordcount(Word) ->
    {ok, wordcount} = dets:open_file(wordcount, 
				     [{file,["./wordcount.dets"]}]),
    Count = case dets:lookup(wordcount, Word) of
		[]      -> 0;
		[{_,N}] -> N
	    end,
    ok = dets:close(wordcount),
    Count.

