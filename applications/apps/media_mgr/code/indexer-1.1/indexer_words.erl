%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(indexer_words).
-export([words_in_file/3, process_word/2]).

%% Note
%%   some of the
%%   functions in lib_indexer
%%     until we have evaluated indexer:starting
%%     this is because we need to open the trigram tables and convert
%%     filenames to absolute incides


%%   Send {Word, Index} messages to Pid
words_in_file(Pid, File, EtsTrigrams) ->
    Ext = filename:extension(File),
    case lists:member(Ext, allowed_extensions()) of
	true  -> do_indexing(Pid, File, EtsTrigrams);
	false -> void
    end.

allowed_extensions() ->
    [".c", ".txt", ".erl", ".xml",".html",".sgml", ".tex", ".dtd", ".js"].

do_indexing(Pid, File, EtsTrigrams) ->
    Index = indexer_server:filename2index(File),
    io:format("Indexing: ~p as~p~n",[File,Index]),
    Tab = ets:new(void, [ordered_set]),
    indexer_misc:foreach_word_in_file(File, 
				      fun(W, N) -> 
					   process_word(W, Index, 
							Tab, EtsTrigrams, 
							Pid),
					   N+1
				   end, 0),
    ets:delete(Tab).



process_word(Word, Index, Tab, EtsTrigrams, Pid) ->
    case process_word(Word, EtsTrigrams) of
	no -> void;
	{yes, Word1} ->
	    Bin  = list_to_binary(Word1),
	    case ets:lookup(Tab, Bin) of
		[] ->
		    ets:insert(Tab, {Bin}),
		    Pid ! {Word1, Index};
		_ ->
		    void
	    end
    end.



%% @spec process_word(Word, EtsTrigram) -> {yes, Word1} | no 
process_word(Word, EtsTrigrams) when length(Word) < 20 ->
    Word1 = to_lower_case(Word),
    case stop(Word1) of
	true  -> no;
	false ->
	    case indexer_trigrams:is_word(EtsTrigrams, Word1) of
		true ->
		    Word2 = indexer_porter:stem(Word1),
		    if 
			length(Word2) < 3 -> no;
			true              -> {yes, Word2}
		    end;
		false -> no
	    end
    end;
process_word(_, _) ->
    no.


to_lower_case([H|T]) when $A=< H, H=<$Z -> [H+$a-$A|to_lower_case(T)];
to_lower_case([H|T])                    -> [H|to_lower_case(T)];
to_lower_case([])                       -> [].

%%%================================================================
%% An English stop word list from http://snowball.tartarus.org

stop("i") -> true;
stop("me") -> true;
stop("my") -> true;
stop("myself") -> true;
stop("we") -> true;
stop("us") -> true;
stop("our") -> true;
stop("ours") -> true;
stop("ourselves") -> true;
stop("you") -> true;
stop("your") -> true;
stop("yours") -> true;
stop("yourself") -> true;
stop("yourselves") -> true;
stop("he") -> true;
stop("him") -> true;
stop("his") -> true;
stop("himself") -> true;
stop("she") -> true;
stop("her") -> true;
stop("hers") -> true;
stop("herself") -> true;
stop("it") -> true;
stop("its") -> true;
stop("itself") -> true;
stop("they") -> true;
stop("them") -> true;
stop("their") -> true;
stop("theirs") -> true;
stop("themselves") -> true;
stop("what") -> true;
stop("which") -> true;
stop("who") -> true;
stop("whom") -> true;
stop("this") -> true;
stop("that") -> true;
stop("these") -> true;
stop("those") -> true;
stop("am") -> true;
stop("is") -> true;
stop("are") -> true;
stop("was") -> true;
stop("were") -> true;
stop("be") -> true;
stop("been") -> true;
stop("being") -> true;
stop("have") -> true;
stop("has") -> true;
stop("had") -> true;
stop("having") -> true;
stop("do") -> true;
stop("does") -> true;
stop("did") -> true;
stop("doing") -> true;
stop("will") -> true;
stop("would") -> true;
stop("shall") -> true;
stop("should") -> true;
stop("can") -> true;
stop("could") -> true;
stop("may") -> true;
stop("might") -> true;
stop("must") -> true;
stop("ought") -> true;
stop("a") -> true;
stop("an") -> true;
stop("the") -> true;
stop("and") -> true;
stop("but") -> true;
stop("if") -> true;
stop("or") -> true;
stop("because") -> true;
stop("as") -> true;
stop("until") -> true;
stop("while") -> true;
stop("of") -> true;
stop("at") -> true;
stop("by") -> true;
stop("for") -> true;
stop("with") -> true;
stop("about") -> true;
stop("against") -> true;
stop("between") -> true;
stop("into") -> true;
stop("through") -> true;
stop("during") -> true;
stop("before") -> true;
stop("after") -> true;
stop("above") -> true;
stop("below") -> true;
stop("to") -> true;
stop("from") -> true;
stop("up") -> true;
stop("down") -> true;
stop("in") -> true;
stop("out") -> true;
stop("on") -> true;
stop("off") -> true;
stop("over") -> true;
stop("under") -> true;
stop("again") -> true;
stop("further") -> true;
stop("then") -> true;
stop("once") -> true;
stop("here") -> true;
stop("there") -> true;
stop("when") -> true;
stop("where") -> true;
stop("why") -> true;
stop("how") -> true;
stop("all") -> true;
stop("any") -> true;
stop("both") -> true;
stop("each") -> true;
stop("few") -> true;
stop("more") -> true;
stop("most") -> true;
stop("other") -> true;
stop("some") -> true;
stop("such") -> true;
stop("no") -> true;
stop("nor") -> true;
stop("not") -> true;
stop("only") -> true;
stop("own") -> true;
stop("same") -> true;
stop("so") -> true;
stop("than") -> true;
stop("too") -> true;
stop("very") -> true;
stop("one") -> true;
stop("every") -> true;
stop("least") -> true;
stop("less") -> true;
stop("many") -> true;
stop("now") -> true;
stop("ever") -> true;
stop("never") -> true;
stop("say") -> true;
stop("says") -> true;
stop("said") -> true;
stop("also") -> true;
stop("get") -> true;
stop("go") -> true;
stop("goes") -> true;
stop("just") -> true;
stop("made") -> true;
stop("make") -> true;
stop("put") -> true;
stop("see") -> true;
stop("seen") -> true;
stop("whether") -> true;
stop("like") -> true;
stop("well") -> true;
stop("back") -> true;
stop("even") -> true;
stop("still") -> true;
stop("way") -> true;
stop("take") -> true;
stop("since") -> true;
stop("another") -> true;
stop("however") -> true;
stop("two") -> true;
stop("three") -> true;
stop("four") -> true;
stop("five") -> true;
stop("first") -> true;
stop("second") -> true;
stop("new") -> true;
stop("old") -> true;
stop("high") -> true;
stop("long") -> true;
stop(_) -> false.
