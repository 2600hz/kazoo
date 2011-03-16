%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(test_name_server).

-compile(export_all).

%% tests the examples in the book
%%   ** remember the code path must be able to find this directory
%%      and the code directory (above this)

test(1) ->  
    kvs:start(),
    lib_chan:start_server();

test(2) ->
    {ok, P} = lib_chan:connect("localhost", 1234, nameServer, "ABXy45", nil),
    io:format("ok P=~p~n",[P]),
    lib_chan_mm:trace_with_tag(P, junk),
    lib_chan:cast(P, {store, "joe", "writing a book"}),
    V1 = lib_chan:rpc(P, {lookup, "joe"}), 
    V2 = lib_chan:rpc(P, {lookup, fred}),
    {V1, V2}.

