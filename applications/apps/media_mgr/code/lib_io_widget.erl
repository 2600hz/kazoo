%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lib_io_widget).

-export([start/1, test/0, 
	 set_handler/2, set_prompt/2, set_title/2, insert_str/2]).

start(Pid) ->
    gs:start(),
    spawn_link(fun() -> widget(Pid) end).

set_title(Pid, Str)   -> Pid ! {title, Str}.
set_handler(Pid, Fun) -> Pid ! {handler, Fun}.
set_prompt(Pid, Str)  -> Pid ! {prompt, Str}.
insert_str(Pid, Str)  -> Pid ! {insert, Str}.

widget(Pid) ->
    Size = [{width,500},{height,200}],
    Win = gs:window(gs:start(),
		    [{map,true},{configure,true},{title,"window"}|Size]),
    gs:frame(packer, Win,[{packer_x, [{stretch,1,500}]},
			  {packer_y, [{stretch,10,120,100},
				      {stretch,1,15,15}]}]),
    gs:create(editor,editor,packer, [{pack_x,1},{pack_y,1},{vscroll,right}]),
    gs:create(entry, entry, packer, [{pack_x,1},{pack_y,2},{keypress,true}]),
    gs:config(packer, Size),
    Prompt = " > ",
    gs:config(entry, {insert,{0,Prompt}}),
    loop(Win, Pid, Prompt, fun parse/1). 


loop(Win, Pid, Prompt, Parse) ->   
    receive
	{handler, Fun} ->
	    loop(Win, Pid, Prompt, Fun);
	{prompt, Str} ->
	    %% this clobbers the line being input ...
	    %% this could be fixed - hint
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Str}}),
	    loop(Win, Pid, Str, Parse);
	{title, Str} ->
	    gs:config(Win, [{title, Str}]),
	    loop(Win, Pid, Prompt, Parse);
	{insert, Str} ->
	    gs:config(editor, {insert,{'end',Str}}),
	    scroll_to_show_last_line(),
	    loop(Win, Pid, Prompt, Parse);
	{gs,_,destroy,_,_} ->
	    io:format("Destroyed~n",[]),
	    Pid ! {self(), closed};
	{gs, entry,keypress,_,['Return'|_]} ->
	    Text = gs:read(entry, text),
	    io:format("Read:~p~n",[Text]),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Prompt}}),
	    case (catch Parse(Text)) of
		{'EXIT', _Error} ->
		    self() ! {insert, "** bad input**\n** /h for help\n"};
		Term ->
		    Pid ! {self(), Term}
	    end,
	    loop(Win, Pid, Prompt, Parse);
	{gs,_,configure,[],[W,H,_,_]} ->
	    gs:config(packer, [{width,W},{height,H}]),
	    loop(Win, Pid, Prompt, Parse);
	{gs, entry,keypress,_,_} ->
	    loop(Win, Pid, Prompt, Parse);
	Any ->
	    io:format("Discarded:~p~n",[Any]),
	    loop(Win, Pid, Prompt, Parse)
    end.

scroll_to_show_last_line() ->
    Size       = gs:read(editor, size),
    Height     = gs:read(editor, height),
    CharHeight = gs:read(editor, char_height),
    TopRow     = Size - Height/CharHeight,
    if  TopRow > 0 -> gs:config(editor, {vscrollpos, TopRow});
	true       -> gs:config(editor, {vscrollpos, 0})
    end.

test() ->
    spawn(fun() -> test1() end).

test1() ->
    W = io_widget:start(self()),
    io_widget:set_title(W, "Test window"),
    loop(W).

loop(W) ->
    receive
	{W, {str, Str}} ->
	    Str1 = Str ++ "\n",
	    io_widget:insert_str(W, Str1),
	    loop(W)
    end.

parse(Str) ->
    {str, Str}.

    
    
    
		  
