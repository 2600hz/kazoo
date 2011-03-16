%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(io_widget).

-export([get_state/1,
	 start/1, test/0, 
	 set_handler/2, 
	 set_prompt/2,
	 set_state/2,
	 set_title/2, insert_str/2, update_state/3]).

start(Pid) ->
    gs:start(),
    spawn_link(fun() -> widget(Pid) end).

get_state(Pid)          -> rpc(Pid, get_state).
set_title(Pid, Str)     -> Pid ! {title, Str}.
set_handler(Pid, Fun)   -> Pid ! {handler, Fun}.
set_prompt(Pid, Str)    -> Pid ! {prompt, Str}.
set_state(Pid, State)   -> Pid ! {state, State}.
insert_str(Pid, Str)    -> Pid ! {insert, Str}.
update_state(Pid, N, X) -> Pid ! {updateState, N, X}. 

rpc(Pid, Q) ->    
    Pid ! {self(), Q},
    receive
	{Pid, R} ->
	    R
    end.

widget(Pid) ->
    Size = [{width,500},{height,200}],
    Win = gs:window(gs:start(),
		    [{map,true},{configure,true},{title,"window"}|Size]),
    gs:frame(packer, Win,[{packer_x, [{stretch,1,500}]},
			  {packer_y, [{stretch,10,100,120},
				      {stretch,1,15,15}]}]),
    gs:create(editor,editor,packer, [{pack_x,1},{pack_y,1},{vscroll,right}]),
    gs:create(entry, entry, packer, [{pack_x,1},{pack_y,2},{keypress,true}]),
    gs:config(packer, Size),
    Prompt = " > ",
    State = nil,
    gs:config(entry, {insert,{0,Prompt}}),
    loop(Win, Pid, Prompt, State, fun parse/1). 

loop(Win, Pid, Prompt, State, Parse) ->   
    receive
	{From, get_state} ->
	    From ! {self(), State},
	    loop(Win, Pid, Prompt, State, Parse);
	{handler, Fun} ->
	    loop(Win, Pid, Prompt, State, Fun);
	{prompt, Str} ->
	    %% this clobbers the line being input ...
	    %% this could be fixed - hint
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Str}}),
	    loop(Win, Pid, Str, State, Parse);
	{state, S} ->
	    loop(Win, Pid, Prompt, S, Parse);
	{title, Str} ->
	    gs:config(Win, [{title, Str}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{insert, Str} ->
	    gs:config(editor, {insert,{'end',Str}}),
	    scroll_to_show_last_line(),
	    loop(Win, Pid, Prompt, State, Parse);
	{updateState, N, X} ->
	    io:format("setelemtn N=~p X=~p State=~p~n",[N,X,State]),
	    State1 = setelement(N, State, X),
	    loop(Win, Pid, Prompt, State1, Parse);
	{gs,_,destroy,_,_} ->
	    io:format("Destroyed~n",[]),
	    exit(windowDestroyed);
	{gs, entry,keypress,_,['Return'|_]} ->
	    Text = gs:read(entry, text),
	    %% io:format("Read:~p~n",[Text]),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Prompt}}),
	    try Parse(Text) of
		Term ->
		    Pid ! {self(), State, Term}
	    catch
		_:_ ->
		    self() ! {insert, "** bad input**\n** /h for help\n"}
	    end,
	    loop(Win, Pid, Prompt, State, Parse);
	{gs,_,configure,[],[W,H,_,_]} ->
	    gs:config(packer, [{width,W},{height,H}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{gs, entry,keypress,_,_} ->
	    loop(Win, Pid, Prompt, State, Parse);
	Any ->
	    io:format("Discarded:~p~n",[Any]),
	    loop(Win, Pid, Prompt, State, Parse)
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

    
    
    
		  
