%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_secure).

-compile(export_all).
 
%% server:
%%   chat_socket:start_chat_server("joe", "123").
%%       "joe" is who I am
%%       
%% client:
%%  chat_socket:call("localhost","fred", "123").
%%      "fred" is who I am
%%      When we call we don't know the name of who we are calling
%%      so we'll have to wait until the server tells us


%% Secret = none | string
%%
%% Who = the person who owns the server


start_chat_server(Who, Secret) ->
    lib_auth_cs:start_server(3030, 
			     {?MODULE, chat_server, Who}, 
			     Secret, 
			     {yes, server}, 
			     50).

stop_echo_server() ->
    new_server:stop(3030).

%% The client is the initiator
%%    Example call("localhost", "joe", "foo123")

call(Host, From, Secret) ->
    lib_auth_cs:start_auth_client(Host, 
				  3030, 
				  {?MODULE, chat_client, {Host, From}}, 
				  Secret,
				  {yes, client}).



%%----------------------------------------------------------------------

chat_server(Client, Me) ->
    %% when we are started popup an I/O widget
    Widget = io_widget:start(self()),
    receive
	{msg, {hello, From}} ->
	    Client ! {msg, {hello, Me}},
	    io_widget:set_title(Widget, From),
	    relay(Client, Widget, Me)
    end.

relay(Pid, Widget, Who) ->
    receive
	{msg, {tell, Str}} ->
	    io_widget:insert(Widget, Str),
	    relay(Pid, Widget, Who);
	closed ->
	    void;
	{Widget, {data, Str}} ->
	    Str1 = Who ++ ":" ++ Str ++ "\n",
	    io_widget:insert(Widget, Str1),
	    Pid ! {msg, {tell, Str1}},
	    relay(Pid, Widget, Who);
	_Other ->
	    relay(Pid, Widget, Who)
    end.



chat_client(Server, {_Host, From}) ->
    Widget = io_widget:start(self()),
    Server ! {msg, {hello, From}},
    receive
	{msg, {hello, Me}} ->
	    io_widget:set_title(Widget, Me),
	    relay(Server, Widget, From)
    end.

    
