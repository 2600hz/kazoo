%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---


% http://www-net.cs.umass.edu/cs653-1997/notes/ch3/ch3-1.html

%% http://www.ibrado.com/sock-faq/
%% http://docs.sun.com/db?q=sockets&p=/doc/802-5886/6i9k5sgso&a=view

%% socket FAQ
                                                       
%% Messages sent by datagram sockets can be broadcast to reach all 
%% of the hosts on an attached network. The network must support broadcast; 
%% the system provides no simulation of broadcast in software. 
%% Broadcast messages can place a high load on a network since they 
%% force every host on the network to service them. 
%% Broadcasting is usually used for either of two reasons: to find 
%% a resource on a local network without having its address, 
%% or functions like routing require that information be sent to all 
%% accessible neighbors.
%%
%%       To send a broadcast message, create an Internet datagram socket:
%%
%%              s = socket(AF_INET, SOCK_DGRAM, 0);
%%
%%       and bind a port number to the socket:
%%
%%              sin.sin_family = AF_INET;
%%              sin.sin_addr.s_addr = htonl(INADDR_ANY);
%%              sin.sin_port = htons(MYPORT);
%%              bind(s, (struct sockaddr *) &sin, sizeof sin);
%%
%%   The datagram can be broadcast on only one network by sending 
%% to the network's broadcast address. A datagram can also be broadcast 
%% on all attached networks by sending to the special address 
%% INADDR_BROADCAST, defined in <netinet/in.h>.


-module(broadcast).
-compile(export_all).

send(IoList) ->
    case inet:ifget("eth0", [broadaddr]) of
	{ok, [{broadaddr, Ip}]} ->
	    {ok, S} =  gen_udp:open(5010, [{broadcast, true}]),
	    gen_udp:send(S, Ip, 6000, IoList),
	    gen_udp:close(S);
	_ ->
	    io:format("Bad interface name, or\n"
		      "broadcasting not supported\n")
    end.

listen() ->
    {ok, _} = gen_udp:open(6000),
    loop().

loop() ->
    receive
	Any ->
	    io:format("received:~p~n", [Any]),
	    loop()
    end.


