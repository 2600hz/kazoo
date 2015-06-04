%% Copyright (c) 2013, Matthias Endler <matthias.endler@pantech.at>
%% 
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hep_udp_listener).

-behaviour(gen_server).

%% API

-export([start_link/0]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

-record(state, {
	socket :: inet:socket(),
	decoder :: module()
}).

init(_Args) ->
	Port = application:get_env(hep_udp_listener, listen_port, 9060),
	Decoder = application:get_env(hep_udp_listener, hep_parser, hep_multi_decoder),
	{ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
	{ok, #state{
		socket = Socket,
		decoder = Decoder
	}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({udp, _, SrcIp, SrcPort, Packet}, #state{decoder = Decoder} = State) ->
	Result = Decoder:decode(Packet),
	error_logger:info_msg(
		" ***~n"
		" *** Source IP     : ~p~n"
		" *** Source Port   : ~p~n"
		" *** Decoder Result:~n"
		"--------------------~n"
		"~p~n"
		"--------------------~n", [SrcIp, SrcPort, Result]),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
	_ = gen_udp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.