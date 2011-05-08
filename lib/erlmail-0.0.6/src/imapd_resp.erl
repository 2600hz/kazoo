%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP server response processing
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(imapd_resp).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").



-behaviour(gen_server).

%% External API
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([respond/3,send/2,insert/1,resp_list/1]).




start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE,stop).

respond(RespList,Tag,State) ->
	insert(RespList),
	send(Tag,State).


send(Tag,State) -> 
	RespList = resp_list(Tag),
	imapd_util:send(RespList,State).

insert(Resp) when is_record(Resp,imap_resp) -> 
	Fun = fun() ->
		mnesia:write(Resp#imap_resp{pid = self(), timestamp = now()})
	end,
	case mnesia:sync_transaction(Fun) of
		{atomic,ok} -> ok;
		{error,Reason} -> {error,Reason};
		{aborted,Reason} -> {error,Reason}
	end;
insert(List) when is_list(List) ->
	lists:map(fun(Resp) -> 
		insert(Resp)
		end, lists:flatten(List)).


resp_list(Tag) ->
	Fun = fun() ->
		Untagged =  mnesia:match_object(#imap_resp{tag='*',pid=self(), _ = '_'}),
		Tagged = mnesia:match_object(#imap_resp{tag=Tag,pid=self(), _ = '_'}),
		RespList = lists:append(Untagged,Tagged),
		lists:map(fun(Resp) -> 
			mnesia:delete_object(Resp)
			end, RespList),
		RespList
	end,
	case mnesia:sync_transaction(Fun) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason};
		{error,Reason} -> {error,Reason}
	end.








%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(stop,State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(test,State) ->
	io:format("Test Message Received~n"),
	{noreply,State};
handle_info(_Info,State) -> 
	?D({_Info,State}),
	{noreply,State}.


%%----------------------------------------------------------------------
%% @spec (Unused::term()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------

init(_) -> 
    process_flag(trap_exit, true),
	erlmail_util:check(imap_resp,imap_resp,bag,[mailbox,timestamp]),
	{ok, ok}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       'process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason,_State) -> erlmail_util:remove(imap_resp).