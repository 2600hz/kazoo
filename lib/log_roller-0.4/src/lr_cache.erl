%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(lr_cache).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

%% API exports
-export([new/1, get/2, put/3, delete/2, size/1, items/1]).

new(CacheSize) ->
	{ok, Pid} = gen_server:start_link(?MODULE, CacheSize, []),
	Pid.
	
get(CachePid, Key) when is_pid(CachePid), is_list(Key) ->
	gen_server:call(CachePid, {get, Key}).
		
put(CachePid, Key, Val) when is_pid(CachePid), is_list(Key), is_binary(Val) ->
	gen_server:call(CachePid, {put, Key, Val});
	
put(_, _, _) ->
	exit({error, cache_value_must_be_binary}).

delete(CachePid, Key) when is_pid(CachePid), is_list(Key) ->
	gen_server:call(CachePid, {delete, Key}).
	
size(CachePid) when is_pid(CachePid) ->
	gen_server:call(CachePid, size).
	
items(CachePid) when is_pid(CachePid) ->
	gen_server:call(CachePid, items).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init(_CacheSize) ->
	TableID = ets:new(lr_cache, [ordered_set, private]),
	{ok, TableID}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------	
handle_call({get, Key}, _From, TableID) ->
	{reply, get_internal(TableID, Key), TableID};

handle_call({put, Key, Val}, _From, TableID) ->
	{reply, put_internal(TableID, Key, Val), TableID};

handle_call({delete, Key}, _From, TableID) ->
	{reply, delete_internal(TableID, Key), TableID};
	
handle_call(size, _From, TableID) ->
	{reply, size_internal(TableID), TableID};

handle_call(items, _From, TableID) ->
	{reply, all_items_internal(TableID), TableID}.
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Message, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------	
handle_info(_Info, State) -> error_logger:info_msg("info: ~p~n", [_Info]), {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------	
get_internal(TableID, Key) ->
	case ets:lookup(TableID, Key) of
		[] -> undefined;
		[{Key,Val}] -> Val
	end.
		
put_internal(TableID, Key, Val) ->
	ets:insert(TableID, {Key, Val}),
	ok.

delete_internal(TableID, Key) ->
	ets:delete(TableID, Key),
	ok.

size_internal(TableID) ->
	ets:info(TableID, size).
	
all_items_internal(TableID) ->
	ets:tab2list(TableID).