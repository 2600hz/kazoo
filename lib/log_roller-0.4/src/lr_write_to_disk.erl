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
%%
-module(lr_write_to_disk).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

-export([current_location/1, get_cache_pid/1]).

-include("log_roller.hrl").

-record(state, {log, args, name, filters, total_writes, cache_pid}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc start the server
start_link(LogConfig) when is_record(LogConfig, log_config) ->
    gen_server:start_link({local, LogConfig#log_config.name}, ?MODULE, LogConfig, []).

%% @spec current_location(LoggerName) -> Result
%%       LoggerName = atom()
%%		 Result = {FileStub, Index, Pos, SizeLimit, MaxIndex}
%%		 FileStub = list()
%%		 Index = integer()
%%		 Pos = integer()
%%		 SizeLimit = integer()
%%		 MaxIndex = integer()
%% @doc return the current location details of the disk_log
%% FileStub is the base file name which Index is appended to
%% when creating the logs. ie: log_roller_data.1
%% Pos is the position in the current log file after the last
%% insert.  SizeLimit and MaxIndex are the config values that
%% dictate how large log files can become and how many files
%% to distribute the logs amongst.
current_location(Name) when is_atom(Name) ->
	gen_server:call(Name, current_location).
	
get_cache_pid(Name) when is_atom(Name) ->
	gen_server:call(Name, cache_pid).
	
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
init(LogConfig) when is_record(LogConfig, log_config) ->
	State = initialize_state(LogConfig),
	pg2:create(log_roller_server),
	pg2:join(log_roller_server, self()),
	{ok, State}.

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
handle_call(current_location, _From, #state{log=Log, args=Args}=State) ->
	Infos = disk_log:info(Log),
	FileStub = proplists:get_value(file, Args),
	Index = proplists:get_value(current_file, Infos),
	Pos = proplists:get_value(no_current_bytes, Infos),
	{SizeLimit, MaxIndex} = proplists:get_value(size, Args),
	{reply, {FileStub, Index, Pos, SizeLimit, MaxIndex}, State};
	
handle_call(cache_pid, _From, #state{cache_pid=CachePid}=State) ->
	{reply, CachePid, State};
	
handle_call(name, _From, #state{name=Name}=State) ->
	{reply, Name, State};

handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.
	
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
handle_info({log_roller, _Sender, LogEntry}, #state{log=Log, filters=Filters, total_writes=Writes}=State) ->
	State1 =
		case lr_filter:filter(LogEntry, Filters) of
			[] ->
				State;
			[_] ->
				BinLog = term_to_binary(LogEntry),
				LogSize = size(BinLog),
				Bin = <<?Bin_Term_Start/binary, LogSize:16, BinLog:LogSize/binary, ?Bin_Term_Stop/binary>>,
				disk_log:blog(Log, Bin),
				gen_server:abcast(lr_hooks, {log, Log, LogEntry}),
				State#state{total_writes=Writes+1}
		end,
	{noreply, State1};

handle_info({_,_,_,{wrap,_NumLostItems}}, State) ->
	% CurrentFile = proplists:get_value(current_file, disk_log:info(Log)),
	% lr_cache:set_page(CachePid, CurrentFile),
	{noreply, State};
		
handle_info(_Info, State) -> 
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, #state{log=Log}) -> 
	disk_log:close(Log).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
		
initialize_state(LogConfig) when is_record(LogConfig, log_config) ->
	LogFile = log_file(LogConfig#log_config.name, LogConfig#log_config.log_dir),
	Args = [
		{name, LogConfig#log_config.name},
		{file, LogFile},
		{type, wrap},
		{format, external},
		{head, none},
		{notify, true},
		{size, {LogConfig#log_config.maxbytes, LogConfig#log_config.maxfiles}}
	],
	{ok, Log, Args1} = open_log(Args),
	#state{
		log=Log, 
		args=Args1, 
		name=LogConfig#log_config.name,
		cache_pid=lr_cache:new(LogConfig#log_config.cache_size),
		filters=LogConfig#log_config.filters, 
		total_writes=0
	}.

log_file(Name, Dir) when is_atom(Name) ->
	case Dir of
		undefined -> atom_to_list(Name);
		_ -> 
			case file:list_dir(Dir) of
				{ok, _} -> 
					Dir ++ "/" ++ atom_to_list(Name);
				{error, enoent} ->
					case file:make_dir(Dir) of
						ok -> 
							Dir ++ "/" ++ atom_to_list(Name);
						_DirErr ->
							%io:format("failed to create directory ~p: ~p~n", [Dir, DirErr]),
							atom_to_list(Name)
					end
			end
	end.
	
open_log(Args) ->
	Res = disk_log:open(Args),
	io:format("opened log: ~p~n", [Res]),
	case Res of
		{ok, Log} ->
			io:format("info: ~p~n", [disk_log:info(Log)]),
			{ok, Log, Args};
		{repaired, Log, {recovered, _Rec}, {badbytes, _Bad}} ->
			{ok, Log, Args};
		{error,{file_error,_,eacces}} ->
			io:format("insufficient permission level to open ~s~n", [proplists:get_value(file)]),
			exit(eacces);
		{error,{size_mismatch,_,NewSize}} ->
			Args1 = proplists:delete(size, Args),
			{ok, Log1, Args2} = open_log(Args1),
			case disk_log:change_size(Log1, NewSize) of
				ok ->
					{ok, Log1, [{size, NewSize}|Args2]};
				Err ->
					io:format("init error: ~p~n", [Err]),
					exit(Err)
			end;
		Err ->
			io:format("init error: ~p~n", [Err]),
			exit(Err)
	end.