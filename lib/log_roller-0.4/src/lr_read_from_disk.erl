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
-module(lr_read_from_disk).
-author('jacob.vorreuter@gmail.com').

-export([start_continuation/2, terms/1]).
-compile(export_all).

-include_lib("kernel/include/file.hrl").
-include("log_roller.hrl").
		
%%====================================================================
%% API
%%====================================================================

%% @spec start_continuation(atom(), true | false) -> {ok, continuation()}
%% @doc fetch a continuation pointed to the log frame currently being written to
start_continuation(Name, UseCache) ->
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = lr_write_to_disk:current_location(Name),
	StartPos = snap_to_grid(Pos),
	ChunkSize = Pos-StartPos,
	Props = {cprops, Name, FileStub, ChunkSize, SizeLimit, MaxIndex, UseCache},
	CachePid = lr_write_to_disk:get_cache_pid(Name),
	State = {cstate, Index, StartPos, ?DEFAULT_TIMESTAMP, <<>>, CachePid, 0},
	{continuation, Props, State}.

%% @spec terms(Cont) -> {ok, Cont1, Terms}
%%		 Cont = continuation()
%% @doc fetch the terms for the continuation passed in
terms(Cont) ->
	{ok, Cont1, Terms} = read_chunk(Cont),
	Timestamp1 = (Cont1#continuation.state)#cstate.last_timestamp,
	Timestamp2 = (Cont#continuation.state)#cstate.last_timestamp,
	%% the continuation record stores the last timestamp it encountered
	%% while traveling backward through the log files. If the timestamp
	%% jumps foreward that means all log files have been traversed.
	case is_full_cycle(Timestamp1, Timestamp2) of
		true when length(Terms) > 0 ->
			exit({error, read_full_cycle});
		_ ->
			{ok, Cont2} = rewind_location(Cont1),
			{ok, Cont2, Terms}
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec read_chunk(Cont) -> {ok, Cont1, Terms}
%% @doc read a chunk either from cache or file
%%	def chunk - a block of binary data containing erlang tuples (log_entry records)
read_chunk(#continuation{properties=Props, state=State}=Cont) ->
    CacheFrame = get_cache_frame(Props#cprops.disk_logger_name, State#cstate.cache_pid, Props#cprops.use_cache, State#cstate.index, State#cstate.position),
    case CacheFrame of
        undefined -> read_chunk_from_file(Cont);
        _ -> read_chunk_from_cache(Cont, CacheFrame)
    end.
    
%% @spec get_cache_frame(LoggerName, UseCache, Index, Pos) -> cache_entry() | undefined
%% @doc fetch the cache frame for the {Index,Pos} key
get_cache_frame(_, _, false, _, _) -> undefined;
get_cache_frame(LoggerName, Cache, true, Index, Pos) ->
    IsCurrent = is_current_location(LoggerName, Index, Pos),
    if
		IsCurrent -> 
		    %% ignore cache for the frame being written to currently
			undefined;
		true ->
			case lr_cache:get(Cache, key({Cache, Index, Pos})) of
				undefined -> undefined; %% cache frame does not exist
				CacheEntry -> 
					binary_to_term(CacheEntry)
			end
	end.
	
%% Fetch the current location from the disk logger.
%% If {Index,Pos} is inside the frame currently being
%% written to then return true, otherwise, false
is_current_location(LoggerName, Index, Pos) ->
    {_, CurrIndex, CurrPos, _, _} = lr_write_to_disk:current_location(LoggerName),
    if
		Index == CurrIndex ->
			A = snap_to_grid(Pos),
			B = snap_to_grid(CurrPos),
			A == B;
		true ->
			false
	end.
	
read_chunk_from_file(#continuation{state=State}=Cont) ->
	{ok, Chunk} = read_file(Cont),
	BinChunk = list_to_binary(Chunk),
	BinRem = State#cstate.binary_remainder,
	Bin = <<BinChunk/binary, BinRem/binary>>,
	{ok, Terms, BinRem1, LTimestamp1} = parse_terms(Bin, <<>>, [], State#cstate.last_timestamp),
    Index = State#cstate.index,
    Pos = State#cstate.position,
    State1 = State#cstate{last_timestamp=LTimestamp1, binary_remainder=BinRem1},
    Cont1 = Cont#continuation{state=State1},
	if State#cstate.cache_pid =/= undefined ->
			lr_cache:put(State#cstate.cache_pid, key({State#cstate.cache_pid, Index, Pos}), term_to_binary({cache_entry, State1, Terms}));
		true -> ok
	end,
	{ok, Cont1, Terms}.
	
read_chunk_from_cache(#continuation{state=State}=Cont, CacheEntry) ->
    CacheState = CacheEntry#cache_entry.cstate,
    LTimestamp = CacheState#cstate.last_timestamp,
	BinRem = CacheState#cstate.binary_remainder,
	State1 = State#cstate{last_timestamp=LTimestamp, binary_remainder=BinRem},
	{ok, Cont#continuation{state=State1}, CacheEntry#cache_entry.terms}.
	
read_file(#continuation{properties=Props, state=State}) ->
    %io:format("read from file {~w, ~w}~n", [State#cstate.index, State#cstate.position]),
	FileName = lists:flatten(io_lib:format("~s.~w", [Props#cprops.file_stub, State#cstate.index])),
	{ok, IoDevice} = file_handle(State#cstate.cache_pid, FileName),
	case file:pread(IoDevice, State#cstate.position, Props#cprops.chunk_size) of
		{ok, Chunk} -> 
			{ok, Chunk};
		eof ->
			{ok, []};
		{error, Reason} -> 
			%io:format("failed reading ~p, ~p, ~p~n", [IoDevice, State#cstate.position, Props#cprops.chunk_size]),
			exit({error, Reason})
	end.
	
file_handle(_, FileName) -> open_file(FileName).

open_file(FileName) ->
	case file:open(FileName, [read]) of
		{ok, IoDevice} -> {ok, IoDevice};
		{error, Reason} -> exit({error, Reason})
	end.
	
rewind_location(#continuation{properties=Props, state=State}=Cont) ->
    FileStub = Props#cprops.file_stub,
    MaxIndex = Props#cprops.max_index,
    Index = State#cstate.index,
    Pos = State#cstate.position,
	if
		%% file handle was left at beginning of file
		Pos =:= 0 -> 
			%% move to previous index file
			{ok, FileSize, Index1} = rewind_file_index(FileStub, Index, undefined, MaxIndex),
			if
				Index1 =:= Index andalso FileSize =< Pos ->
					exit({error, read_full_cycle});
				true -> ok
			end,
			{Pos1,ChunkSize1} =
				if
					FileSize > ?MAX_CHUNK_SIZE ->
						P1 = snap_to_grid(FileSize),
						{P1, (FileSize - P1)};
					true ->
						{0, ?MAX_CHUNK_SIZE}
				end,
			Props1 = Props#cprops{chunk_size=ChunkSize1},
			State1 = State#cstate{index=Index1, position=Pos1},
			{ok, Cont#continuation{properties=Props1, state=State1}};
		Pos =< ?MAX_CHUNK_SIZE -> %% less than one chunk left
		    Props1 = Props#cprops{chunk_size=?MAX_CHUNK_SIZE},
			State1 = State#cstate{position=0},
			{ok, Cont#continuation{properties=Props1, state=State1}};
		true -> %% more than a chunk's worth left
			Pos1 = snap_to_grid(Pos - ?MAX_CHUNK_SIZE),
			Props1 = Props#cprops{chunk_size=?MAX_CHUNK_SIZE},
			State1 = State#cstate{position=Pos1},
			{ok, Cont#continuation{properties=Props1, state=State1}}
	end.	

%% if Index and StartingIndex match then we've cycled all the way around
rewind_file_index(_FileStub, Index, Index, _MaxIndex) -> exit({error, cannot_find_next_index});

rewind_file_index(FileStub, Index, StartingIndex, MaxIndex) ->
	Index1 =
		if
			Index =< 1 -> %% base index, cycle to top index
				MaxIndex;
			true ->
				Index - 1
		end,
	FileName = lists:flatten(io_lib:format("~s.~w", [FileStub, Index1])),
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			{ok, FileInfo#file_info.size, Index1};
		{error, enoent} ->
			case StartingIndex of
				undefined ->
					rewind_file_index(FileStub, Index1, Index, MaxIndex);
				_ ->
					rewind_file_index(FileStub, Index1, StartingIndex, MaxIndex)
			end;
		{error, Reason} ->
			exit({error, Reason})
	end.
			
%% @spec parse_terms(Bin, Rem, Acc, LTimestamp) -> Result
%%		 Bin = binary()
%%		 Rem = binary()
%%		 Acc = list()
%%		 LTimestamp = tuple()
%%		 Result = {ok, Terms, Rem}
parse_terms(<<16#FF:8, 16#FF:8, 16#FF:8, 16#FF:8, LogSize:16/integer, Rest/binary>> = Bin, Rem, Acc, LTimestamp) ->
	if 
		size(Rest) >= LogSize ->
			case Rest of
				<<Log:LogSize/binary, 16#EE:8, 16#EE:8, 16#EE:8, 16#EE:8, Tail/binary>> ->
					Term = binary_to_term(Log),
					parse_terms(Tail, Rem, [Term|Acc], Term#log_entry.time);
				_ ->
					error_logger:info_msg("bad binary data: ~n~p~n", [Bin]),
					exit({error, bad_binary_data_format})
			end;
		true ->
			{ok, Acc, Bin, LTimestamp}
	end;
	
parse_terms(<<>>, Rem, Acc, LTimestamp) ->
	{ok, Acc, Rem, LTimestamp};
	
parse_terms(<<A:8, Rest/binary>>, Rem, Acc, LTimestamp) ->
	parse_terms(Rest, <<Rem/binary, A>>, Acc, LTimestamp).
		
snap_to_grid(Position) ->
	((Position div ?MAX_CHUNK_SIZE) * ?MAX_CHUNK_SIZE).

is_full_cycle(?DEFAULT_TIMESTAMP, ?DEFAULT_TIMESTAMP) -> false;	
is_full_cycle({A1,B1,C1}, {A2,B2,C2}) ->
	if 
		A1 =:= A2, B1 =:= B2, C1 >= C2 -> true;
		A1 =:= A2, B1 > B2 -> true;
		A1 > A2 -> true;
		true -> false
	end.
	
key({Cache, Index, Pos}) when is_pid(Cache), is_integer(Index), is_integer(Pos) -> 
	lists:flatten(io_lib:format("~p_~w_~w", [Cache, Index, Pos]));
key(_) ->
	exit({error, unexpected_key}).