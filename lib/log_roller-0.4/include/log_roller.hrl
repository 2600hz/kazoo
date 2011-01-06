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

-record(log_entry, {time, type, node, message}).

%% {disk_logger, Name::atom(), Filters::[{atom(), any()}], LogDir::string(), MaxBytes::integer(), MaxFiles::integer()}
-record(log_config, {
	name = default,
	filters = [], 
	log_dir = undefined, 
	cache_size = 1048576,
	maxbytes = 10485760, 
	maxfiles = 10
}).

%-define(MAX_CHUNK_SIZE, 65536).
-define(MAX_CHUNK_SIZE, 8192).
-define(DEFAULT_TIMESTAMP, {9999,0,0}).
-define(Bin_Term_Start, <<16#FF, 16#FF, 16#FF, 16#FF>>).
-define(Bin_Term_Stop, <<16#EE, 16#EE, 16#EE, 16#EE>>).

-record(cprops, {disk_logger_name, file_stub, chunk_size, size_limit, max_index, use_cache}).
-record(cstate, {index, position, last_timestamp, binary_remainder, cache_pid, num_items}).
-record(continuation, {properties, state}).
-record(cache_entry, {cstate, terms}).

-define(GET_CPROP(Continuation, Field), (Continuation#continuation.properties)#cprops.Field).
-define(GET_CSTATE(Continuation, Field), (Continuation#continuation.state)#cstate.Field).
-define(SET_CPROP(Continuation, Field, Value), Continuation#continuation{properties=(Continuation#continuation.properties)#cprops{Field=Value}}).
-define(SET_CSTATE(Continuation, Field, Value), Continuation#continuation{state=(Continuation#continuation.state)#cstate{Field=Value}}).

