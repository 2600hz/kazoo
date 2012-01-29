
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([process_csv_file_with/2, process_csv_string_with/2]).
-export([process_csv_file_with/3, process_csv_string_with/3]).
-export([process_csv_binary_with/2, process_csv_binary_with/3]).

%% @doc parse a csv file and process each parsed row with the RowFunction
process_csv_file_with(IoDevice, RowFunction) ->
    process_csv_file_with(IoDevice, RowFunction, []).

%% @doc parse a csv string and process each parsed row with the RowFunction
process_csv_string_with(String, RowFunction) ->
    process_csv_string_with(String, RowFunction, []).

%% @doc parse a csv file and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_file_with(IoDevice, RowFunction, InitState) ->
    do_it(IoDevice, RowFunction, stream_from_file, InitState).

%% @doc parse a csv string and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_string_with(String, RowFunction, InitState) ->
    do_it(String, RowFunction, stream_from_string, InitState).

process_csv_binary_with(Bin, RowFunction) ->
    process_csv_binary_with(Bin, RowFunction, []).
process_csv_binary_with(Bin, RowFunction, InitState) ->
    do_it(Bin, RowFunction, stream_from_binary, InitState).

%
% Internal API
%

do_it(Stream, RowFunction, StreamFunctionName, InitState) ->
    % prepare the processes
    ProcessingPid = self(),
    ParsingPid = spawn(ecsv_parser, start_parsing, [ProcessingPid]),
    _ReadingPid = spawn(ecsv_reader, StreamFunctionName, [Stream, ParsingPid]),

    % let's go!
    loop(RowFunction, InitState).

loop(RowFunction, State) ->
    receive
        % ignore empty row
        {newline, [[]]} ->
            loop(RowFunction, State);
        % process a new row
        {newline, NewLine} ->
            NewState = RowFunction(NewLine, State),
            loop(RowFunction, NewState);
        % the parsing is done, time to stop
        {done} ->
            {ok, State}
    end.
