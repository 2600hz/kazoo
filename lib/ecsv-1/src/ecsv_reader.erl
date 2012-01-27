
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv_reader).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([stream_from_file/2, stream_from_string/2, stream_from_binary/2]).

%% @doc read a csv file and stream it to a csv parser
stream_from_file(IoDevice, ParsingPid) ->
    send_chars(ParsingPid, IoDevice).

%% @doc read a string and stream it to a csv parser
stream_from_string(String, ParsingPid) ->
    [ send_char(ParsingPid, C) || C <- String],
    ParsingPid ! {eof}.

stream_from_binary(Bin, ParsingPid) ->
    [ send_char(ParsingPid, C) || <<C>> <= Bin],
    ParsingPid ! {eof}.

%%
%% Local Functions
%%
send_chars(ParsingPid, IoDevice) ->
    send_chars(ParsingPid, IoDevice, io:get_chars(IoDevice, "", 1)).

send_chars(ParsingPid, IoDevice, eof) ->
    file:close(IoDevice),
    ParsingPid ! {eof};
send_chars(ParsingPid, IoDevice, Char) ->
    send_char(ParsingPid, Char),
    send_chars(ParsingPid, IoDevice, io:get_chars(IoDevice, "", 1)).

send_char(ParsingPid, C) ->
    ParsingPid ! {char, C}.
