
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv_parser).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

%
% This module is the raw csv parser.
% It will expect receiving:
% - {char, Char} for each character in a csv file
% - {eof} when the file is over
%
% It will send to the ResultPid (given to the funtion start_parsing):
% - {newline, NewLine} for each parsed line
% - {done} when the parsing is done (usually because eof has been sent)
%
% This parser is based on the blog post written by Andy Till located
% here http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html.
%
% This parser supports well formed csv files which are
% - a set of lines ending with a \n
% - each line contains a set of fields separated with a comma (,)
% - each field value can be enclosed with double quote (") ONLY
% - each field value can be empty
%
% Please note:
% - This parser has no failsafe mechanism if the file is badly formed!
%   But the line a,,,,,\n is perfectly fine.
% - This parser doesn't allow a return (\n) in a field value!
%

-export([start_parsing/1]).

-define(EMPTY_STRING, []).

%% @doc start parsing a csv stream and send the result to ResultPid
start_parsing(ResultPid) ->
    ready(ResultPid).

% -----------------------------------------------------------------------------

% the ready state is the initial one and also the most common state
% through the parsing
ready(ResultPid) ->
    ready(ResultPid, [], []).
ready(ResultPid, ParsedCsv, CurrentValue) ->
    %% io:format("ready: ~s: ~p~n", [CurrentValue, CurrentValue]),
    receive
        {eof} ->
            NewTerm = lists:reverse(CurrentValue),
            %% io:format("newterm: ~s: ~p~n", [NewTerm, NewTerm]),
            NewLine = lists:reverse([NewTerm | ParsedCsv]),
            %% io:format("newline: ~p~n", [NewLine]),
            send_line(ResultPid, NewLine),
            %% io:format("eof~n", []),
            send_eof(ResultPid);
        {char, Char} when Char =:= $" orelse Char =:= $' ->
            %% io:format("new quote: ~s: ~p~n", [[Char], Char]),
            % pass an empty string to in_quotes as we do not want the
            % preceeding characters to be included, only those in quotes
            in_quotes(ResultPid, ParsedCsv, ?EMPTY_STRING, Char);
        {char, Char} when Char == $, ->
            NewTerm = lists:reverse(CurrentValue),
            %% io:format("new term: ~s: ~p~n", [NewTerm, NewTerm]),
            ready(ResultPid, [NewTerm | ParsedCsv], ?EMPTY_STRING);
        {char, Char} when Char == $\n ->
            % a new line has been parsed: time to send it back
            NewTerm = lists:reverse(CurrentValue),
            %% io:format("newterm: ~s: ~p~n", [NewTerm, NewTerm]),
            NewLine = lists:reverse([NewTerm | ParsedCsv]),
            %% io:format("newline: ~p~n", [NewLine]),
            ResultPid ! {newline, NewLine},
            %% io:format("newline~n", []),
            ready(ResultPid, [], ?EMPTY_STRING);
        {char, Char} when Char == $\r ->
            % ignore line feed characters
            ready(ResultPid, ParsedCsv, CurrentValue);
        {char, Char} ->
            %% io:format("new char: ~p~n", [Char]),
            ready(ResultPid, ParsedCsv, [Char | CurrentValue])
    end.

% the in_quotes state adds all chars it receives to the value string until
% it receives a char matching the initial quote in which case it moves to
% the skip_to_delimiter state.
in_quotes(ResultPid, ParsedCsv, CurrentValue, QuoteChar) ->
    %% io:format("in_quotes: ~s~n", [CurrentValue]),
    receive
        {eof} ->
            NewLine = lists:reverse([lists:reverse(CurrentValue) | ParsedCsv]),
            send_line(ResultPid, NewLine),
            send_eof(ResultPid);
        {char, Char} when Char == QuoteChar ->
            NewCsv = [lists:reverse(CurrentValue) | ParsedCsv],
            %% io:format("skip: ~p~n", [ParsedCsv]),
            skip_to_delimiter(ResultPid, NewCsv);
        {char, Char} ->
            in_quotes(ResultPid, ParsedCsv, [Char | CurrentValue], QuoteChar)
    end.

% the skip_to_delimiter awaits chars which will get thrown away, when a
% value delimiter is received the machine moves to the ready state again.
skip_to_delimiter(ResultPid, ParsedCsv) ->
    receive
        {eof} ->
            %% io:format("skip: eof~n", []),
            NewLine = lists:reverse(ParsedCsv),
            send_line(ResultPid, NewLine),
            send_eof(ResultPid);
        {char, Char} when Char =:= $, ->
            %% io:format("skip term: ~p: ~s~n", [Char, [Char]]),
            ready(ResultPid, ParsedCsv, ?EMPTY_STRING);
        {char, Char} when Char =:= $\n ->
            %% io:format("skip line~n", []),
            send_line(ResultPid, lists:reverse(ParsedCsv)),
            ready(ResultPid, ?EMPTY_STRING, ?EMPTY_STRING);
        _D ->
            %% io:format("skip unknown: ~p~n", [_D]),
            skip_to_delimiter(ResultPid, ParsedCsv)
    end.

% ----------------------------------------------------------------------------

send_line(ResultPid, NewLine) ->
    ResultPid ! {newline, NewLine}.

send_eof(ResultPid) ->
    ResultPid ! {done}.
