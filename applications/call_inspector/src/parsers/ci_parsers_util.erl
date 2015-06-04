%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Pierre Fenoll
%%%-------------------------------------------------------------------
-module(ci_parsers_util).

%% ci_parsers_util: utilities for parsers.

-export([timestamp/1]).
-export([open_file/1]).
-export([parse_interval/0]).
-export([make_name/1]).

-include_lib("whistle/include/wh_types.hrl").

%% API

-spec timestamp(ne_binary() | erlang:timestamp()) -> api_integer().
timestamp(<<YYYY:4/binary, "-", MM:2/binary, "-", DD:2/binary, "T"
            ,HH:2/binary, ":", MMM:2/binary, ":", SS:2/binary, "."
            ,Micro:6/binary, "+", _H:2/binary, ":", _M:2/binary, " ", _/binary>>) ->
    1.0e-6 * wh_util:to_integer(Micro) +
        calendar:datetime_to_gregorian_seconds(
          { {wh_util:to_integer(YYYY), wh_util:to_integer(MM), wh_util:to_integer(DD)}
          , {wh_util:to_integer(HH), wh_util:to_integer(MMM), wh_util:to_integer(SS)} });
timestamp({_,_,Micro} = TS) ->
    Datetime = calendar:now_to_universal_time(TS),
    1.0e-6 * Micro +
        calendar:datetime_to_gregorian_seconds(Datetime);
timestamp(_) -> 'undefined'.


-spec open_file(iodata()) -> file:io_device().
open_file(Filename) ->
    Options = ['read','append'     %% Read whole file then from its end
              ,'binary'            %% Return binaries instead of lists
              ,'raw','read_ahead'  %% Faster access to file
              ],
    case file:open(Filename, Options) of
        {'ok', IoDevice} -> IoDevice;
        {'error', _FileOpenError} ->
            lager:debug("parser cannot open '~p': ~p", [Filename,_FileOpenError])
    end.


-spec parse_interval() -> pos_integer().
parse_interval() ->
    2*1000.  %% Milliseconds


-spec make_name(ne_binary() | {'parser_args', ne_binary(), _}) -> atom().
make_name(Bin)
  when is_binary(Bin) ->
    binary_to_atom(Bin, 'utf8');
make_name({'parser_args', ListenIP, Port})
  when is_integer(Port) ->
    make_name(<< (wh_util:to_binary(ListenIP))/binary,
                 ":",
                 (wh_util:to_binary(Port))/binary
              >>);
make_name({'parser_args', Filename, _IP}) ->
    FName = filename:absname(Filename),
    make_name(wh_util:to_binary(FName)).

%% Internals

%% End of Module.
