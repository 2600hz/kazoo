%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Pierre Fenoll
%%%-------------------------------------------------------------------
-module(ci_parsers_util).

%% ci_parsers_util: 

-export([timestamp/1]).

-include_lib("whistle/include/wh_types.hrl").

%% API

-spec timestamp(ne_binary()) -> api_binary().
timestamp(<<YYYY:4/binary, "-", MM:2/binary, "-", DD:2/binary, "T"
            ,HH:2/binary, ":", MMM:2/binary, ":", SS:2/binary, "."
            ,Micro:6/binary, "+", _H:2/binary, ":", _M:2/binary, " ", _/binary>>) ->
    1.0e-6 * wh_util:to_integer(Micro) +
        calendar:datetime_to_gregorian_seconds(
          { {wh_util:to_integer(YYYY), wh_util:to_integer(MM), wh_util:to_integer(DD)}
          , {wh_util:to_integer(HH), wh_util:to_integer(MMM), wh_util:to_integer(SS)} });
timestamp(_) -> 'undefined'.

%% Internals

%% End of Module.
