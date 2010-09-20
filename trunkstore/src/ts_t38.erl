%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% T.38 processing
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------

-module(ts_t38).

-export([process_flags/1]).

-include("ts.hrl").

-spec(process_flags/1 :: (Flags :: tuple()) -> tuple()).

process_flags(#route_flags{fax=[]}=Flags) ->
    Flags.
