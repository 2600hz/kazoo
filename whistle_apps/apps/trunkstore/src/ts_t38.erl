%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% T.38 processing
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------

-module(ts_t38).

-export([process_flags/1]).

-include("ts.hrl").

-spec(process_flags(Flags :: tuple()) -> tuple()).

process_flags(F) ->
    F.
