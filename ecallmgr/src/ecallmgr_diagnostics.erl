%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Given a diagnostics record, create a diagnostics proplist to return
%%% @end
%%% Created : 25 Oct 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(ecallmgr_diagnostics).

-export([get_diagnostics/1]).

-include("ecallmgr.hrl").

-spec(get_diagnostics/1 :: (Stats :: tuple()) -> list(tuple(atom(), term()))).
get_diagnostics(Stats) when is_tuple(Stats) ->
    [{lookups_success, Stats#handler_stats.lookups_success}
     ,{lookups_failed, Stats#handler_stats.lookups_failed}
     ,{lookups_timeout, Stats#handler_stats.lookups_timeout}
     ,{lookups_requested, Stats#handler_stats.lookups_requested}
     ,{uptime, timer:now_diff(erlang:now(), Stats#handler_stats.started)}
     ].
