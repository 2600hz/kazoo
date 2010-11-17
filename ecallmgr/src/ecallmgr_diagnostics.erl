%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Given a diagnostics record, create a diagnostics proplist to return
%%% @end
%%% Created : 25 Oct 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_diagnostics).

-export([get_diagnostics/1]).

-include("ecallmgr.hrl").
-include("whistle_api.hrl").

-spec(get_diagnostics/1 :: (Stats :: tuple()) -> proplist()).
get_diagnostics(#handler_stats{}=Stats) ->
    [{lookups_success, Stats#handler_stats.lookups_success}
     ,{lookups_failed, Stats#handler_stats.lookups_failed}
     ,{lookups_timeout, Stats#handler_stats.lookups_timeout}
     ,{lookups_requested, Stats#handler_stats.lookups_requested}
     ,{uptime, timer:now_diff(erlang:now(), Stats#handler_stats.started)}
    ];
get_diagnostics(#node_stats{}=Stats) ->
    [{uptime, timer:now_diff(erlang:now(), Stats#node_stats.started)}
     ,{last_heartbeat, timer:now_diff(erlang:now(), Stats#node_stats.last_heartbeat)}
     ,{active_channels, Stats#node_stats.created_channels - Stats#node_stats.destroyed_channels}
     ,{created_channels, Stats#node_stats.created_channels}
     ,{destroyed_channels, Stats#node_stats.destroyed_channels}
    ].
