%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP, INC
%%% @doc
%%% Given a diagnostics record, create a diagnostics proplist to return
%%% @end
%%% Created : 25 Oct 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_diagnostics).

-export([get_diagnostics/1]).

-include("ecallmgr.hrl").

-spec get_diagnostics/1 :: (Stats) -> [{'active_channels' |
					'created_channels' |
					'destroyed_channels' |
					'last_heartbeat' |
					'lookups_failed' |
					'lookups_requested' |
					'lookups_success' |
					'lookups_timeout' |
					'uptime'
					,integer()},...] when
      Stats :: #handler_stats{} | #node_stats{}.

get_diagnostics(#handler_stats{lookups_success=LS, lookups_failed=LF, lookups_timeout=LT, lookups_requested=LR, started=Started}) ->
    [{lookups_success, LS}
     ,{lookups_failed, LF}
     ,{lookups_timeout, LT}
     ,{lookups_requested, LR}
     ,{uptime, timer:now_diff(erlang:now(), Started)}
    ];
get_diagnostics(#node_stats{last_heartbeat=LH, created_channels=CC, destroyed_channels=DC, started=Started}) ->
    [{last_heartbeat, timer:now_diff(erlang:now(), LH)}
     ,{active_channels, CC - DC}
     ,{created_channels, CC}
     ,{destroyed_channels, DC}
     ,{uptime, timer:now_diff(erlang:now(), Started)}
    ].
