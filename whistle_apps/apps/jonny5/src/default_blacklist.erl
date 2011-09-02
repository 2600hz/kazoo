%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% When no blacklist provider is present
%%% @end
%%% Created : 30 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(default_blacklist).

-export([start_link/0, is_blacklisted/2, stop/1]).

start_link() -> ignore.

stop(_) -> ok.

is_blacklisted(_Srv, _AccountID) -> false.
