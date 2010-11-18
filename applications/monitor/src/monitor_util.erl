%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Provides utility methods for monitoring
%%% @end
%%% Created : 17 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_util).

-export([to_binary/1]).

to_binary(Term) ->
    whistle_util:to_binary(Term).
