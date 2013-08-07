%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% Created :  3 Jul 2013 by Stephen Gibberd <stephen.gibberd@2600hz.com>
%%%-------------------------------------------------------------------
-module(stats).

%% API
-export([start/0]).

start() ->
    application:start(stats).

    
