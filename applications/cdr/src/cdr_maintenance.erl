%%%-------------------------------------------------------------------
%%% @Copyright (c) 2010-2014, 2600Hz
%%% @doc
%%% Maintenance module for migrating CDRs to the new Transient
%%% Account database sharding structure.
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_maintenance).

%% API
-export([flush/0]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?CDR_CACHE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
