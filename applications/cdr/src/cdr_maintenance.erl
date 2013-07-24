%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2013, 2600Hz
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
-export([stop_v3_migrator/0]).
-export([start_v3_migrator/0]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?CDR_CACHE).

-spec stop_v3_migrator() -> any().
stop_v3_migrator() ->
    cdr_sup:stop_v3_migrate().

-spec start_v3_migrator() -> any().
start_v3_migrator() ->
    cdr_sup:start_v3_migrate().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
