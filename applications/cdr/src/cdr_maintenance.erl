%%%-------------------------------------------------------------------
%%% @author Ben Wann <bwann@tickbook.local>
%%% @copyright (C) 2013, Ben Wann
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2013 by Ben Wann <bwann@tickbook.local>
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
    supervisor:terminate_child('cdr_sup', 'cdr_v3_migrate_server').

-spec start_v3_migrator() -> any().
start_v3_migrator() ->
    supervisor:start_child('cdr_sup', 'cdr_v3_migrate_server').


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
