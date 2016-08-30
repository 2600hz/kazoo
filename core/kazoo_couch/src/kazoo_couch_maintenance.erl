%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_couch_maintenance).

-include("kz_couch.hrl").

-export([start_auto_compaction/0
        ,stop_auto_compaction/0
        ,compaction_status/0
        ,cancel_compaction_job/0
        ,cancel_compaction_jobs/0

        ,compact_all/0
        ,compact_node/1
        ,compact_db/1
        ,compact_db/2

        ]).


start_auto_compaction() ->
    kz_couch_compactor:start_auto_compaction().

stop_auto_compaction() ->
    kz_couch_compactor:stop_auto_compaction().

compaction_status() ->
    kz_couch_compactor:status().

compact_all() ->
    kz_couch_compactor:compact().

compact_node(Node) ->
    kz_couch_compactor:compact_node(Node).

compact_db(Db) ->
    kz_couch_compactor:compact_db(Db).

compact_db(Node, Db) ->
    kz_couch_compactor:compact_db(Node, Db).

cancel_compaction_job() ->
    kz_couch_compactor:cancel_current_job().

cancel_compaction_jobs() ->
    kz_couch_compactor:cancel_all_jobs().
