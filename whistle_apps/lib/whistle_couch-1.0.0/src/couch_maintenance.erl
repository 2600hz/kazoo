%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(couch_maintenance).

-export([start_auto_compaction/0
         ,stop_auto_compaction/0
         ,compaction_status/0
         ,cancel_compaction_job/0
         ,cancel_compaction_jobs/0

         ,compact_all/0
         ,compact_node/1
         ,compact_db/1
         ,compact_db/2

         ,test_connection/0
         ,test_admin_connection/0
        ]).

start_auto_compaction() ->
    couch_compactor_fsm:start_auto_compaction().

stop_auto_compaction() ->
    couch_compactor_fsm:stop_auto_compaction().

compaction_status() ->
    couch_compactor_fsm:status().

compact_all() ->
    couch_compactor_fsm:compact().

compact_node(Node) ->
    couch_compactor_fsm:compact_node(Node).

compact_db(Db) ->
    couch_compactor_fsm:compact_db(Db).

compact_db(Node, Db) ->
    couch_compactor_fsm:compact_db(Node, Db).

cancel_compaction_job() ->
    couch_compactor_fsm:cancel_current_job().

cancel_compaction_jobs() ->
    couch_compactor_fsm:cancel_all_jobs().

test_connection() ->
    couch_mgr:test_conn().

test_admin_connection() ->
    couch_mgr:test_admin_conn().
