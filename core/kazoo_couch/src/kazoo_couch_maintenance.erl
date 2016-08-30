%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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

-type not_compacting() :: {'error', 'compactor_down'}.

-spec start_auto_compaction() -> {'ok', 'already_started'} |
                                 {'queued', reference()} |
                                 not_compacting().
start_auto_compaction() ->
    kz_couch_compactor:start_auto_compaction().

-spec stop_auto_compaction() -> {'ok', 'updated' | 'already_stopped'} | not_compacting().
stop_auto_compaction() ->
    kz_couch_compactor:stop_auto_compaction().

-spec compaction_status() -> {'ok', 'ready' | 'not_running' | kz_proplist()}.
compaction_status() ->
    kz_couch_compactor:status().

-spec compact_all() -> {'queued', reference()} | not_compacting().
compact_all() ->
    kz_couch_compactor:compact().

-spec compact_node(ne_binary()) -> {'queued', reference()} | not_compacting().
compact_node(Node) ->
    kz_couch_compactor:compact_node(Node).

-spec compact_db(ne_binary()) -> {'queued', reference()} | not_compacting().
compact_db(Db) ->
    kz_couch_compactor:compact_db(Db).

-spec compact_db(ne_binary(), ne_binary()) -> {'queued', reference()} | not_compacting().
compact_db(Node, Db) ->
    kz_couch_compactor:compact_db(Node, Db).

-spec cancel_compaction_job() -> {'ok', 'job_cancelled'} | {'error', 'no_job_running'} | not_compacting().
cancel_compaction_job() ->
    kz_couch_compactor:cancel_current_job().

-spec cancel_compaction_jobs() -> {'ok', 'jobs_cancelled'} | not_compacting().
cancel_compaction_jobs() ->
    kz_couch_compactor:cancel_all_jobs().
