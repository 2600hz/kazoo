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
        ]).
