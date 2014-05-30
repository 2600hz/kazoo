%% @author root
%% @doc @todo Add description to doodle_maintenance.


-module(doodle_maintenance).

-include("doodle.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([refresh/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================


refresh() ->
    couch_mgr:db_create(?DOODLE_DB),
    _ = couch_mgr:revise_doc_from_file(?DOODLE_DB, 'doodle', <<"views/jobs.json">>),
    'ok'.
