%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(notify_maintenance).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([refresh/0]).

-spec refresh() -> ok.
refresh() ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json(notify, <<"views/notify.json">>)],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views),
    ok.
