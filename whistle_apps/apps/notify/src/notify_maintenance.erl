%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_maintenance).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([refresh/0]).

-spec refresh/0 :: () -> ok.
refresh() ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json(notify, <<"views/notify.json">>)],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views),
    ok.
