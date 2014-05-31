%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_couch_maintenance).

-include("wh_couch.hrl").

-export([flush/0
         ,start_auto_compaction/0
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


-export([change_api_url/2]).

flush() ->
    wh_cache:flush_local(?WH_COUCH_CACHE).

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
    wh_couch_connections:test_conn().

test_admin_connection() ->
    wh_couch_connections:test_admin_conn().

%%
%% Change app's urls in bulk
%%
%% Usage: sup whistle_couch_maintenance change_api_url userportal https://newurl.tld:8443/v1
%%
change_api_url(AppName, ApiUrl) ->
    {'ok', DbsList} = couch_mgr:db_info(),
    lists:foreach(fun(DbName) -> maybe_change_url(AppName, ApiUrl, wh_util:format_account_id(DbName,'encoded')) end, DbsList).

maybe_change_url(AppName, ApiUrl, <<"account", _:41/binary>> = DbName) ->
    case couch_mgr:get_results(DbName, <<"users/crossbar_listing">>) of
    {'ok', JObj} ->
        lists:foreach(fun(UserObj) ->
                         check_user_urls(AppName, ApiUrl, DbName, wh_json:get_value(<<"id">>,UserObj))
                      end,
                       JObj);
    {'error', E} ->
            io:format("An error occurred: ~p\n", [E])
        end,
    timer:sleep(100);
maybe_change_url(_AppName, _ApiUrl, _DbName) ->
    ok.
  %  io:format("Skipping ~p\n", [DbName]).

check_user_urls(AppName, ApiUrl, DbName, UserId) ->
    {'ok', UserDoc} = couch_mgr:open_doc(DbName, UserId),
    AccountName = whapps_util:get_account_name(DbName),
    CurrApiUrl = wh_json:get_value([<<"apps">>, AppName,<<"api_url">>],UserDoc),
    case CurrApiUrl =/= 'undefined' andalso CurrApiUrl =/= ApiUrl of
        true ->
               {'ok', _} = couch_mgr:save_doc(DbName, wh_json:set_value([<<"apps">>, AppName,<<"api_url">>], ApiUrl, UserDoc)),
               io:format("Url to be changed ~p, Account: ~p, User: ~p\n", [CurrApiUrl, AccountName, UserId]);
        _ -> ok
    end.

