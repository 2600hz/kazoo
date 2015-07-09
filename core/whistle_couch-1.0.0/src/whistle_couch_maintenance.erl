%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_couch_maintenance).

-include("wh_couch.hrl").

-export([flush/0
         ,flush/1
         ,flush/2
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

         ,archive/1
         ,archive/2
        ]).

-export([change_api_url/2, change_api_url/3]).

-spec flush() -> 'ok'.
-spec flush(ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary()) -> 'ok'.
flush() ->
    _ = couch_mgr:flush_cache_docs(),
    io:format("flushed all cached docs from Couch~n").

flush(Account) ->
    _ = couch_mgr:flush_cache_docs(wh_util:format_account_id(Account, 'encoded')),
    io:format("flushed all docs cached for account ~s~n", [Account]).

flush(Account, DocId) ->
    _ = couch_mgr:flush_cache_doc(wh_util:format_account_id(Account, 'encoded'), DocId),
    io:format("flushed cached doc ~s for account ~s~n", [DocId, Account]).

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

-spec archive(ne_binary()) -> 'ok'.
archive(Db) ->
    couch_util:archive(Db).

-spec archive(ne_binary(), ne_binary()) -> 'ok'.
archive(Db, Filename) ->
    couch_util:archive(Db, Filename).

%%
%% Change app's urls in bulk
%%
%% Usage: sup whistle_couch_maintenance change_api_url userportal https://newurl.tld:8443/v1
%%
-spec change_api_url(ne_binary(), ne_binary()) -> 'ignore'.
-spec change_api_url(ne_binary(), ne_binary(), ne_binary()) -> 'ignore'.
change_api_url(AppName, ApiUrl) ->
    _ = [begin
             change_api_url(AppName, ApiUrl, Account),
             timer:sleep(100)
         end
         || Account <- whapps_util:get_all_accounts()
        ],
    'ignore'.

change_api_url(AppName, ApiUrl, Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    _ = case couch_mgr:get_results(AccountDb, <<"users/crossbar_listing">>, ['include_docs']) of
            {'ok', JObjs} ->
                io:format("checking users in account ~s~n", [Account]),
                _ = [maybe_update_user_urls(AppName, ApiUrl, AccountDb, wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs],
                io:format("  users in account ~s successfully updated~n", [Account]);
            {'error', _E} ->
                io:format("an error occurred fetching users for ~s: ~p\n", [Account, _E])
        end,
    'ignore'.

-spec maybe_update_user_urls(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_update_user_urls(AppName, ApiUrl, AccountDb, UserJObj) ->
    case wh_json:get_value([<<"apps">>, AppName, <<"api_url">>], UserJObj) of
        'undefined' -> 'ok';
        ApiUrl -> 'ok';
        OldApiUrl ->
            io:format("  user ~s in account ~s has old api url ~s, updating...~n"
                      ,[wh_doc:id(UserJObj)
                        ,whapps_util:get_account_name(AccountDb)
                        ,OldApiUrl
                       ]
                     ),
            {'ok', _} =
                couch_mgr:ensure_saved(AccountDb
                                       ,wh_json:set_value([<<"apps">>, AppName, <<"api_url">>], ApiUrl, UserJObj)
                                      ),
            'ok'
    end.
