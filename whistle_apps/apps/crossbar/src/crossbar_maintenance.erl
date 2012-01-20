%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  13 jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([find_invalid_acccount_dbs/0]).
-export([refresh/0, refresh/1]).
-export([blocking_refresh/0]).
-export([purge_doc_type/2]).

-include("../include/crossbar.hrl").

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_invalid_acccount_dbs/0 :: () -> [] | [ne_binary(),...].
find_invalid_acccount_dbs() ->
    lists:foldr(fun(AccountDb, Acc) ->
                        AccountId = wh_util:format_account_id(AccountDb, raw),
                        case couch_mgr:open_doc(AccountDb, AccountId) of
                            {error, not_found} ->
                                [AccountDb|Acc];
                            {ok, _} ->
                                Acc
                        end
                end, [], whapps_util:get_all_accounts()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh/0 :: () -> 'ok'.
blocking_refresh() ->
    do_refresh().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'started'.
-spec refresh/1 :: (ne_binary() | nonempty_string()) -> 'ok'.
-spec refresh/2 :: (ne_binary(), json_objects()) -> 'ok'.

refresh() ->
    spawn(fun do_refresh/0),
    started.

do_refresh() ->
    refresh(?WH_SIP_DB),
    refresh(?WH_SCHEMA_DB),
    refresh(?WH_ACCOUNTS_DB),
    Views = [get_view_json(crossbar, ?MAINTENANCE_VIEW_FILE)
             ,get_view_json(conference, <<"views/conference.json">>)
             |get_views_json(crossbar, "account") ++ get_views_json(callflow, "views")
            ],
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(AccountDb, Current) ->
                        ?LOG("refreshing database (~p/~p) '~s'", [Current, Total, AccountDb]),
                        case refresh(AccountDb, Views) of
                            ok -> Current + 1;
                            remove -> Current + 1
                        end
                end, 1, Accounts).

refresh(Account) when not is_binary(Account) ->
    refresh(wh_util:to_binary(Account));
refresh(?WH_SIP_DB) ->
    couch_mgr:db_create(?WH_SIP_DB),
    couch_mgr:revise_doc_from_file(?WH_SIP_DB, registrar, <<"auth.json">> ),
    case couch_mgr:all_docs(?WH_SIP_DB, [{<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            [cleanup_aggregated_device(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
        _ ->
            ok
    end;
refresh(?WH_SCHEMA_DB) ->
    couch_mgr:db_create(?WH_SCHEMA_DB),
    couch_mgr:revise_docs_from_folder(?WH_SCHEMA_DB, crossbar, "schemas");
refresh(?WH_ACCOUNTS_DB) ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    case couch_mgr:all_docs(?WH_ACCOUNTS_DB, [{<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            [cleanup_aggregated_account(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
        _ ->
            ok
    end,
    couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?ACCOUNTS_AGG_VIEW_FILE),
    couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?MAINTENANCE_VIEW_FILE),
    ok;
refresh(Account) ->
    Views = [get_view_json(crossbar, ?MAINTENANCE_VIEW_FILE)
             |get_views_json(crossbar, "account")
            ],
    refresh(Account, Views).

refresh(Account, Views) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    AccountId = wh_util:format_account_id(Account, raw),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
                {ok, Def} ->
                    ?LOG("account is missing its local account definition, but it was recovered from the accounts db"),
                    couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"_rev">>, Def));
                {error, not_found} ->
                    ?LOG("account is missing its local account definition, and it doesnt exist in the accounts db. REMOVING!"),
                    couch_mgr:db_delete(AccountDb)
            end,
            remove;
        {ok, JObj} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj),
            AccountRealm = crossbar_util:get_account_realm(AccountDb, AccountId),
            case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, [{<<"include_docs">>, true}]) of
                {ok, Devices} ->
                    _ = [add_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
                         || Device <- Devices
                                ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =/= AccountRealm
                        ],
                    _ = [rm_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device)) 
                         || Device <- Devices
                                ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =:= AccountRealm
                        ];
                {error, _} ->
                    ok
            end,
            ViewOptions = [{<<"startkey">>, <<"_design/">>}
                           ,{<<"endkey">>, <<"_e">>}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(AccountDb, <<"_all_docs">>, ViewOptions) of
                {ok, Found} ->
                    update_views(Found, AccountDb, Views);
                {error, _} ->
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec cleanup_aggregated_account/1 :: (json_object()) -> ok.
cleanup_aggregated_account(Account) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Account),
    case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
        true ->
            ?LOG("removing aggregated account for missing db ~s", [AccountDb]),
            couch_mgr:del_doc(?WH_ACCOUNTS_DB, Account);
         false ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec cleanup_aggregated_device/1 :: (json_object()) -> ok.
cleanup_aggregated_device(Device) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Device),
    case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
        true ->
            ?LOG("removing aggregated device for missing db ~s", [AccountDb]),
            couch_mgr:del_doc(?WH_SIP_DB, Device);
         false ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec add_aggregate_device/2 :: (ne_binary(), undefined | ne_binary()) -> ok.
add_aggregate_device(_, undefined) ->
    ok;
add_aggregate_device(Db, Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),
    case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
        {ok, Rev} ->
            ?LOG("aggregating device ~s/~s", [Db, DeviceId]),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Device));
        {error, not_found} ->
            ?LOG("aggregating device ~s/~s", [Db, DeviceId]),
            couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Device))
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec rm_aggregate_device/2 :: (ne_binary(), undefined | ne_binary()) -> ok.
rm_aggregate_device(_, undefined) ->
    ok;
rm_aggregate_device(Db, Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),
    case couch_mgr:open_doc(?WH_SIP_DB, DeviceId) of
        {ok, JObj} ->
            ?LOG("removing aggregated device ~s/~s", [Db, DeviceId]),
            couch_mgr:del_doc(?WH_SIP_DB, JObj);
        {error, not_found} ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_views/3 :: (json_objects(), ne_binary(), proplist()) -> ok.
update_views([], _, []) ->
    ok;
update_views([], AccountDb, [{Id,View}|Views]) ->
    ?LOG("adding view '~s'", [Id]),
    couch_mgr:ensure_saved(AccountDb, View),
    update_views([], AccountDb, Views);
update_views([Found|Finds], AccountDb, Views) ->
    Id = wh_json:get_value(<<"id">>, Found),
    Doc = wh_json:get_value(<<"doc">>, Found),
    RawDoc = wh_json:delete_key(<<"_rev">>, Doc),
    case props:get_value(Id, Views) of
        undefined -> 
            ?LOG("removing view '~s'", [Id]),
            couch_mgr:del_doc(AccountDb, Doc),
            update_views(Finds, AccountDb, proplists:delete(Id, Views));
        View1 when View1 =:= RawDoc ->
            update_views(Finds, AccountDb, proplists:delete(Id, Views));
        View2 ->
            ?LOG("updating view '~s'", [Id]),
            Rev = wh_json:get_value(<<"_rev">>, Doc),
            couch_mgr:ensure_saved(AccountDb, wh_json:set_value(<<"_rev">>, Rev, View2)),
            update_views(Finds, AccountDb, proplists:delete(Id, Views))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec purge_doc_type/2 :: (ne_binary(), ne_binary()) -> ok | {error, term()}.
purge_doc_type(Type, Account) when not is_binary(Type) ->
    purge_doc_type(wh_util:to_binary(Type), Account);
purge_doc_type(Type, Account) when not is_binary(Account) ->
    purge_doc_type(Type, wh_util:to_binary(Account));
purge_doc_type(Type, Account) ->
    Db = wh_util:format_account_id(Account, encoded),
    case couch_mgr:get_results(Db, {<<"maintenance">>, <<"listing_by_type">>}, [{<<"key">>, Type}, {<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            couch_mgr:del_docs(Db, [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]);
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_views_json/2 :: (atom(), string()) -> json_objects().
get_views_json(App, Folder) ->
    Files = filelib:wildcard(lists:flatten([code:priv_dir(App), "/couchdb/", Folder, "/*.json"])),
    [JObj 
     || File <- Files, 
        begin 
            JObj = (catch(get_view_json(File))),
            case JObj of {'EXIT', _} -> false; _ -> true end 
        end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_view_json/2 :: (atom(), string()) -> {ne_binary(), json_object()}.
-spec get_view_json/1 :: (string()) -> {ne_binary(), json_object()}.

get_view_json(App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    get_view_json(Path).

get_view_json(Path) ->
    ?LOG_SYS("fetch view from ~s", [Path]),
    {ok, Bin} = file:read_file(Path),
    JObj = wh_json:decode(Bin),
    {wh_json:get_value(<<"_id">>, JObj), JObj}.



    
