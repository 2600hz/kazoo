%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  13 jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([refresh/0, refresh/1]).
-export([blocking_refresh/0]).
-export([purge_doc_type/2]).

-include("../include/crossbar.hrl").

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).

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

refresh() ->
    spawn(fun do_refresh/0),
    started.

do_refresh() ->
    refresh(?WH_SIP_DB),
    refresh(?WH_SCHEMA_DB),
    refresh(?WH_ACCOUNTS_DB),
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2000),
                          refresh(AccountDb)
                  end, whapps_util:get_all_accounts()).

refresh(Account) when not is_binary(Account) ->
    refresh(wh_util:to_binary(Account));
refresh(?WH_SIP_DB) ->
    couch_mgr:db_create(?WH_SIP_DB),
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
    AccountDb = wh_util:format_account_id(Account, encoded),
    AccountId = wh_util:format_account_id(Account, raw),
    couch_mgr:revise_docs_from_folder(AccountDb, crossbar, "account"),
    couch_mgr:revise_doc_from_file(AccountDb, crossbar, ?MAINTENANCE_VIEW_FILE),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            ?LOG("account ~s is missing its local account definition!", [AccountId]);
        {ok, JObj} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj)
    end,
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
                ],
            ok;
        {error, _} ->
            ok
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
