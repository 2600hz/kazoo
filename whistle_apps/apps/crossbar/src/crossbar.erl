%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  19 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar).

-include("../include/crossbar.hrl").

-export([start_link/0, stop/0]).

-export([refresh/0, refresh/1, init_first_account/0]).
-export([purge_doc_type/2]).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    start_deps(),
    crossbar_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> 'ok'.
stop() ->
    ok = application:stop(crossbar).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    wh_util:ensure_started(sasl), % logging
    wh_util:ensure_started(crypto), % random
    wh_util:ensure_started(inets),
    wh_util:ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    wh_util:ensure_started(webmachine),
    wh_util:ensure_started(whistle_amqp). % amqp wrapper

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verify that an application is running
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'started'.
-spec refresh/1 :: (ne_binary() | nonempty_string()) -> 'ok'.
refresh() ->
    spawn(fun do_refresh/0),
    started.

do_refresh() ->
    refresh(?SIP_AGG_DB),
    refresh(?SCHEMAS_DB),
    refresh(?ACCOUNTS_AGG_DB),
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2000),
                          refresh(AccountDb)
                  end, whapps_util:get_all_accounts()).

refresh(Account) when not is_binary(Account) ->
    refresh(wh_util:to_binary(Account));
refresh(?SIP_AGG_DB) ->
    couch_mgr:db_create(?SIP_AGG_DB);
refresh(?SCHEMAS_DB) ->
    couch_mgr:db_create(?SCHEMAS_DB),
    couch_mgr:revise_docs_from_folder(?SCHEMAS_DB, crossbar, "schemas");
refresh(?ACCOUNTS_AGG_DB) ->
    couch_mgr:db_create(?ACCOUNTS_AGG_DB),
    couch_mgr:revise_doc_from_file(?ACCOUNTS_AGG_DB, crossbar, ?ACCOUNTS_AGG_VIEW_FILE),
    couch_mgr:revise_doc_from_file(?ACCOUNTS_AGG_DB, crossbar, ?MAINTENANCE_VIEW_FILE),
    ok;
refresh(Account) ->
    AccountDb = whapps_util:get_db_name(Account, encoded),
    AccountId = whapps_util:get_db_name(Account, raw),
    couch_mgr:revise_docs_from_folder(AccountDb, crossbar, "account"),
    couch_mgr:revise_doc_from_file(AccountDb, crossbar, ?MAINTENANCE_VIEW_FILE),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            ?LOG("account ~s is missing its local account definition!", [AccountId]);
        {ok, JObj} ->
            couch_mgr:ensure_saved(?ACCOUNTS_AGG_DB, JObj)
    end,
    case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, [{<<"include_docs">>, true}]) of
        {ok, Devices} ->
            _ = [aggregate_device(wh_json:get_value(<<"doc">>, Device)) || Device <- Devices], ok;
        {error, _} ->
            ok
    end.

aggregate_device(undefined) ->
    ok;
aggregate_device(Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),
    case couch_mgr:lookup_doc_rev(?SIP_AGG_DB, DeviceId) of
        {ok, Rev} ->
            couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:set_value(<<"_rev">>, Rev, Device));
        {error, not_found} ->
            couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:delete_key(<<"_rev">>, Device))
    end,
    ok.

-spec init_first_account/0 :: () -> {'error', 'accounts_exist' | 'db_create_failed' | 'user_create_failed' | ne_binary()} |
                                    {ok, {ne_binary(), ne_binary()}}.
init_first_account() ->
    refresh(?ACCOUNTS_AGG_DB),
    case couch_mgr:all_docs(?ACCOUNTS_AGG_DB) of
        {ok, Docs} ->
            case [AcctDoc || AcctDoc <- Docs, erlang:binary_part(wh_json:get_value(<<"id">>, AcctDoc, <<>>), 0, 8) =/= <<"_design/">>] of
                [] -> init_first_account_for_reals();
                _ -> {error, accounts_exist}
            end;
        E -> E
    end.

-spec init_first_account_for_reals/0 :: () -> {'error', 'db_create_failed' | 'user_create_failed' | ne_binary()} |
                                              {'ok', {ne_binary(), ne_binary()}}.
init_first_account_for_reals() ->
    DbName = wh_util:to_binary(couch_mgr:get_uuid()),
    put(callid, DbName),

    Db = whapps_util:get_db_name(DbName, encoded),

    JObj = wh_json:from_list([{<<"name">>, <<"Master Account">>}
                              ,{<<"realm">>, list_to_binary([<<"admin.">>, net_adm:localhost()])}
                              ,{<<"pvt_superduper_admin">>, true} % first account is super duper account
                              ,{<<"_id">>, DbName}
                             ]),
    DbContext = cb_accounts:create_account(#cb_context{db_name=Db, req_data=JObj, req_verb = <<"put">>}),

    case couch_mgr:db_create(Db) of
        false ->
            {error, db_create_failed};
        true ->
            try
                _ = crossbar_bindings:map(<<"account.created">>, Db),
                _ = couch_mgr:revise_docs_from_folder(Db, crossbar, "account"),
                _ = couch_mgr:revise_doc_from_file(Db, crossbar, ?MAINTENANCE_VIEW_FILE),
                #cb_context{resp_status=success, doc=AcctDoc} = crossbar_doc:save(DbContext#cb_context{db_name = Db, req_verb = <<"put">>}),
                {ok, _} = couch_mgr:save_doc(?ACCOUNTS_AGG_DB, wh_json:delete_key(<<"_rev">>, AcctDoc)),
                create_init_user(Db)
            catch
                error:{badmatch, #cb_context{resp_status=Status,resp_error_msg=Msg,resp_error_code=Code,resp_data=JTerm}} ->
                    ?LOG("Failed to match successful context"),
                    ?LOG("Error: Status: ~s Msg: ~s Code: ~p Data: ~p", [Status, Msg, Code, JTerm]),
                    revert_init(Db, DbName),
                    {error, Msg};
                _T:_R ->
                    ?LOG("Failed to create user: ~p:~p", [_T, _R]),
                    ?LOG("Reverting changes"),
                    revert_init(Db, DbName),
                    {error, user_create_failed}
            end
    end.

-spec revert_init/2 :: (ne_binary(), ne_binary()) -> boolean().
revert_init(Db, DbName) ->
    _ = couch_mgr:del_doc(?ACCOUNTS_AGG_DB, DbName),
    couch_mgr:db_delete(Db).

-spec create_init_user/1 :: (ne_binary()) -> {'ok', {ne_binary(), ne_binary()}}.
create_init_user(Db) ->
    Username = <<"admin">>,

    Pass = wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(6))),

    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Pass),

    User = wh_json:from_list([{<<"username">>, Username}
                              ,{<<"verified">>, true}
                              ,{<<"apps">>, wh_json:from_list([{<<"voip">>, wh_json:from_list([{<<"label">>, <<"VoIP Services">>}
                                                                                               ,{<<"icon">>, <<"voip_services">>}
                                                                                               ,{<<"api_url">>, list_to_binary(["http://", net_adm:localhost(), ":8000/v1"])}
                                                                                               ,{<<"admin">>, true}
                                                                                              ])}])}
                              ,{<<"pvt_md5_auth">>, MD5}
                              ,{<<"pvt_sha1_auth">>, SHA1}
                             ]),

    #cb_context{resp_status=success, doc=UserDoc}=Context = cb_users:create_user(#cb_context{db_name=Db, req_data=User, req_verb = <<"put">>}),
    ?LOG("Saving ~p", [UserDoc]),

    #cb_context{resp_status=success} = crossbar_doc:save(Context),
    {ok, {Username, Pass}}.

purge_doc_type(Type, Account) when not is_binary(Type) ->
    purge_doc_type(wh_util:to_binary(Type), Account);
purge_doc_type(Type, Account) when not is_binary(Account) ->
    purge_doc_type(Type, wh_util:to_binary(Account));
purge_doc_type(Type, Account) ->
    Db = whapps_util:get_db_name(Account, encoded),
    case couch_mgr:get_results(Db, {<<"maintenance">>, <<"listing_by_type">>}, [{<<"key">>, Type}, {<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            couch_mgr:del_docs(Db, [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]);
        {error, _}=E ->
            E
    end.
