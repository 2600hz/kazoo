%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_maintenance).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-export([migrate/0]).
-export([find_invalid_acccount_dbs/0]).
-export([refresh/0, refresh/1]).
-export([blocking_refresh/0]).
-export([purge_doc_type/2]).
-export([cleanup_aggregated_account/1]).
-export([migrate_media/0, migrate_media/1]).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).
-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(FAXES_VIEW_FILE, <<"views/faxes.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(ACCOUNTS_AGG_NOTIFY_VIEW_FILE, <<"views/notify.json">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate/0 :: () -> ok.
migrate() ->
    couch_mgr:db_delete(<<"crossbar_schemas">>),
    couch_mgr:db_delete(<<"registrations">>),
    couch_mgr:db_delete(<<"crossbar%2Fsessions">>),
    stepswitch_maintenance:refresh(),
    blocking_refresh(),
%%    whistle_number_manager_maintenance:reconcile(all),
    whapps_config:flush(),
    XbarUpdates = [fun(L) -> lists:delete(<<"cb_cdr">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_signups">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_resources">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_provisioner_templates">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_ts_accounts">>, L) end
                   ,fun(L) -> [<<"cb_phone_numbers">> | lists:delete(<<"cb_phone_numbers">>, L)] end
                   ,fun(L) -> [<<"cb_templates">> | lists:delete(<<"cb_templates">>, L)] end
                   ,fun(L) -> [<<"cb_onboard">> | lists:delete(<<"cb_onboard">>, L)] end
                   ,fun(L) -> [<<"cb_connectivity">> | lists:delete(<<"cb_ts_accounts">>, L)] end
                   ,fun(L) -> [<<"cb_local_provisioner_templates">> | lists:delete(<<"cb_local_provisioner_templates">>, L)] end
                   ,fun(L) -> [<<"cb_global_provisioner_templates">> | lists:delete(<<"cb_global_provisioner_templates">>, L)] end
                   ,fun(L) -> [<<"cb_queues">> | lists:delete(<<"cb_queues">>, L)] end
                   ,fun(L) -> [<<"cb_schemas">> | lists:delete(<<"cb_schema">>, L)] end
                   ,fun(L) -> [<<"cb_configs">> | lists:delete(<<"cb_configs">>, L)] end
                  ],
    StartModules = whapps_config:get(<<"crossbar">>, <<"autoload_modules">>, []),
    _ = whapps_config:set_default(<<"crossbar">>
                                      ,<<"autoload_modules">>
                                      ,lists:foldr(fun(F, L) -> F(L) end, StartModules, XbarUpdates)),
    WhappsUpdates = [fun(L) -> [<<"sysconf">> | lists:delete(<<"sysconf">>, L)] end
                    ,fun(L) -> [<<"acdc">> | lists:delete(<<"acdc">>, L)] end
                    ],
    StartWhapps = whapps_config:get(<<"whapps_controller">>, <<"whapps">>, []),
    _ = whapps_config:set_default(<<"whapps_controller">>
                                      ,<<"whapps">>
                                      ,lists:foldr(fun(F, L) -> F(L) end, StartWhapps, WhappsUpdates)),
    _ = whapps_controller:restart_app(crossbar),
    _ = whapps_controller:restart_app(sysconf),
    _ = whapps_controller:restart_app(notify),
    _ = whapps_controller:restart_app(acdc),
    ok.

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
-spec blocking_refresh/0 :: () -> pos_integer().
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
-spec refresh/2 :: (ne_binary(), wh_json:json_objects()) -> 'ok'.

refresh() ->
    spawn(fun do_refresh/0),
    started.

-spec do_refresh/0 :: () -> pos_integer().
do_refresh() ->
    refresh(?WH_SIP_DB),
    refresh(?WH_SCHEMA_DB),
    refresh(?WH_ACCOUNTS_DB),
    refresh(?WH_PROVISIONER_DB),
    refresh(?WH_FAXES),
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(conference, <<"views/conference.json">>)
             |whapps_util:get_views_json(crossbar, "account")
             ++ whapps_util:get_views_json(callflow, "views")
            ],
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(AccountDb, Current) ->
                        lager:debug("refreshing database (~p/~p) '~s'", [Current, Total, AccountDb]),
                        _ = refresh(AccountDb, Views),
                        Current + 1
                end, 1, Accounts).

refresh(?WH_SIP_DB) ->
    couch_mgr:db_create(?WH_SIP_DB),
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(registrar, <<"auth.json">>)
            ],
    whapps_util:update_views(?WH_SIP_DB, Views, true),
    case couch_mgr:all_docs(?WH_SIP_DB, [{<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            [cleanup_aggregated_device(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
        _ ->
            ok
    end;
refresh(?WH_SCHEMA_DB) ->
    couch_mgr:db_create(?WH_SCHEMA_DB),
    couch_mgr:revise_docs_from_folder(?WH_SCHEMA_DB, crossbar, "schemas"),
    ok;
refresh(?WH_ACCOUNTS_DB) ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(whistle_apps, ?ACCOUNTS_AGG_VIEW_FILE)
             ,whapps_util:get_view_json(notify, ?ACCOUNTS_AGG_NOTIFY_VIEW_FILE)
            ],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views, true),
    _ = case couch_mgr:all_docs(?WH_ACCOUNTS_DB, [{<<"include_docs">>, true}]) of
            {ok, JObjs} ->
                _ = [cleanup_aggregated_account(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
            _ ->
                ok
        end,
    ok;
refresh(?WH_PROVISIONER_DB) ->
    couch_mgr:db_create(?WH_PROVISIONER_DB),
    couch_mgr:revise_doc_from_file(?WH_PROVISIONER_DB, crossbar, "account/provisioner_templates.json"),
    ok;
refresh(?WH_FAXES) ->
    couch_mgr:db_create(?WH_FAXES),
    couch_mgr:revise_doc_from_file(?WH_FAXES, whistle_apps, ?FAXES_VIEW_FILE),
    ok;
refresh(<<Account/binary>>) ->
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(conference, <<"views/conference.json">>)
             |whapps_util:get_views_json(crossbar, "account")
             ++ whapps_util:get_views_json(callflow, "views")
            ],
    refresh(Account, Views);
refresh(Account) ->
    refresh(wh_util:to_binary(Account)).

refresh(Account, Views) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    AccountId = wh_util:format_account_id(Account, raw),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
                {ok, Def} ->
                    lager:debug("account ~s is missing its local account definition, but it was recovered from the accounts db", [AccountId]),
                    couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"_rev">>, Def));
                {error, not_found} ->
                    lager:debug("account ~s is missing its local account definition, and not in the accounts db. REMOVING!", [AccountId])
                    %%                    couch_mgr:db_delete(AccountDb)
            end,
            remove;
        {ok, JObj} ->
            _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj),
            AccountRealm = crossbar_util:get_account_realm(AccountDb, AccountId),
            _ = case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, [{<<"include_docs">>, true}]) of
                    {ok, Devices} ->
                        _ = [whapps_util:add_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
                             || Device <- Devices
                                    ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =/= AccountRealm
                            ],
                        _ = [whapps_util:rm_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
                             || Device <- Devices
                                    ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =:= AccountRealm
                            ];
                    {error, _} ->
                        ok
                end,
            whapps_util:update_views(AccountDb, Views, true)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_aggregated_account/1 :: (wh_json:json_object()) -> ok.
cleanup_aggregated_account(Account) ->
    Default = case wh_json:get_value(<<"pvt_account_id">>, Account) of
                  undefined -> undefined;
                  Else -> wh_util:format_account_id(Else, encoded)
              end,
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Account, Default),
    case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
        true ->
            lager:debug("removing aggregated account for missing db ~s", [AccountDb]),
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
-spec cleanup_aggregated_device/1 :: (wh_json:json_object()) -> ok.
cleanup_aggregated_device(Device) ->
    Default = case wh_json:get_value(<<"pvt_account_id">>, Device) of
                  undefined -> undefined;
                  Else -> wh_util:format_account_id(Else, encoded)
              end,
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Device, Default),
    case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
        true ->
            lager:debug("removing aggregated device for missing db ~s", [AccountDb]),
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
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_media/0 :: () -> 'ok'.
-spec migrate_media/1 :: (atom() | string() | binary()) -> 'ok'.

migrate_media() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(AccountDb, Current) ->
                        lager:info("migrating media in database (~p/~p) '~s'", [Current, Total, AccountDb]),
                        _ = migrate_media(AccountDb),
                        couch_compactor:compact_db(AccountDb),
                        Current + 1
                end, 1, Accounts),
    ok.

migrate_media(Account) when not is_binary(Account) ->
    migrate_media(wh_util:to_binary(Account));
migrate_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    true -> Account;
                    false -> wh_util:format_account_id(Account, encoded)
                end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_by_name">>, []) of
        {ok, []} -> lager:info("no public  media files in db ~s", [AccountDb]);
        {ok, JObjs1}->
            [migrate_attachment(AccountDb, JObj) || JObj <- JObjs1],
            ok;
        {error, _}=E1 -> 
            lager:info("unable to fetch media files in db ~s: ~p", [AccountDb, E1])
    end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_private_media">>, []) of
        {ok, []} -> lager:info("no private media files in db ~s", [AccountDb]);
        {ok, JObjs2}->
            [migrate_attachment(AccountDb, JObj) || JObj <- JObjs2],
            ok;
        {error, _}=E2 -> 
            lager:info("unable to fetch private media files in db ~s: ~p", [AccountDb, E2])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_attachment/2 :: (ne_binary(), wh_json:json_objects()) -> 'ok'.
migrate_attachment(AccountDb, ViewJObj) ->
    Id = wh_json:get_value(<<"id">>, ViewJObj),
    case couch_mgr:open_doc(AccountDb, Id) of
        {error, _}=E1 -> lager:info("unable to open media for attachment migration ~s/~s: ~p", [AccountDb, Id, E1]);
        {ok, JObj1} ->
            case wh_json:get_ne_value(<<"_attachments">>, JObj1) of
                undefined ->
                    lager:debug("media doc ~s/~s has no attachments, removing", [AccountDb, Id]),
                    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, JObj1));
                Attachments ->
                    [catch migrate_attachment(AccountDb, JObj1, Attachment, wh_json:get_value(Attachment, Attachments)) 
                     || Attachment <- wh_json:get_keys(Attachments)
                    ],
                    ok
            end
    end,
    %% we must reopen the doc since the _attachments has changed or we will effectively remove all attachments!
    case couch_mgr:open_doc(AccountDb, Id) of
        {error, _}=E2 -> lager:info("unable to open media for depreciated field removal ~s/~s: ~p", [AccountDb, Id, E2]);
        {ok, JObj2} ->
            J = wh_json:delete_keys([<<"status">>, <<"content_size">>, <<"size">>, <<"content_type">>
                                         ,<<"content_length">>, <<"format">>, <<"sample">>, <<"media_type">>
                                    ], JObj2),
            Result = case J =/= JObj2 andalso wh_json:get_value(<<"source_id">>, J) of
                         false -> no_need;
                         undefined ->
                             couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"media_source">>, <<"upload">>, J));
                         _Else ->
                             couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"media_source">>, <<"recording">>, J))
                end,
            case Result of
                no_need -> ok;
                {ok, _} ->
                    lager:info("removed depreciated properties from ~s/~s", [AccountDb, Id]);
                {error, _}=E3 ->
                    lager:info("removal of depreciated properties from ~s/~s failed: ~p", [AccountDb, Id, E3])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_attachment/4 :: (ne_binary(), wh_json:json_object(), ne_binary(), wh_json:json_object()) -> 'ok'.
migrate_attachment(AccountDb, JObj, Attachment, MetaData) ->
    DocCT = wh_json:get_value(<<"content_type">>, JObj),
    MetaCT = wh_json:get_value(<<"content_type">>, MetaData),
    Migrations = [fun({A, _CT}) ->
                          case {is_audio_content(DocCT), is_audio_content(MetaCT)} of
                              {_, true} -> {A, MetaCT};
                              {true, _} -> {A, DocCT};
                              {_, _} ->
                                  Ext = wh_util:to_list(filename:extension(A)),
                                  case mochiweb_mime:from_extension(Ext) of
                                      undefined -> {A, <<"audio/mpeg">>};
                                      MIME -> {A, wh_util:to_binary(MIME)}
                                  end
                          end
                  end  
                  ,fun({A, CT}) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               false -> {A, CT};
                               true -> {add_extension(A, CT), CT}
                           end
                   end],          
    Migrate = lists:foldl(fun(F, Acc) -> F(Acc) end
                          ,{Attachment, MetaCT}
                          ,Migrations),
    Id = wh_json:get_value(<<"_id">>, JObj),
    maybe_update_attachment(AccountDb, Id, {Attachment, MetaCT}, Migrate).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type attachment_and_content() :: {ne_binary(), ne_binary()}.
-spec maybe_update_attachment/4 :: (ne_binary(), ne_binary(), attachment_and_content(), attachment_and_content()) -> 'ok'.
maybe_update_attachment(_, _, {Attachment, CT}, {Attachment, CT}) ->
    ok;
maybe_update_attachment(AccountDb, Id, {OrigAttch, _CT1}, {NewAttch, CT}) ->
    %% this preforms the following:
    %% 1. Get the current attachment
    %% 2. Fix the name and content type then put the new attachment on the doc
    %% 3. Save the old attachment content (I am paranoid) to disk
    %% 4. Remove the original (erronous) attachment
    %% However, if it failes at any of those stages it will leave the media doc with multiple
    %%    attachments and require manual intervention
    Updaters = [fun(_) -> 
                        case couch_mgr:fetch_attachment(AccountDb, Id, OrigAttch) of
                            {ok, _}=Ok -> Ok;
                            {error, _}=E ->
                                lager:info("unable to fetch attachment ~s/~s/~s: ~p", [AccountDb, Id, OrigAttch, E]),
                                E
                            end
                end
                ,fun({ok, Content1}) ->
                         {'ok', Rev} = couch_mgr:lookup_doc_rev(AccountDb, Id),
                         Options = [{headers, [{content_type, wh_util:to_list(CT)}]}
                                    ,{rev, Rev}
                                   ],
                         %% bigcouch is awesome in that it sometimes returns 409 (conflict) but does the work anyway..
                         %%   so rather than check the put return fetch the new attachment and compare it to the old 
                         Result = couch_mgr:put_attachment(AccountDb, Id, NewAttch, Content1, Options),
                         {ok, JObj} = couch_mgr:open_doc(AccountDb, Id),
                         case wh_json:get_value([<<"_attachments">>, OrigAttch, <<"length">>], JObj) =:= wh_json:get_value([<<"_attachments">>, NewAttch, <<"length">>], JObj) of
                             false -> 
                                 lager:info("unable to put new attachment ~s/~s/~s: ~p", [AccountDb, Id, NewAttch, Result]),
                                 {error, length_mismatch};
                             true -> 
                                 Filename = wh_util:to_list(<<"/tmp/media_", Id/binary, "_", OrigAttch/binary>>),
                                 case file:write_file(Filename, Content1) of
                                     ok -> ok;
                                     {error, _}=E2 ->
                                         lager:info("unable to backup attachment ~s/~s/~s: ~p", [AccountDb, Id, NewAttch, E2]),
                                         E2
                                 end
                         end
                 end
                ,fun(ok) ->
                         case OrigAttch =/= NewAttch of
                             true ->
                                 case couch_mgr:delete_attachment(AccountDb, Id, OrigAttch) of
                                     {ok, _} ->
                                         lager:info("updated attachment name ~s/~s/~s", [AccountDb, Id, NewAttch]),
                                         ok;
                                     {error, _}=E ->
                                         lager:info("unable to remove original attachment ~s/~s/~s: ~p", [AccountDb, Id, OrigAttch, E]),
                                         error
                                 end;
                             false ->
                                 lager:info("updated content type for ~s/~s/~s", [AccountDb, Id, NewAttch]),
                                 ok
                         end
                 end
               ],
    lists:foldl(fun(F, A) -> F(A) end, [], Updaters).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_extension/2 :: (ne_binary(), ne_binary()) -> ne_binary().
add_extension(A, <<"audio/x-wav">>) ->
    <<A/binary, ".wav">>;
add_extension(A, <<"audio/wav">>) ->
    <<A/binary, ".wav">>;
add_extension(A, <<"audio/mpeg3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/x-mpeg-3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/mpeg">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/x-mpeg">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/mp3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, _) -> A.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_audio_content/1 :: (ne_binary()) -> boolean().
is_audio_content(<<"audio/", _/binary>>) -> true;
is_audio_content(_) -> false.
