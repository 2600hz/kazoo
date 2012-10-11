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
-export([migrate_limits/0, migrate_limits/1]).
-export([migrate_media/0, migrate_media/1]).

-export([get_all_account_views/0]).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).
-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(RESELLER_VIEW_FILE, <<"views/reseller.json">>).
-define(FAXES_VIEW_FILE, <<"views/faxes.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(ACCOUNTS_AGG_NOTIFY_VIEW_FILE, <<"views/notify.json">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate/0 :: () -> 'no_return'.
migrate() ->
    %% Remove depreciated dbs
    io:format("removing depreciated databases...~n", []),
    couch_mgr:db_delete(<<"crossbar_schemas">>),
    couch_mgr:db_delete(<<"registrations">>),
    couch_mgr:db_delete(<<"crossbar%2Fsessions">>),

    %% Ensure the offnet db exists and has all the necessary views
    io:format("refreshing views used by Stepswitch...~n", []),
    _ = stepswitch_maintenance:refresh(),

    %% Create missing limits doc
    _ = migrate_limits(),

    %% Ensure the phone_numbers doc in the account db is up-to-date
    _ = whistle_number_manager_maintenance:reconcile_numbers(),

    %% Load available providers into system_config
    _ = whistle_number_manager_maintenance:reconcile_providers(),

    %% Ensure the views in each DB are update-to-date, depreciated view removed, sip_auth docs
    %% that need to be aggregated have been, and the account definition is aggregated
    _ = blocking_refresh(),

    %% Clear the config cache as to ensure we manipulate what is actually in the db...
    _ = whapps_config:flush(),

    %% Remove depreciated crossbar modules from the startup list and add new defaults
    _ = crossbar_maintenance:migrate(),

    %% Migrate recorded name doc ids from VM Boxes to the Owner ID's doc (if exists)
    _ = callflow_maintenance:migrate_recorded_name(),

    %% Remove depreciated whapps from the startup list and add new defaults
    io:format("updating default kazoo modules~n", []),
    WhappsUpdates = [fun(L) -> [<<"sysconf">> | lists:delete(<<"sysconf">>, L)] end
                    ,fun(L) -> [<<"acdc">> | lists:delete(<<"acdc">>, L)] end
                    ],
    StartWhapps = whapps_config:get(<<"whapps_controller">>, <<"whapps">>, []),
    _ = whapps_config:set_default(<<"whapps_controller">>
                                      ,<<"whapps">>
                                      ,lists:foldr(fun(F, L) -> F(L) end, StartWhapps, WhappsUpdates)),

    io:format("restarting updated modules~n", []),
    %% Ensure the new settings are applied and the new defaults are running
    io:format("restarting updated modules~n", []),
    _ = whapps_controller:restart_app(sysconf),
    _ = whapps_controller:restart_app(notify),
    _ = whapps_controller:restart_app(acdc),
    no_return.

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
    refresh(?WH_SERVICES_DB),
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(conference, <<"views/conference.json">>)
             |whapps_util:get_views_json(crossbar, "account")
             ++ whapps_util:get_views_json(callflow, "views")
            ],
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(AccountDb, Current) ->
                        io:format("refreshing database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
                        _ = refresh(AccountDb, Views),
                        Current + 1
                end, 1, Accounts).

refresh(Database) when not is_binary(Database) ->
    refresh(wh_util:to_binary(Database));
refresh(?WH_SERVICES_DB) ->
    whistle_services_maintenance:refresh();
refresh(?WH_SIP_DB) ->
    couch_mgr:db_create(?WH_SIP_DB),
    Views = [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json(registrar, <<"auth.json">>)
            ],
    whapps_util:update_views(?WH_SIP_DB, Views, true),

    _ = case couch_mgr:all_docs(?WH_SIP_DB, [include_docs]) of
            {ok, JObjs} ->
                [cleanup_aggregated_device(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
            _ ->
                ok
        end,
    wapi_switch:publish_reloadacl();
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
    _ = case couch_mgr:all_docs(?WH_ACCOUNTS_DB, [include_docs]) of
            {ok, JObjs} ->
                _ = [cleanup_aggregated_account(wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs];
            _ ->
                ok
        end,
    ok;
refresh(?WH_PROVISIONER_DB) ->
    couch_mgr:db_create(?WH_PROVISIONER_DB),
    _ = couch_mgr:revise_doc_from_file(?WH_PROVISIONER_DB, crossbar, "account/provisioner_templates.json"),
    ok;
refresh(?WH_FAXES) ->
    couch_mgr:db_create(?WH_FAXES),
    _ = couch_mgr:revise_doc_from_file(?WH_FAXES, whistle_apps, ?FAXES_VIEW_FILE),
    ok;
refresh(?NE_BINARY = Account) ->
    Views = get_all_account_views(),
    refresh(Account, Views).

refresh(Account, Views) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    AccountId = wh_util:format_account_id(Account, raw),

    %% Remove old views
    _ = couch_mgr:del_doc(AccountDb, <<"_design/limits">>),
    _ = couch_mgr:del_doc(AccountDb, <<"_design/sub_account_reps">>),

    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            _ = case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
                    {ok, Def} ->
                        io:format("    account ~s is missing its local account definition, but it was recovered from the accounts db~n", [AccountId]),
                        couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"_rev">>, Def));
                    {error, not_found} ->
                        io:format("    account ~s is missing its local account definition, and not in the accounts db~n", [AccountId])
                        %%                    couch_mgr:db_delete(AccountDb)
                end,
            remove;
        {ok, JObj} ->
            _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj),
            AccountRealm = crossbar_util:get_account_realm(AccountDb, AccountId),

            _ = case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, [include_docs]) of
                    {ok, Devices} ->
                        _ = [whapps_util:add_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
                             || Device <- Devices
                                    ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =/= AccountRealm
                                    orelse wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"ip">>], Device, undefined) =/= undefined
                            ],
                        _ = [whapps_util:rm_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
                             || Device <- Devices
                                    ,wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =:= AccountRealm
                                    orelse wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"ip">>], Device, undefined) =:= undefined
                            ];
                    {error, _} ->
                        ok
                end,
            io:format("    updating views in ~s~n", [AccountDb]),
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
    _ = case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
            true ->
                io:format("    removing aggregated account for missing db ~s~n", [AccountDb]),
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
-spec cleanup_aggregated_device/1 :: (wh_json:json_object()) -> 'ok'.
cleanup_aggregated_device(Device) ->
    Default = case wh_json:get_value(<<"pvt_account_id">>, Device) of
                  undefined -> undefined;
                  Else -> wh_util:format_account_id(Else, encoded)
              end,
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Device, Default),
    _ = case AccountDb =/= undefined andalso (couch_mgr:db_exists(AccountDb) =/= true) of
            true ->
                io:format("    removing aggregated device for missing db ~s~n", [AccountDb]),
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
-spec purge_doc_type/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_objects()} |
                                                        {'error', term()}.
purge_doc_type(Type, Account) when not is_binary(Type) ->
    purge_doc_type(wh_util:to_binary(Type), Account);
purge_doc_type(Type, Account) when not is_binary(Account) ->
    purge_doc_type(Type, wh_util:to_binary(Account));
purge_doc_type(Type, Account) ->
    Db = wh_util:format_account_id(Account, encoded),
    case couch_mgr:get_results(Db, <<"maintenance/listing_by_type">>, [{key, Type}, include_docs]) of
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
-spec migrate_limits/0 :: () -> 'ok'.
-spec migrate_limits/1 :: (atom() | string() | binary()) -> 'ok'.

migrate_limits() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(AccountDb, Current) ->
                        io:format("migrating limits doc in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
                        _ = migrate_limits(AccountDb),
                        Current + 1
                end, 1, Accounts),
    ok.

migrate_limits(Account) when not is_binary(Account) ->
    migrate_limits(wh_util:to_binary(Account));
migrate_limits(Account) ->
    TStamp = wh_util:current_tstamp(),

    TwowayTrunks = whapps_config:get(<<"jonny5">>, <<"default_twoway_trunks">>),
    InboundTrunks = whapps_config:get(<<"jonny5">>, <<"default_inbound_trunks">>),

    AccountDb = case couch_mgr:db_exists(Account) of
                    true -> Account;
                    false -> wh_util:format_account_id(Account, encoded)
                end,
    {TT, IT} = clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks),
    JObj = wh_json:from_list([KV || {_, V}= KV <- [{<<"_id">>, <<"limits">>}
                                                   ,{<<"twoway_trunks">>, TT}
                                                   ,{<<"inbound_trunks">>, IT}
                                                   ,{<<"pvt_account_db">>, AccountDb}
                                                   ,{<<"pvt_account_id">>, wh_util:format_account_id(Account, raw)}
                                                   ,{<<"pvt_type">>, <<"limits">>}
                                                   ,{<<"pvt_created">>, TStamp}
                                                   ,{<<"pvt_modified">>, TStamp}
                                                   ,{<<"pvt_vsn">>, 1}
                                                  ]
                                        ,V =/= undefined
                             ]),
    _ = couch_mgr:save_doc(AccountDb, JObj),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_trunkstore_docs/3 :: (ne_binary(), integer(), integer()) -> {integer(), integer()}.
-spec clean_trunkstore_docs/4 :: (ne_binary(), wh_json:json_objects(), integer(), integer())
                                 -> {integer(), integer()}.

clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks) ->
    ViewOptions = [include_docs
                   ,{reduce, false}
                  ],
    case couch_mgr:get_results(AccountDb, <<"trunkstore/crossbar_listing">>, ViewOptions) of
        {ok, JObjs} -> clean_trunkstore_docs(AccountDb, JObjs, TwowayTrunks, InboundTrunks);
        {error, _}=E -> E
    end.

clean_trunkstore_docs(_, [], Trunks, InboundTrunks) ->
    {Trunks, InboundTrunks};
clean_trunkstore_docs(AccountDb, [JObj|JObjs], Trunks, InboundTrunks) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    %% if there are no servers and it was created by jonny5 softdelete the doc
    _ = case wh_json:get_ne_value(<<"servers">>, Doc) =:= undefined
            andalso wh_json:get_ne_value(<<"pvt_created_by">>, Doc) =:= <<"jonny5">>
        of
            true -> couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, Doc));
            false -> ok
        end,
    NewTrunks = case wh_json:get_integer_value([<<"account">>, <<"trunks">>], Doc, 0) of
                    OldTrunks when OldTrunks > Trunks -> OldTrunks;
                    _ -> Trunks
           end,
    NewInboundTrunks = case wh_json:get_integer_value([<<"account">>, <<"inbound_trunks">>], Doc, 0) of
                           OldInboundTrunks when OldInboundTrunks > InboundTrunks -> OldInboundTrunks;
                           _ -> Trunks
                       end,
    clean_trunkstore_docs(AccountDb, JObjs, NewTrunks, NewInboundTrunks).

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
                        io:format("migrating media in database (~p/~p) '~s'", [Current, Total, AccountDb]),
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
        {ok, []} -> io:format("no public media files in db ~s~n", [AccountDb]);
        {ok, [_|_]=JObjs1}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs1],
            ok;
        {error, _}=E1 ->
            io:format("unable to fetch media files in db ~s: ~p~n", [AccountDb, E1])
    end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_private_media">>, []) of
        {ok, []} -> io:format("no private media files in db ~s~n", [AccountDb]);
        {ok, JObjs2}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs2],
            ok;
        {error, _}=E2 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E2])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_attachment/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
migrate_attachment(AccountDb, ViewJObj) ->
    Id = wh_json:get_value(<<"id">>, ViewJObj),
    _ = case couch_mgr:open_doc(AccountDb, Id) of
            {error, _}=E1 -> io:format("unable to open media for attachment migration ~s/~s: ~p~n", [AccountDb, Id, E1]);
            {ok, JObj1} ->
                case wh_json:get_ne_value(<<"_attachments">>, JObj1) of
                    undefined ->
                        io:format("media doc ~s/~s has no attachments, removing~n", [AccountDb, Id]),
                        couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, JObj1));
                Attachments ->
                        _ = [catch migrate_attachment(AccountDb, JObj1, Attachment, wh_json:get_value(Attachment, Attachments))
                             || Attachment <- wh_json:get_keys(Attachments)
                            ],
                        ok
                end
        end,
    %% we must reopen the doc since the _attachments has changed or we will effectively remove all attachments!
    case couch_mgr:open_doc(AccountDb, Id) of
        {error, _}=E2 -> io:format("unable to open media for depreciated field removal ~s/~s: ~p~n", [AccountDb, Id, E2]);
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
                    io:format("removed depreciated properties from ~s/~s~n", [AccountDb, Id]);
                {error, _}=E3 ->
                    io:format("removal of depreciated properties from ~s/~s failed: ~p~n", [AccountDb, Id, E3])
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
                                io:format("unable to fetch attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttch, E]),
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
                                 io:format("unable to put new attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttch, Result]),
                                 {error, length_mismatch};
                             true ->
                                 Filename = wh_util:to_list(<<"/tmp/media_", Id/binary, "_", OrigAttch/binary>>),
                                 case file:write_file(Filename, Content1) of
                                     ok -> ok;
                                     {error, _}=E2 ->
                                         io:format("unable to backup attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttch, E2]),
                                         E2
                                 end
                         end
                 end
                ,fun(ok) ->
                         case OrigAttch =/= NewAttch of
                             true ->
                                 case couch_mgr:delete_attachment(AccountDb, Id, OrigAttch) of
                                     {ok, _} ->
                                         io:format("updated attachment name ~s/~s/~s~n", [AccountDb, Id, NewAttch]),
                                         ok;
                                     {error, _}=E ->
                                         io:format("unable to remove original attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttch, E]),
                                         error
                                 end;
                             false ->
                                 io:format("updated content type for ~s/~s/~s~n", [AccountDb, Id, NewAttch]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_all_account_views() ->
    [whapps_util:get_view_json(whistle_apps, ?MAINTENANCE_VIEW_FILE)
     ,whapps_util:get_view_json(whistle_apps, ?RESELLER_VIEW_FILE)
     ,whapps_util:get_view_json(conference, <<"views/conference.json">>)
     |whapps_util:get_views_json(crossbar, "account")
     ++ whapps_util:get_views_json(callflow, "views")
    ]. 
