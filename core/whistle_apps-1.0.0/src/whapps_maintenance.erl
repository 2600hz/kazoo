%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_maintenance).

-include("whistle_apps.hrl").

-export([rebuild_token_auth/0
        ,rebuild_token_auth/1
        ]).
-export([migrate/0
         ,migrate/1
        ]).
-export([find_invalid_acccount_dbs/0]).
-export([refresh/0, refresh/1
         ,refresh_account_db/1
        ]).
-export([blocking_refresh/0
         ,blocking_refresh/1
        ]).
-export([remove_depreciated_databases/0]).
-export([ensure_aggregate_devices/0
        ,ensure_aggregate_device/1
        ]).
-export([cleanup_aggregated_devices/0
         ,cleanup_aggregated_device/1
        ]).
-export([cleanup_aggregated_accounts/0
        ,cleanup_aggregated_account/1
        ,remove_aggregated_account/1
        ]).
-export([migrate_limits/0, migrate_limits/1]).
-export([migrate_media/0, migrate_media/1]).
-export([purge_doc_type/2, purge_doc_type/3]).
-export([call_id_status/1, call_id_status/2]).
-export([get_all_account_views/0]).
-export([cleanup_voicemail_media/1]).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).
-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(RESELLER_VIEW_FILE, <<"views/reseller.json">>).
-define(FAXES_VIEW_FILE, <<"views/faxes.json">>).
-define(FAXBOX_VIEW_FILE, <<"views/faxbox.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(ACCOUNTS_AGG_NOTIFY_VIEW_FILE, <<"views/notify.json">>).
-define(SEARCH_VIEW_FILE, <<"views/search.json">>).

-define(VMBOX_VIEW, <<"vmboxes/crossbar_listing">>).
-define(PMEDIA_VIEW, <<"media/listing_private_media">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rebuild_token_auth() -> 'ok'.
rebuild_token_auth() ->
    rebuild_token_auth(5000).

-spec rebuild_token_auth(text() | integer()) -> 'ok'.
rebuild_token_auth(Pause) ->
    _ = couch_mgr:db_delete(?KZ_TOKEN_DB),
    timer:sleep(wh_util:to_integer(Pause)),
    refresh(?KZ_TOKEN_DB),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(2000).

-spec migrate(text() | integer()) -> 'no_return'.
migrate(Pause) ->
    {'ok', Databases} = couch_mgr:db_info(),
    Accounts = [wh_util:format_account_id(Db, 'encoded')
                || Db <- Databases,
                   whapps_util:is_account_db(Db)
               ],
    io:format("updating system dbs...~n"),
    _ = refresh(?KZ_SYSTEM_DBS, Pause),

    %% Ensure the views in each DB are update-to-date, depreciated view removed, sip_auth docs
    %% that need to be aggregated have been, and the account definition is aggregated
    io:format("updating views...~n"),
    _ = refresh([Db || Db <- Databases, (not lists:member(Db, ?KZ_SYSTEM_DBS))], Pause),

    %% Remove depreciated dbs
    io:format("removing depreciated databases...~n"),
    _  = remove_depreciated_databases(Databases),

    %% Remove depreciated crossbar modules from the startup list and add new defaults
    io:format("running crossbar migrations...~n"),
    _ = crossbar_maintenance:migrate(Accounts),

    %% Migrate Faxes with private_media to fax
    io:format("running fax migrations...~n"),
    _ = fax_maintenance:migrate(Accounts),

    %% Migrate settings for whistle_media
    io:format("running media migrations...~n"),
    _ = whistle_media_maintenance:migrate(),

    'no_return'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh() -> 'no_return'.
blocking_refresh() -> refresh().

-spec blocking_refresh(text() | non_neg_integer()) -> 'no_return'.
blocking_refresh(Pause) ->
    {'ok', Databases} = couch_mgr:db_info(),
    refresh(Databases, Pause).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'no_return'.
-spec refresh(ne_binary() | nonempty_string()) -> 'ok' | 'remove'.
-spec refresh(ne_binaries(), text() | non_neg_integer()) -> 'no_return'.
-spec refresh(ne_binaries(), non_neg_integer(), non_neg_integer()) -> 'no_return'.
refresh() ->
    {'ok', Databases} = couch_mgr:db_info(),
    refresh(Databases, 2000).

refresh(Databases, Pause) ->
    Total = length(Databases),
    refresh(Databases, wh_util:to_integer(Pause), Total).

refresh([], _, _) -> 'no_return';
refresh([Database|Databases], Pause, Total) ->
    io:format("refreshing database (~p/~p) '~s'~n"
             ,[length(Databases) + 1, Total, Database]),
    _ = refresh(Database),
    _ = case Pause < 1 of
            'false' -> timer:sleep(Pause);
            'true' -> 'ok'
        end,
    refresh(Databases, Pause, Total).

refresh(?WH_OFFNET_DB) ->
    couch_mgr:db_create(?WH_OFFNET_DB),
    stepswitch_maintenance:refresh();
refresh(?WH_SERVICES_DB) ->
    couch_mgr:db_create(?WH_SERVICES_DB),
    whistle_services_maintenance:refresh();
refresh(?WH_SIP_DB) ->
    couch_mgr:db_create(?WH_SIP_DB),
    Views = [whapps_util:get_view_json('whistle_apps', ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json('registrar', <<"auth.json">>)
             ,whapps_util:get_view_json('crossbar', <<"views/resources.json">>)
            ],
    whapps_util:update_views(?WH_SIP_DB, Views, 'true');
refresh(?WH_SCHEMA_DB) ->
    couch_mgr:db_create(?WH_SCHEMA_DB),
    couch_mgr:revise_docs_from_folder(?WH_SCHEMA_DB, 'crossbar', "schemas"),
    'ok';
refresh(?WH_MEDIA_DB) ->
    couch_mgr:db_create(?WH_MEDIA_DB),
    whistle_media_maintenance:refresh(),
    'ok';
refresh(?WH_RATES_DB) ->
    couch_mgr:db_create(?WH_RATES_DB),
    couch_mgr:revise_docs_from_folder(?WH_RATES_DB, 'hotornot', "views"),
    _ = couch_mgr:revise_doc_from_file(?WH_RATES_DB, 'crossbar', <<"views/rates.json">>),
    couch_mgr:load_fixtures_from_folder(?WH_RATES_DB, 'hotornot'),
    'ok';
refresh(?WH_ANONYMOUS_CDR_DB) ->
    couch_mgr:db_create(?WH_ANONYMOUS_CDR_DB),
    _ = couch_mgr:revise_doc_from_file(?WH_ANONYMOUS_CDR_DB, 'cdr', <<"cdr.json">>),
    'ok';
refresh(?WH_DEDICATED_IP_DB) ->
    couch_mgr:db_create(?WH_DEDICATED_IP_DB),
    kz_ip_utils:refresh_database();
refresh(?WH_ACCOUNTS_DB) ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json('whistle_apps', ?MAINTENANCE_VIEW_FILE)
             ,whapps_util:get_view_json('whistle_apps', ?ACCOUNTS_AGG_VIEW_FILE)
             ,whapps_util:get_view_json('whistle_apps', ?SEARCH_VIEW_FILE)
             ,whapps_util:get_view_json('notify', ?ACCOUNTS_AGG_NOTIFY_VIEW_FILE)
            ],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views, 'true'),
    'ok';
refresh(?WH_FAXES) ->
    couch_mgr:db_create(?WH_FAXES),
    _ = couch_mgr:revise_doc_from_file(?WH_FAXES, 'fax', ?FAXES_VIEW_FILE),
    _ = couch_mgr:revise_doc_from_file(?WH_FAXES, 'fax', ?FAXBOX_VIEW_FILE),
    'ok';
refresh(?KZ_PORT_REQUESTS_DB) ->
    couch_mgr:db_create(?KZ_PORT_REQUESTS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>),
    'ok';
refresh(?KZ_ACDC_DB) ->
    couch_mgr:db_create(?KZ_ACDC_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_ACDC_DB, 'crossbar', <<"views/acdc.json">>),
    'ok';
refresh(?KZ_TOKEN_DB) ->
    _ = couch_mgr:db_create(?KZ_TOKEN_DB),
    couch_mgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),
    'ok';
refresh(Database) when is_binary(Database) ->
    case couch_util:db_classification(Database) of
        'account' -> refresh_account_db(Database);
        'modb' -> kazoo_modb:refresh_views(Database);
        'system' ->
            couch_mgr:db_create(Database),
            'ok';
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
refresh_account_db(Database) ->
    AccountDb = wh_util:format_account_id(Database, 'encoded'),
    AccountId = wh_util:format_account_id(Database, 'raw'),
    _ = remove_depreciated_account_views(AccountDb),
    _ = ensure_account_definition(AccountDb, AccountId),
    Views = get_all_account_views(),
    _ = whapps_util:update_views(AccountDb, Views, 'true'),
    crossbar_util:descendants_count(AccountId).

-spec remove_depreciated_account_views(ne_binary()) -> 'ok'.
remove_depreciated_account_views(AccountDb) ->
    _ = couch_mgr:del_doc(AccountDb, <<"_design/limits">>),
    _ = couch_mgr:del_doc(AccountDb, <<"_design/sub_account_reps">>),
    'ok'.

-spec ensure_account_definition(ne_binary(), ne_binary()) -> 'ok'.
ensure_account_definition(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} ->
            get_definition_from_accounts(AccountDb, AccountId);
        {'ok', _} -> 'ok'
    end.

-spec get_definition_from_accounts(ne_binary(), ne_binary()) -> 'ok'.
get_definition_from_accounts(AccountDb, AccountId) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"_rev">>, JObj));
        {'error', 'not_found'} ->
            io:format("    account ~s is missing its local account definition, and not in the accounts db~n"
                     ,[AccountId]),
            _ = couch_mgr:db_archive(AccountDb),
            maybe_delete_db(AccountDb)
    end.

-spec get_all_account_views() -> wh_proplist().
get_all_account_views() ->
    case get('account_views') of
        'undefined' ->
            Views = fetch_all_account_views(),
            put('account_views', Views),
            Views;
        Views -> Views
    end.

-spec fetch_all_account_views() -> wh_proplist().
fetch_all_account_views() ->
    [whapps_util:get_view_json('whistle_apps', ?MAINTENANCE_VIEW_FILE)
     ,whapps_util:get_view_json('conference', <<"views/conference.json">>)
     |whapps_util:get_views_json('crossbar', "account")
     ++ whapps_util:get_views_json('callflow', "views")
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_depreciated_databases() -> 'ok'.
remove_depreciated_databases() ->
    {'ok', Databases} = couch_mgr:db_info(),
    remove_depreciated_databases(Databases).

-spec remove_depreciated_databases(ne_binaries()) -> 'ok'.
remove_depreciated_databases([]) -> 'ok';
remove_depreciated_databases([Database|Databases]) ->
    _ = case couch_util:db_classification(Database) of
            'depreciated' ->
                io:format("    archive and remove depreciated database ~s~n"
                         ,[Database]),
                _ = couch_mgr:db_archive(Database),
                maybe_delete_db(Database);
            _Else -> 'ok'
        end,
    remove_depreciated_databases(Databases).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_aggregated_accounts() -> 'ok'.
cleanup_aggregated_accounts() ->
    case couch_mgr:all_docs(?WH_ACCOUNTS_DB, []) of
        {'ok', JObjs} -> cleanup_aggregated_accounts(JObjs);
        _ -> 'ok'
    end.

-spec cleanup_aggregated_accounts(wh_json:objects()) -> 'ok'.
cleanup_aggregated_accounts([]) -> 'ok';
cleanup_aggregated_accounts([JObj|JObjs]) ->
    _ = case wh_json:get_value(<<"id">>, JObj) of
            <<"_design", _/binary>> -> 'ok';
            AccountId ->
                io:format("    verifing ~s doc ~s~n"
                         ,[?WH_ACCOUNTS_DB, AccountId]),
                cleanup_aggregated_account(AccountId)
        end,
    cleanup_aggregated_accounts(JObjs).

-spec cleanup_aggregated_account(ne_binary()) -> 'ok'.
cleanup_aggregated_account(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} ->
            remove_aggregated_account(AccountDb);
        _Else -> 'ok'
    end.

-spec remove_aggregated_account(ne_binary()) -> 'ok'.
remove_aggregated_account(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    {'ok', JObj} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
    io:format("    removing invalid ~s doc ~s~n"
             ,[?WH_ACCOUNTS_DB, AccountId]),
    _ = couch_mgr:del_doc(?WH_ACCOUNTS_DB, JObj),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_aggregated_devices() -> 'ok'.
cleanup_aggregated_devices() ->
    case couch_mgr:all_docs(?WH_SIP_DB, []) of
        {'ok', JObjs} -> cleanup_aggregated_devices(JObjs);
        _ -> 'ok'
    end.

-spec cleanup_aggregated_devices(wh_json:objects()) -> 'ok'.
cleanup_aggregated_devices([]) -> 'ok';
cleanup_aggregated_devices([JObj|JObjs]) ->
    _ = case wh_json:get_value(<<"id">>, JObj) of
            <<"_design", _/binary>> -> 'ok';
            DocId ->
                io:format("    verifing ~s doc ~s~n"
                         ,[?WH_SIP_DB, DocId]),
                cleanup_aggregated_device(DocId)
        end,
    cleanup_aggregated_devices(JObjs).

-spec cleanup_aggregated_device(ne_binary()) -> 'ok'.
cleanup_aggregated_device(DocId) ->
    {'ok', JObj} = couch_mgr:open_doc(?WH_SIP_DB, DocId),
    case wh_json:get_first_defined([<<"pvt_account_db">>
                                   ,<<"pvt_account_id">>
                                   ], JObj)
    of
        'undefined' -> 'ok';
        Account ->
            AccountDb = wh_util:format_account_id(Account, 'encoded'),
            AccountId = wh_util:format_account_id(Account, 'raw'),
            verify_aggregated_device(AccountDb, AccountId, JObj)
    end.

-spec verify_aggregated_device(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
verify_aggregated_device(AccountDb, AccountId, JObj) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} ->
            io:format("    removing ~s doc ~s referencing missing db ~s~n"
                     ,[?WH_SIP_DB, AccountId, AccountDb]),
            _ = couch_mgr:del_doc(?WH_SIP_DB, JObj),
            'ok';
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_voicemail_media(ne_binary()) -> {'ok', wh_json:objects()} | {'error', any()}.
cleanup_voicemail_media(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Medias = get_medias(Account),
    Messages = get_messages(Account),
    ExtraMedia = lists:subtract(Medias, Messages),
    case couch_mgr:del_docs(AccountDb, ExtraMedia) of
        {'ok', _}=Res -> Res;
        {'error', _E}=Err ->
            lager:error("could not delete docs ~p: ~p", [ExtraMedia, _E]),
            Err
    end.

-spec get_messages(ne_binary()) -> ne_binaries().
get_messages(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(AccountDb, ?VMBOX_VIEW, ViewOptions) of
        {'ok', ViewRes} ->
            lists:foldl(fun extract_messages/2, [], ViewRes);
        {'error', _E} ->
            lager:error("coumd not load view ~p: ~p", [?VMBOX_VIEW, _E]),
            []
    end.

-spec extract_messages(wh_json:objects() | wh_json:object(), ne_binaries()) -> ne_binaries().
extract_messages([], CurMessages) -> CurMessages;
extract_messages([Mess|Messages], CurMessages) ->
    extract_messages(Messages, [wh_json:get_value(<<"media_id">>, Mess)|CurMessages]);
extract_messages(JObj, CurMessages) ->
    Messages = wh_json:get_value([<<"doc">>, <<"messages">>], JObj, []),
    extract_messages(Messages, CurMessages).

-spec get_medias(ne_binary()) -> ne_binaries().
get_medias(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    ViewOptions = [],
    case couch_mgr:get_results(AccountDb, ?PMEDIA_VIEW, ViewOptions) of
        {'ok', ViewRes} ->
            [wh_json:get_value(<<"id">>, JObj) || JObj<- ViewRes];
        {'error', _E} ->
            lager:error("could not load view ~p: ~p", [?PMEDIA_VIEW, _E]),
            []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_limits() -> 'ok'.
-spec migrate_limits(atom() | string() | binary()) -> 'ok'.

migrate_limits() ->
    migrate_all_limits(whapps_util:get_all_accounts()).

-spec migrate_all_limits(ne_binaries()) -> 'ok'.
migrate_all_limits(Accounts) ->
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_limits_fold(A, C, Total) end, 1, Accounts),
    'ok'.

migrate_limits_fold(AccountDb, Current, Total) ->
    io:format("checking limits doc in database (~p/~p) '~s'~n"
             ,[Current, Total, AccountDb]),
    _ = case couch_mgr:open_doc(AccountDb, <<"limits">>) of
            {'error', 'not_found'} -> migrate_limits(AccountDb);
            _Else -> 'ok'
        end,
    Current + 1.

migrate_limits(Account) when not is_binary(Account) ->
    migrate_limits(wh_util:to_binary(Account));
migrate_limits(Account) ->
    TStamp = wh_util:current_tstamp(),

    TwowayTrunks = whapps_config:get(<<"jonny5">>, <<"default_twoway_trunks">>),
    InboundTrunks = whapps_config:get(<<"jonny5">>, <<"default_inbound_trunks">>),

    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    {TT, IT} = clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks),
    JObj = wh_json:from_list(
             props:filter_undefined(
               [{<<"_id">>, <<"limits">>}
                ,{<<"twoway_trunks">>, TT}
                ,{<<"inbound_trunks">>, IT}
                ,{<<"pvt_account_db">>, AccountDb}
                ,{<<"pvt_account_id">>, wh_util:format_account_id(Account, 'raw')}
                ,{<<"pvt_type">>, <<"limits">>}
                ,{<<"pvt_created">>, TStamp}
                ,{<<"pvt_modified">>, TStamp}
                ,{<<"pvt_vsn">>, 1}
               ]
              )),
    _ = couch_mgr:save_doc(AccountDb, JObj),
    'ok'.

-spec clean_trunkstore_docs(ne_binary(), integer(), integer()) ->
                                   {integer(), integer()}.
clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks) ->
    ViewOptions = ['include_docs'
                   ,{'reduce', 'false'}
                  ],
    case couch_mgr:get_results(AccountDb, <<"trunkstore/crossbar_listing">>, ViewOptions) of
        {'ok', JObjs} -> clean_trunkstore_docs(AccountDb, JObjs, TwowayTrunks, InboundTrunks);
        {'error', _}=E -> E
    end.

-spec clean_trunkstore_docs(ne_binary(), wh_json:objects(), integer(), integer()) ->
                                   {integer(), integer()}.

clean_trunkstore_docs(_, [], Trunks, InboundTrunks) ->
    {Trunks, InboundTrunks};
clean_trunkstore_docs(AccountDb, [JObj|JObjs], Trunks, InboundTrunks) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    %% if there are no servers and it was created by jonny5 softdelete the doc
    _ = case wh_json:get_ne_value(<<"servers">>, Doc) =:= 'undefined'
            andalso wh_json:get_ne_value(<<"pvt_created_by">>, Doc) =:= <<"jonny5">>
        of
            'true' -> couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, 'true', Doc));
            'false' -> 'ok'
        end,
    NewTrunks = case wh_json:get_integer_value([<<"account">>, <<"trunks">>], Doc, 0) of
                    OldTrunks when OldTrunks > Trunks -> OldTrunks;
                    _ -> Trunks
                end,

    NewInboundTrunks =
        case wh_json:get_integer_value([<<"account">>, <<"inbound_trunks">>], Doc, 0) of
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
-spec migrate_media() -> 'ok'.
-spec migrate_media(atom() | string() | binary()) -> 'ok'.

migrate_media() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_media_fold(A, C, Total) end, 1, Accounts),
    'ok'.

migrate_media_fold(AccountDb, Current, Total) ->
    io:format("migrating media in database (~p/~p) '~s'", [Current, Total, AccountDb]),
    _ = migrate_media(AccountDb),
    couch_compactor_fsm:compact_db(AccountDb),
    Current + 1.

migrate_media(Account) when not is_binary(Account) ->
    migrate_media(wh_util:to_binary(Account));
migrate_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_by_name">>, []) of
        {'ok', []} -> io:format("no public media files in db ~s~n", [AccountDb]);
        {'ok', [_|_]=JObjs1}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs1],
            'ok';
        {'error', _}=E1 ->
            io:format("unable to fetch media files in db ~s: ~p~n", [AccountDb, E1])
    end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_private_media">>, []) of
        {'ok', []} -> io:format("no private media files in db ~s~n", [AccountDb]);
        {'ok', JObjs2}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs2],
            'ok';
        {'error', _}=E2 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E2])
    end.




%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_aggregate_devices() -> 'ok'.
ensure_aggregate_devices() ->
    ensure_aggregate_devices(whapps_util:get_all_accounts()).

-spec ensure_aggregate_devices(ne_binaries()) -> 'ok'.
ensure_aggregate_devices([]) -> 'ok';
ensure_aggregate_devices([Account|Accounts]) ->
    _ = ensure_aggregate_device(Account),
ensure_aggregate_devices(Accounts).

-spec ensure_aggregate_device(ne_binary()) -> 'ok'.
ensure_aggregate_device(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountRealm = crossbar_util:get_account_realm(AccountDb, AccountId),
    case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, ['include_docs']) of
        {'ok', Devices} ->
            _ = remove_aggregate_devices(AccountDb, AccountRealm, Devices),
            refresh_account_devices(AccountDb, AccountRealm, Devices);
        {'error', _} -> 'ok'
    end.

-spec refresh_account_devices(ne_binary(), ne_binary(), wh_json:objects()) -> 'ok'.
refresh_account_devices(AccountDb, AccountRealm, Devices) ->
    [whapps_util:add_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
     || Device <- Devices,
        wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =/= AccountRealm
            orelse wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"ip">>], Device, 'undefined') =/= 'undefined'
    ],
    'ok'.

-spec remove_aggregate_devices(ne_binary(), ne_binary(), wh_json:objects()) -> 'ok'.
remove_aggregate_devices(AccountDb, AccountRealm, Devices) ->
    [whapps_util:rm_aggregate_device(AccountDb, wh_json:get_value(<<"doc">>, Device))
     || Device <- Devices,
        wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], Device, AccountRealm) =:= AccountRealm
            andalso wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"ip">>], Device, 'undefined') =:= 'undefined'
    ],
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_invalid_acccount_dbs() -> ne_binaries().
find_invalid_acccount_dbs() ->
    lists:foldr(fun find_invalid_acccount_dbs_fold/2, [], whapps_util:get_all_accounts()).

find_invalid_acccount_dbs_fold(AccountDb, Acc) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> [AccountDb|Acc];
        {'ok', _} -> Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_migrate_attachments(AccountDb, Id, JObj) ->
    case wh_json:get_ne_value(<<"_attachments">>, JObj) of
        'undefined' ->
            io:format("media doc ~s/~s has no attachments, removing~n", [AccountDb, Id]),
            couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, 'true', JObj));
        Attachments ->
            _ = [catch migrate_attachment(AccountDb, JObj, K, V)
                 || {K,V} <- wh_json:to_proplist(Attachments)
                ]
    end.

-spec migrate_attachment(ne_binary(), wh_json:object()) -> 'ok'.
migrate_attachment(AccountDb, ViewJObj) ->
    Id = wh_json:get_value(<<"id">>, ViewJObj),
    _ = case couch_mgr:open_doc(AccountDb, Id) of
            {'error', _}=E1 -> io:format("unable to open media for attachment migration ~s/~s: ~p~n", [AccountDb, Id, E1]);
            {'ok', JObj1} ->
                maybe_migrate_attachments(AccountDb, Id, JObj1)
        end,

    %% we must reopen the doc since the _attachments has changed or we will effectively remove all attachments!
    case couch_mgr:open_doc(AccountDb, Id) of
        {'error', _}=E2 -> io:format("unable to open media for depreciated field removal ~s/~s: ~p~n", [AccountDb, Id, E2]);
        {'ok', JObj2} ->
            remove_deprecated_attachment_properties(AccountDb, Id, JObj2)
    end.

remove_deprecated_attachment_properties(AccountDb, Id, JObj) ->
    J = wh_json:delete_keys([<<"status">>, <<"content_size">>, <<"size">>
                             ,<<"content_type">>, <<"content_length">>
                             ,<<"format">>, <<"sample">>, <<"media_type">>
                            ], JObj),
    Result = case (J =/= JObj) andalso wh_json:get_value(<<"source_id">>, J) of
                 'false' -> 'ignore';
                 'undefined' ->
                     couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"media_source">>, <<"upload">>, J));
                 _Else ->
                     couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"media_source">>, <<"recording">>, J))
             end,
    case Result of
        'ignore' -> 'ok';
        {'ok', _} ->
            io:format("removed depreciated properties from ~s/~s~n", [AccountDb, Id]);
        {'error', _}=E3 ->
            io:format("removal of depreciated properties from ~s/~s failed: ~p~n", [AccountDb, Id, E3])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_attachment(ne_binary(), wh_json:object(), ne_binary(), wh_json:object()) -> 'ok'.
migrate_attachment(AccountDb, JObj, Attachment, MetaData) ->
    DocCT = wh_json:get_value(<<"content_type">>, JObj),
    MetaCT = wh_json:get_value(<<"content_type">>, MetaData),
    Migrations = [fun({A, _CT}) ->
                          case {is_audio_content(DocCT), is_audio_content(MetaCT)} of
                              {_, 'true'} -> {A, MetaCT};
                              {'true', _} -> {A, DocCT};
                              {_, _} ->
                                  Ext = wh_util:to_list(filename:extension(A)),
                                  case mochiweb_mime:from_extension(Ext) of
                                      'undefined' -> {A, <<"audio/mpeg">>};
                                      MIME -> {A, wh_util:to_binary(MIME)}
                                  end
                          end
                  end
                  ,fun({A, CT}) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               'false' -> {A, CT};
                               'true' -> {add_extension(A, CT), CT}
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
-spec maybe_update_attachment(ne_binary(), ne_binary(), attachment_and_content(), attachment_and_content()) -> 'ok'.
maybe_update_attachment(_, _, {Attachment, CT}, {Attachment, CT}) ->
    'ok';
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
                            {'ok', _}=Ok -> Ok;
                            {'error', _}=E ->
                                io:format("unable to fetch attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttch, E]),
                                E
                            end
                end
                ,fun({'ok', Content1}) ->
                         {'ok', Rev} = couch_mgr:lookup_doc_rev(AccountDb, Id),
                         Options = [{'headers', [{'content_type', wh_util:to_list(CT)}]}
                                    ,{'rev', Rev}
                                   ],
                         %% bigcouch is awesome in that it sometimes returns 409 (conflict) but does the work anyway..
                         %%   so rather than check the put return fetch the new attachment and compare it to the old
                         Result = couch_mgr:put_attachment(AccountDb, Id, NewAttch, Content1, Options),
                         {'ok', JObj} = couch_mgr:open_doc(AccountDb, Id),
                         case wh_json:get_value([<<"_attachments">>, OrigAttch, <<"length">>], JObj) =:= wh_json:get_value([<<"_attachments">>, NewAttch, <<"length">>], JObj) of
                             'false' ->
                                 io:format("unable to put new attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttch, Result]),
                                 {'error', 'length_mismatch'};
                             'true' ->
                                 Filename = wh_util:to_list(<<"/tmp/media_", Id/binary, "_", OrigAttch/binary>>),
                                 case file:write_file(Filename, Content1) of
                                     'ok' -> 'ok';
                                     {'error', _}=E2 ->
                                         io:format("unable to backup attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttch, E2]),
                                         E2
                                 end
                         end
                 end
                ,fun('ok') ->
                         case OrigAttch =/= NewAttch of
                             'true' ->
                                 case couch_mgr:delete_attachment(AccountDb, Id, OrigAttch) of
                                     {'ok', _} ->
                                         io:format("updated attachment name ~s/~s/~s~n", [AccountDb, Id, NewAttch]),
                                         'ok';
                                     {'error', _}=E ->
                                         io:format("unable to remove original attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttch, E]),
                                         'error'
                                 end;
                             'false' ->
                                 io:format("updated content type for ~s/~s/~s~n", [AccountDb, Id, NewAttch]),
                                 'ok'
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
-spec add_extension(ne_binary(), ne_binary()) -> ne_binary().
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
-spec is_audio_content(ne_binary()) -> boolean().
is_audio_content(<<"audio/", _/binary>>) -> 'true';
is_audio_content(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_delete_db(ne_binary()) -> 'ok'.
maybe_delete_db(Database) ->
    case whapps_config:get_is_true(<<"whistle_couch">>, <<"allow_maintenance_db_delete">>, 'false') of
        'true' ->
            lager:warning("deleting database ~s", [Database]),
            couch_mgr:db_delete(Database);
        'false' ->
            lager:warning("database deletion requested but disabled for ~s", [Database])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec purge_doc_type(ne_binaries() | ne_binary(), ne_binary()) ->
                            {'ok', wh_json:objects()} |
                            {'error', term()} |
                            'ok'.
-spec purge_doc_type(ne_binaries() | ne_binary(), ne_binary(), integer()) ->
                            {'ok', wh_json:objects()} |
                            {'error', term()} |
                            'ok'.
purge_doc_type([], _Account) -> 'ok';
purge_doc_type([Type|Types], Account) ->
    _ = purge_doc_type(Type, Account),
    purge_doc_type(Types, Account, whapps_config:get_integer(<<"whistle_couch">>, <<"default_chunk_size">>, 1000));
purge_doc_type(Type, Account) when not is_binary(Type) ->
    purge_doc_type(wh_util:to_binary(Type), Account, whapps_config:get_integer(<<"whistle_couch">>, <<"default_chunk_size">>, 1000));
purge_doc_type(Type, Account) when not is_binary(Account) ->
    purge_doc_type(Type, wh_util:to_binary(Account), whapps_config:get_integer(<<"whistle_couch">>, <<"default_chunk_size">>, 1000)).
purge_doc_type(Type, Account, ChunkSize) ->
    Db = wh_util:format_account_id(Account, 'encoded'),
    Opts = [{'key', Type}
           ,{'limit', ChunkSize}
           ,'include_docs'
           ],
    case couch_mgr:get_results(Db, <<"maintenance/listing_by_type">>, Opts) of
        {'error', _}=E -> E;
        {'ok', []} -> 'ok';
        {'ok', Ds} ->
            lager:debug('deleting up to ~p documents of type ~p', [ChunkSize, Type]),
            couch_mgr:del_docs(Db, [wh_json:get_value(<<"doc">>, D) || D <- Ds]),
            purge_doc_type(Type, Account, ChunkSize)
    end.

-spec call_id_status(ne_binary()) -> 'ok'.
-spec call_id_status(ne_binary(), boolean() | ne_binary()) -> 'ok'.
call_id_status(CallId) ->
    call_id_status(CallId, 'false').
call_id_status(CallId, Verbose) ->
    Req = [{<<"Call-ID">>, wh_util:to_binary(CallId)}
           | wh_api:default_headers(<<"shell">>, <<"0">>)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_call:publish_channel_status_req/1
                                       ,fun wapi_call:channel_status_resp_v/1
                                      )
    of
        {'ok', Resp} ->
            show_status(CallId, wh_util:is_true(Verbose), Resp);
        {'error', _E} ->
            lager:info("failed to get status of '~s': '~p'", [CallId, _E])
    end.

show_status(CallId, 'false', Resp) ->
    lager:info("channel '~s' has status '~s'", [CallId, wapi_call:get_status(Resp)]);
show_status(CallId, 'true', Resp) ->
    lager:info("Channel ~s", [CallId]),
    lager:info("Status: ~s", [wh_json:get_value(<<"Status">>, Resp)]),
    lager:info("Media Server: ~s", [wh_json:get_value(<<"Switch-Hostname">>, Resp)]),
    lager:info("Responding App: ~s", [wh_json:get_value(<<"App-Name">>, Resp)]),
    lager:info("Responding Node: ~s", [wh_json:get_value(<<"Node">>, Resp)]).
