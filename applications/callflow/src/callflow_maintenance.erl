%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(callflow_maintenance).

-export([lookup_endpoint/1
         ,lookup_endpoint/2
        ]).
-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).
-export([migrate_menus/0, migrate_menus/1]).
-export([migrate_recorded_names/0
        ,migrate_recorded_name/1
        ]).
-export([show_calls/0]).
-export([flush/0]).
-export([account_set_classifier_inherit/2
         ,account_set_classifier_deny/2
         ,all_accounts_set_classifier_inherit/1
         ,all_accounts_set_classifier_deny/1
         ,device_classifier_inherit/2
         ,device_classifier_deny/2
         ,list_account_restrictions/1
        ]).
-export([update_feature_codes/0, update_feature_codes/1]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_endpoint(ne_binary()) -> 'no_return'.
lookup_endpoint(URI) ->
    case binary:split(URI, <<"@">>) of
        [Username, Realm] -> lookup_endpoint(Username, Realm);
        _Else -> io:format("invalid SIP URI~n", [])
    end.

-spec lookup_endpoint(ne_binary(), ne_binary()) -> 'no_return'.
lookup_endpoint(Username, Realm) ->
    _ = case kapps_util:get_account_by_realm(Realm) of
            {'ok', AccountDb} ->
                case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
                    {'ok', EndpointId} ->
                        Endpoint = kz_endpoint:get(EndpointId, AccountDb),
                        io:format("~p~n", [Endpoint]);
                    _Else -> io:format("unable to find username ~s in ~s~n", [Username, AccountDb])
                end;
            _Else -> io:format("unable to find account with realm ~s~n", [Realm])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec show_calls() -> 'ok'.
show_calls() ->
    do_show_calls(cf_exe_sup:workers(), 0).

do_show_calls([], Total) ->
    io:format("Total: ~p~n", [Total]);
do_show_calls([Srv|Srvs], Total) ->
    case catch(cf_exe:get_call(Srv)) of
        {'ok', Call} ->
            io:format("CF_EXE(~p): ~p~n", [Srv, kapps_call:to_proplist(Call)]);
        _ -> 'ok'
    end,
    do_show_calls(Srvs, Total + 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh() -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun(AccountDb) ->
                          refresh(AccountDb)
                  end, kapps_util:get_all_accounts()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'started'.
refresh() ->
    _ = kz_util:spawn(fun blocking_refresh/0),
    'started'.

-spec refresh(binary() | string()) -> 'ok'.
refresh(<<Account/binary>>) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    Views = kapps_util:get_views_json('callflow', "views"),
    kapps_util:update_views(AccountDb, Views);
refresh(Account) ->
    refresh(kz_util:to_binary(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate_recorded_names() -> 'no_return'.
migrate_recorded_names() ->
    migrate_recorded_names(kapps_util:get_all_accounts()).

-spec migrate_recorded_names(ne_binaries()) -> 'no_return'.
migrate_recorded_names([]) -> 'no_return';
migrate_recorded_names([Account|Accounts]) ->
    _ = (catch migrate_recorded_name(Account)),
    migrate_recorded_names(Accounts).

-spec migrate_recorded_name(ne_binary()) -> any().
migrate_recorded_name(Db) ->
    lager:info("migrating all name recordings from vmboxes w/ owner_id in ~s", [Db]),

    case kz_datamgr:get_results(Db, <<"vmboxes/crossbar_listing">>, ['include_docs']) of
        {'ok', []} -> lager:info("no vmboxes in ~s", [Db]);
        {'error', _E} -> lager:info("unable to get vm box list: ~p", [_E]);
        {'ok', VMBoxes} ->
            [do_recorded_name_migration(Db, kz_json:get_value(<<"doc">>, VMBox))
             || VMBox <- VMBoxes
            ]
    end.

-spec do_recorded_name_migration(ne_binary(), kz_json:object()) -> any().
-spec do_recorded_name_migration(ne_binary(), kz_json:object(), api_binary()) -> any().
do_recorded_name_migration(Db, VMBox) ->
    VMBoxId = kz_doc:id(VMBox),
    case kz_json:get_value(?RECORDED_NAME_KEY, VMBox) of
        'undefined' -> lager:info("vm box ~s has no recorded name to migrate", [VMBoxId]);
        MediaId ->
            lager:info("vm box ~s has recorded name in doc ~s", [VMBoxId, MediaId]),
            do_recorded_name_migration(Db, MediaId, kz_json:get_value(<<"owner_id">>, VMBox)),
            {'ok', _} = kz_datamgr:save_doc(Db, kz_json:delete_key(?RECORDED_NAME_KEY, VMBox))
    end.

do_recorded_name_migration(_Db, _MediaId, 'undefined') ->
    lager:info("no owner id on vm box");
do_recorded_name_migration(Db, MediaId, OwnerId) ->
    {'ok', Owner} = kz_datamgr:open_doc(Db, OwnerId),
    case kz_json:get_value(?RECORDED_NAME_KEY, Owner) of
        'undefined' ->
            lager:info("no recorded name on owner, setting to ~s", [MediaId]),
            {'ok', _} = kz_datamgr:save_doc(Db, kz_json:set_value(?RECORDED_NAME_KEY, MediaId, Owner)),
            lager:info("updated owner doc with recorded name doc id ~s", [MediaId]);
        MediaId ->
            lager:info("owner already has recorded name at ~s", [MediaId]);
        OwnerMediaId ->
            lager:info("owner has recorded name at ~s(not ~s), using owners", [OwnerMediaId, MediaId]),
            kz_datamgr:del_doc(Db, MediaId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will migrate all the menus mailbox documents to
%% the latest version.
%% @end
%%--------------------------------------------------------------------
-spec migrate_menus() -> ['done' | 'error',...].
-spec migrate_menus(ne_binary()) -> 'done' | 'error'.
migrate_menus() ->
    [migrate_menus(Account) || Account <- kapps_util:get_all_accounts('raw')].
migrate_menus(Account) ->
    Db = kz_util:format_account_id(Account, 'encoded'),
    lager:info("migrating all menus in ~s", [Db]),
    case kz_datamgr:get_results(Db, <<"menus/crossbar_listing">>, ['include_docs']) of
        {'ok', []} ->
            lager:info("db ~s has no menus", [Db]),
            'done';
        {'ok', Menus} ->
            [do_menu_migration(Menu, Db) || Menu <- Menus];
        {'error', _E} ->
            lager:info("unable to get a list of menus: ~p", [_E]),
            'error'
    end.

do_menu_migration(Menu, Db) ->
    Doc = kz_json:get_value(<<"doc">>, Menu),
    MenuId = kz_doc:id(Doc),
    VSN = kz_doc:vsn(Doc, 1),
    case kz_datamgr:fetch_attachment(Db, MenuId, <<"prompt.mp3">>) of
        {'ok', _} when VSN =/= 1 ->
            lager:info("menu ~s in ~s already migrated", [MenuId, Db]);
        {'ok', Bin} ->
            Name = <<(kz_json:get_value(<<"name">>, Doc, <<>>))/binary, " menu greeting">>,
            MediaId = create_media_doc(Name, <<"menu">>, MenuId, Db),
            AName = <<(kz_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".mp3">>,
            {'ok', _} = kz_datamgr:put_attachment(Db, MediaId, AName, Bin),
            'ok' = update_doc([<<"media">>, <<"greeting">>], MediaId, MenuId, Db),
            'ok' = update_doc([<<"pvt_vsn">>], <<"2">>, MenuId, Db),
            lager:info("migrated menu ~s in ~s prompt to /~s/~s/~s", [MenuId, Db, Db, MediaId, AName]);
        _ ->
            lager:info("menu ~s in ~s has no greeting or prompt", [MenuId, Db])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_media_doc(binary(), binary(), binary(), binary()) -> binary().
create_media_doc(Name, SourceType, SourceId, Db) ->
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<SourceType/binary, " recorded/prompt media">>}
             ,{<<"source_type">>, SourceType}
             ,{<<"source_id">>, SourceId}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, 'true'}],
    Doc = kz_doc:update_pvt_parameters(kz_json:from_list(Props), Db, [{'type', <<"media">>}]),
    {'ok', JObj} = kz_datamgr:save_doc(Db, Doc),
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc(list() | binary(), kz_json:json_term(), binary(), binary()) ->
                        'ok' |
                        {'error', atom()}.
update_doc(Key, Value, Id, Db) ->
    case kz_datamgr:open_doc(Db, Id) of
        {'ok', JObj} ->
            case kz_datamgr:save_doc(Db, kz_json:set_value(Key, Value, JObj)) of
                {'error', 'conflict'} -> update_doc(Key, Value, Id, Db);
                {'ok', _} -> 'ok';
                {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
            end;
        {'error', _}=E ->
            lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%        Set call_restriction flag on account level
%%        Usage: sup callflow_maintenance account_set_classifier_inherit international accountname
%%        Usage: sup callflow_maintenance account_set_classifier_deny international accountname
%%        Usage: sup callflow_maintenance all_accounts_set_classifier_inherit international
%%        Usage: sup callflow_maintenance all_accounts_set_classifier_deny international
%% @end
%%--------------------------------------------------------------------

-spec account_set_classifier_inherit(ne_binary(), ne_binary()) -> 'ok'.
account_set_classifier_inherit(Classifier, Account) ->
    {'ok', AccountDb} = kapps_util:get_accounts_by_name(kz_util:normalize_account_name(Account)),
    set_account_classifier_action(<<"inherit">>, Classifier, AccountDb).

-spec account_set_classifier_deny(ne_binary(), ne_binary()) -> 'ok'.
account_set_classifier_deny(Classifier, Account) ->
    {'ok', AccountDb} = kapps_util:get_accounts_by_name(kz_util:normalize_account_name(Account)),
    set_account_classifier_action(<<"deny">>, Classifier, AccountDb).

-spec all_accounts_set_classifier_inherit(ne_binary()) -> 'ok'.
all_accounts_set_classifier_inherit(Classifier) ->
    all_accounts_set_classifier(<<"inherit">>, Classifier).

-spec all_accounts_set_classifier_deny(ne_binary()) -> 'ok'.
all_accounts_set_classifier_deny(Classifier) ->
    all_accounts_set_classifier(<<"deny">>, Classifier).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_account_classifier_action(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
set_account_classifier_action(Action, Classifier, AccountDb) ->
    'true' = is_classifier(Classifier),
    io:format("found account: ~p", [get_account_name_by_db(AccountDb)]),
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),

    kz_datamgr:update_doc(AccountDb, AccountId, [{[<<"call_restriction">>, Classifier, <<"action">>], Action}]),
    kz_datamgr:update_doc(<<"accounts">>, AccountId, [{[<<"call_restriction">>, Classifier, <<"action">>], Action}]),

    kz_endpoint:flush_account(AccountDb),

    io:format("  ...  classifier '~s' switched to action '~s'\n", [Classifier, Action]).

all_accounts_set_classifier(Action, Classifier) ->
    'true' = is_classifier(Classifier),
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2000),
                          %% Not shure if this interruption is realy needed.
                          %%  Keeping it as it was taken as an example from kapps_util:update_all_accounts/1
                          set_account_classifier_action(Action, Classifier, AccountDb)
                  end, kapps_util:get_all_accounts()).

-spec get_account_name_by_db(ne_binary()) -> ne_binary() | 'undefined'.
get_account_name_by_db(AccountDb) ->
    case kz_account:fetch(AccountDb) of
        {'error', _Error} ->
            lager:error('error opening account doc ~p', [AccountDb]),
            'undefined';
        {'ok', JObj} -> kz_account:name(JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%        Set call_restriction flag on device level
%%        Usage: sup callflow_maintenance device_classifier_inherit international  username@realm.tld
%%        Usage: sup callflow_maintenance device_classifier_deny international username@realm.tld
%% @end
%%--------------------------------------------------------------------

-spec device_classifier_inherit(ne_binary(), ne_binary()) -> 'ok'.
device_classifier_inherit(Classifier, Uri) ->
    set_device_classifier_action(<<"inherit">>, Classifier, Uri).

-spec device_classifier_deny(ne_binary(), ne_binary()) -> 'ok'.
device_classifier_deny(Classifier, Uri) ->
    set_device_classifier_action(<<"deny">>, Classifier, Uri).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_device_classifier_action(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
set_device_classifier_action(Action, Classifier, Uri) ->
    'true' = is_classifier(Classifier),
    [User, Realm] = re:split(Uri, <<"@">>),
    {'ok', AccountDb} = kapps_util:get_account_by_realm(Realm),
    Options = [{'key', User}],
    {'ok', [DeviceDoc]} = kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, Options),
    DeviceId = kz_doc:id(DeviceDoc),
    kz_datamgr:update_doc(AccountDb, DeviceId, [{[<<"call_restriction">>, Classifier, <<"action">>], Action}]),
    kz_endpoint:flush(AccountDb, DeviceId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%           Checks if classifier defined in system_config -> number_manager doc
%% @end
%%--------------------------------------------------------------------
-spec is_classifier(ne_binary()) -> boolean().
is_classifier(Classifier) ->
    Classifiers = kz_json:get_keys(knm_converters:available_classifiers()),
    case lists:member(Classifier, Classifiers) of
        'true' -> 'true';
        'false' ->
            io:format("classifier '~s' not among configured classifiers: ~p"
                      ,[Classifier, Classifiers]
                     ),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%        Lists call restrictions on all levels of an account
%%        Usage: sup callflow_maintenance list_account_restrictions accountname
%% @end
%%--------------------------------------------------------------------
-spec list_account_restrictions(ne_binary()) -> 'ok'.
list_account_restrictions(Account) ->
    {'ok', AccountDb} = kapps_util:get_accounts_by_name(kz_util:normalize_account_name(Account)),
    DbNameEncoded = kz_util:format_account_id(AccountDb,'encoded'),
    io:format("\nAccount level classifiers:\n\n"),
    print_call_restrictions(DbNameEncoded, kz_util:format_account_id(AccountDb,'raw')),
    print_users_level_call_restrictions(DbNameEncoded),
    print_devices_level_call_restrictions(DbNameEncoded),
    print_trunkstore_call_restrictions(DbNameEncoded).

-spec print_call_restrictions(ne_binary(), ne_binary()) -> 'ok'.
print_call_restrictions(DbName, DocId) ->
    case kz_datamgr:open_doc(DbName, DocId) of
        {'ok', JObj} ->
            lists:foreach(fun(Classifier) ->
                             io:format("Classifier ~p:\t\t action ~p\n",[Classifier, kz_json:get_value([<<"call_restriction">>,Classifier,<<"action">>], JObj)])
                          end,
                          kz_json:get_keys(<<"call_restriction">>, JObj));
        {'error', E} ->
            io:format("An error occurred: ~p\n", [E])
    end.

-spec print_users_level_call_restrictions(ne_binary()) -> 'ok'.
print_users_level_call_restrictions(DbName) ->
        case kz_datamgr:get_results(DbName, <<"users/crossbar_listing">>) of
        {'ok', JObj} ->
            io:format("\n\nUser level classifiers:\n"),
            lists:foreach(fun(UserObj) ->
                             io:format("\nUsername: ~s\n\n", [kz_json:get_value([<<"value">>,<<"username">>],UserObj)]),
                             print_call_restrictions(DbName, kz_doc:id(UserObj))
                          end,
                          JObj);
        {'error', E} ->
            io:format("An error occurred: ~p", [E])
    end.

-spec print_devices_level_call_restrictions(ne_binary()) -> 'ok'.
print_devices_level_call_restrictions(DbName) ->
        case kz_datamgr:get_results(DbName, <<"devices/crossbar_listing">>) of
        {'ok', JObj} ->
            io:format("\n\nDevice level classifiers:\n"),
            lists:foreach(fun(UserObj) ->
                             io:format("\nDevice: ~s\n\n", [kz_json:get_value([<<"value">>,<<"name">>],UserObj)]),
                             print_call_restrictions(DbName, kz_doc:id(UserObj))
                          end,
                          JObj);
        {'error', E} ->
            io:format("An error occurred: ~p", [E])
    end.

-spec print_trunkstore_call_restrictions(ne_binary()) -> 'ok'.
print_trunkstore_call_restrictions(DbName) ->
        case kz_datamgr:get_results(DbName, <<"trunkstore/lookup_user_flags">>) of
        {'ok', JObj} ->
            io:format("\n\nTrunkstore classifiers:\n\n"),
            lists:foreach(fun(UserObj) ->
                             io:format("Trunk: ~s@~s\n\n", lists:reverse(kz_json:get_value(<<"key">>,UserObj))),
                             print_call_restrictions(DbName, kz_doc:id(UserObj))
                          end,
                          JObj);
        {'error', E} ->
            io:format("An error occurred: ~p", [E])
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update certain patterns matching feature codes (see KAZOO-3122)
%% @end
%%--------------------------------------------------------------------

-spec update_feature_codes() -> 'ok'.
update_feature_codes() ->
    lists:foreach(fun update_feature_codes/1, kapps_util:get_all_accounts()).

-spec update_feature_codes(ne_binary()) -> 'ok'.
update_feature_codes(Account)
  when not is_binary(Account) ->
    update_feature_codes(kz_util:to_binary(Account));
update_feature_codes(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, ?LIST_BY_PATTERN, ['include_docs']) of
        {'error', _Reason} ->
            io:format("error listing feature code patterns: ~p\n", [_Reason]);
        {'ok', Patterns} ->
            AccountId = kz_util:format_account_id(Account, 'raw'),
            io:format("~s : looking through patterns...\n", [AccountId]),
            maybe_update_feature_codes(AccountDb, Patterns)
    end.

maybe_update_feature_codes(Db, []) ->
    io:format("~s : feature codes up to date\n", [kz_util:format_account_id(Db, 'raw')]);
maybe_update_feature_codes(Db, [Pattern|Patterns]) ->
    DocId = kz_doc:id(Pattern),
    Regex = kz_json:get_value(<<"key">>, Pattern),
    case Regex of
        <<"^\\*5([0-9]*)$">> ->
            NewRegex = <<"^\\*5(|[0-9]{2,})$">>,
            case kz_datamgr:update_doc(Db, DocId, [{<<"patterns">>, [NewRegex]}]) of
                {'error', _Reason} ->
                    io:format("failed to update doc ~s with new patterns\n", [DocId]);
                {'ok', _} ->
                    io:format("successfully updated patterns for doc ~s (~p -> ~p)\n",
                              [DocId, Regex, NewRegex])
            end;
        _OtherRegex ->
            io:format("skipping pattern ~p\n", [_OtherRegex])
    end,
    maybe_update_feature_codes(Db, Patterns).
