%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Utilities shared by a subset of `kapps'.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_util).

-export([update_all_accounts/1]).
-export([replicate_from_accounts/2, replicate_from_account/3]).
-export([revise_whapp_views_in_accounts/1]).
-export([get_all_accounts/0
        ,get_all_accounts/1
        ,get_all_accounts_and_mods/0
        ,get_all_accounts_and_mods/1
        ,get_all_account_mods/0
        ,get_all_account_mods/1
        ,get_account_mods/1
        ,get_account_mods/2
        ]).
-export([is_account_db/1
        ,is_account_mod/1
        ]).
-export([get_account_by_realm/1
        ,get_ccvs_by_ip/1
        ,get_accounts_by_name/1

        ,are_all_enabled/1
        ]).
-export([get_master_account_id/0
        ,get_master_account_db/0
        ]).
-export([is_master_account/1]).
-export([account_depth/1]).
-export([account_has_descendants/1
        ,account_descendants/1
        ]).
-export([find_oldest_doc/1]).
-export([get_call_termination_reason/1]).
-export([get_view_json/1, get_view_json/2]).
-export([get_views_json/2]).
-export([add_aggregate_device/2]).
-export([rm_aggregate_device/2]).
-export([get_destination/3]).
-export([get_origination/3]).

-export([write_tts_file/2]).
-export([to_magic_hash/1
        ,from_magic_hash/1
        ]).
-export([get_application/0
        ,put_application/1
        ]).
-export([epmd_enabled/0
        ,epmd_disabled/0
        ]).

-include("kazoo_apps.hrl").

-define(REPLICATE_ENCODING, 'encoded').
-define(AGG_LIST_BY_REALM, <<"accounts/listing_by_realm">>).
-define(AGG_LIST_BY_NAME, <<"accounts/listing_by_name">>).
-define(AGG_LIST_BY_IP, <<"credentials/lookup_by_ip">>).
-define(PROMPTS_CONFIG_CAT, <<"prompts">>).

-define(ACCT_BY_NAME_CACHE(Name), {?MODULE, 'account_by_name', Name}).
-define(ACCT_BY_REALM_CACHE(Name), {?MODULE, 'account_by_realm', Name}).
-define(ACCT_BY_IP_CACHE(IP), {?MODULE, 'account_by_ip', IP}).
-define(GET_BY_CACHE_ORIGIN, [{'origin', [{'db', ?KZ_ACCOUNTS_DB, <<"account">>}]}]).

%%------------------------------------------------------------------------------
%% @doc Update a document in each crossbar account database with the
%% file contents.  This is intended for `_design' docs.
%%
%% @end
%%------------------------------------------------------------------------------
-spec update_all_accounts(kz_term:ne_binary()) -> 'ok'.
update_all_accounts(File) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          kz_datamgr:revise_doc_from_file(AccountDb, 'crossbar', File)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%------------------------------------------------------------------------------
%% @doc This function will import every `.json' file found in the given
%% application `priv/couchdb/views/' folder into every account database.
%% @end
%%------------------------------------------------------------------------------
-spec revise_whapp_views_in_accounts(atom()) -> 'ok'.
revise_whapp_views_in_accounts(App) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          kz_datamgr:revise_views_from_folder(AccountDb, App)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%------------------------------------------------------------------------------
%% @doc This function will replicate the results of the filter from each
%% account db into the target database.
%% @end
%%------------------------------------------------------------------------------
-spec replicate_from_accounts(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
replicate_from_accounts(TargetDb, FilterDoc) when is_binary(FilterDoc) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          replicate_from_account(AccountDb, TargetDb, FilterDoc)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%------------------------------------------------------------------------------
%% @doc This function will replicate the results of the filter from the
%% source database into the target database.
%% @end
%%------------------------------------------------------------------------------
-spec replicate_from_account(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' | {'error', 'matching_dbs'}.
replicate_from_account(AccountDb, AccountDb, _) ->
    lager:debug("requested to replicate from db ~s to self, skipping", [AccountDb]),
    {'error', 'matching_dbs'};
replicate_from_account(AccountDb, TargetDb, FilterDoc) ->
    ReplicateProps = [{<<"source">>, kzs_util:format_account_id(AccountDb, ?REPLICATE_ENCODING)}
                     ,{<<"target">>, TargetDb}
                     ,{<<"filter">>, FilterDoc}
                     ,{<<"create_target">>, 'true'}
                     ],
    try kz_datamgr:db_replicate(ReplicateProps) of
        {'ok', _} ->
            lager:debug("replicate ~s to ~s using filter ~s succeeded", [AccountDb, TargetDb, FilterDoc]);
        {'error', _} ->
            lager:debug("replicate ~s to ~s using filter ~s failed", [AccountDb, TargetDb, FilterDoc])
    catch
        _:_ ->
            lager:debug("replicate ~s to ~s using filter ~s error", [AccountDb, TargetDb, FilterDoc])
    end.

%%------------------------------------------------------------------------------
%% @doc Find the system admin from the `system_config' if set, if not
%% set it to the oldest account and return that.
%% @end
%%------------------------------------------------------------------------------
-spec get_master_account_id() -> {'ok', kz_term:ne_binary()} |
          {'error', atom()}.
get_master_account_id() ->
    case kapps_config:get_ne_binary(?KZ_ACCOUNTS_DB, <<"master_account_id">>) of
        'undefined' ->
            R = kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_id">>, ['include_docs']),
            find_master_account_id(R);
        Default -> {'ok', Default}
    end.

find_master_account_id({'error', _}=E) -> E;
find_master_account_id({'ok', []}) -> {'error', 'no_accounts'};
find_master_account_id({'ok', Accounts}) ->
    {'ok', OldestAccountId}=Ok =
        find_oldest_doc([kz_json:get_value(<<"doc">>, Account)
                         || Account <- Accounts
                        ]),
    lager:debug("setting ~s.master_account_id to ~s", [?KZ_ACCOUNTS_DB, OldestAccountId]),
    {'ok', _} = kapps_config:set(?KZ_ACCOUNTS_DB, <<"master_account_id">>, OldestAccountId),
    Ok.

%%------------------------------------------------------------------------------
%% @doc Find the system admin database.
%% @see get_master_account_id/0
%% @end
%%------------------------------------------------------------------------------
-spec get_master_account_db() -> {'ok', kz_term:ne_binary()} |
          {'error', any()}.
get_master_account_db() ->
    case get_master_account_id() of
        {'error', _}=E -> E;
        {'ok', AccountId} ->
            {'ok', kzs_util:format_account_db(AccountId)}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns whether or not the  Account' is system admin or not.
%% @end
%%------------------------------------------------------------------------------
-spec is_master_account(kz_term:ne_binary()) -> boolean().
is_master_account(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    case get_master_account_id() of
        {'ok', AccountId} -> 'true';
        _Else -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Returns length of the given account tree.
%% @end
%%------------------------------------------------------------------------------
-spec account_depth(kz_term:ne_binary()) -> kz_term:api_non_neg_integer().
account_depth(Account) ->
    {'ok', JObj} = kzd_accounts:fetch(Account),
    length(kzd_accounts:tree(JObj)).

%%------------------------------------------------------------------------------
%% @doc List an account's descendants (including the provided AccountId).
%% @end
%%------------------------------------------------------------------------------
-spec account_descendants(kz_term:ne_binary()) -> kz_term:ne_binaries().
account_descendants(?MATCH_ACCOUNT_RAW(AccountId)) ->
    View = <<"accounts/listing_by_descendants">>,
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, View, ViewOptions) of
        {'ok', JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Checks if account has descendants or not.
%% @end
%%------------------------------------------------------------------------------
-spec account_has_descendants(kz_term:ne_binary()) -> boolean().
account_has_descendants(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    [] =/= (account_descendants(AccountId) -- [AccountId]).

%%------------------------------------------------------------------------------
%% @doc Given a list of accounts this returns the id of the oldest.
%% @end
%%------------------------------------------------------------------------------
-spec find_oldest_doc(kz_json:objects()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_docs'}.
find_oldest_doc([]) -> {'error', 'no_docs'};
find_oldest_doc([First|Docs]) ->
    {_, OldestDocID} =
        lists:foldl(fun(Doc, {Created, _}=Eldest) ->
                            Older = kz_doc:created(Doc),
                            case Older < Created  of
                                'true' -> {Older, kz_doc:id(Doc)};
                                'false' -> Eldest
                            end
                    end
                   ,{kz_doc:created(First), kz_doc:id(First)}
                   ,Docs),
    {'ok', OldestDocID}.

%%------------------------------------------------------------------------------
%% @doc This function will return a list of all account database names
%% in the requested encoding.
%% @end
%%------------------------------------------------------------------------------

-spec get_all_accounts() -> kz_term:ne_binaries().
get_all_accounts() -> get_all_accounts(?REPLICATE_ENCODING).

-spec get_all_accounts(kz_util:account_format()) -> kz_term:ne_binaries().
get_all_accounts(Encoding) ->
    {'ok', Dbs} = kz_datamgr:db_list([{'startkey', <<"account/">>}
                                     ,{'endkey', <<"account/\ufff0">>}
                                     ]),
    [kzs_util:format_account_id(Db, Encoding)
     || Db <- Dbs, is_account_db(Db)
    ].

-spec get_all_accounts_and_mods() -> kz_term:ne_binaries().
get_all_accounts_and_mods() ->
    get_all_accounts_and_mods(?REPLICATE_ENCODING).

-spec get_all_accounts_and_mods(kz_util:account_format()) -> kz_term:ne_binaries().
get_all_accounts_and_mods(Encoding) ->
    {'ok', Databases} = kz_datamgr:db_info(),
    [format_db(Db, Encoding)
     || Db <- Databases,
        is_account_db(Db)
            orelse is_account_mod(Db)
    ].

-spec format_db(kz_term:ne_binary(), kz_util:account_format()) -> kz_term:ne_binary().
format_db(Db, Encoding) ->
    Fs = [{fun is_account_db/1, fun kzs_util:format_account_id/2}
         ,{fun is_account_mod/1, fun kzs_util:format_account_modb/2}
         ],
    format_db(Db, Encoding, Fs).

format_db(Db, Encoding, [{Predicate, Formatter}|Fs]) ->
    case Predicate(Db) of
        'true' -> Formatter(Db, Encoding);
        'false' -> format_db(Db, Encoding, Fs)
    end.

-spec get_all_account_mods() -> kz_term:ne_binaries().
get_all_account_mods() ->
    get_all_account_mods(?REPLICATE_ENCODING).

-spec get_all_account_mods(kz_util:account_format()) -> kz_term:ne_binaries().
get_all_account_mods(Encoding) ->
    {'ok', Databases} = kz_datamgr:db_info(),
    [kzs_util:format_account_modb(Db, Encoding)
     || Db <- Databases,
        is_account_mod(Db)
    ].

-spec get_account_mods(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_account_mods(Account) ->
    get_account_mods(Account, ?REPLICATE_ENCODING).

-spec get_account_mods(kz_term:ne_binary(), kz_util:account_format()) -> kz_term:ne_binaries().
get_account_mods(Account, Encoding) ->
    AccountId = kzs_util:format_account_id(Account, Encoding),
    [MOD
     || MOD <- get_all_account_mods(Encoding),
        is_account_mod(MOD),
        is_matched_account_mod(Encoding, MOD, AccountId)
    ].

-spec is_matched_account_mod(kz_util:account_format(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_matched_account_mod('unencoded'
                      ,?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, _, _)
                      ,?MATCH_ACCOUNT_UNENCODED(A, B, Rest)
                      ) ->
    'true';
is_matched_account_mod('encoded'
                      ,?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _, _)
                      ,?MATCH_ACCOUNT_ENCODED(A, B, Rest)
                      ) ->
    'true';
is_matched_account_mod('raw'
                      ,?MATCH_MODB_SUFFIX_RAW(A, B, Rest, _, _)
                      ,?MATCH_ACCOUNT_RAW(A, B, Rest)
                      ) ->
    'true';
is_matched_account_mod(_, _, _) ->
    'false'.

-spec is_account_mod(kz_term:ne_binary()) -> boolean().
is_account_mod(Db) ->
    kz_datamgr:db_classification(Db) =:= 'modb'.

-spec is_account_db(kz_term:ne_binary()) -> boolean().
is_account_db(Db) ->
    kz_datamgr:db_classification(Db) =:= 'account'.


-type getby_return() :: {'ok', kz_term:ne_binary()} |
                        {'multiples', kz_term:ne_binaries()} |
                        {'error', 'not_found'}.

%%------------------------------------------------------------------------------
%% @doc Get the account db by realm. Realms are one->one with accounts.
%% @end
%%------------------------------------------------------------------------------
-spec get_account_by_realm(kz_term:ne_binary()) -> getby_return().
get_account_by_realm(RawRealm) ->
    Realm = kz_term:to_lower_binary(RawRealm),
    get_accounts_by(Realm, ?ACCT_BY_REALM_CACHE(Realm), ?AGG_LIST_BY_REALM).

-spec get_ccvs_by_ip(kz_term:ne_binary()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
get_ccvs_by_ip(IP) ->
    case kz_cache:peek_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP)) of
        {'ok', {'error', 'not_found'}=E} -> E;
        {'error', 'not_found'} -> do_get_ccvs_by_ip(IP);
        {'ok', _AccountCCVs} = OK -> OK
    end.

-spec do_get_ccvs_by_ip(kz_term:ne_binary()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
do_get_ccvs_by_ip(IP) ->
    case kapps_config:get_is_true(<<"registrar">>, <<"use_aggregate">>, 'true')
        andalso kz_datamgr:get_results(?KZ_SIP_DB, ?AGG_LIST_BY_IP, [{'key', IP}])
    of
        'false' ->
            NotF = {'error', 'not_found'},
            lager:debug("aggregate SIP auth db disabled, skipping CCV lookup by IP ~s", [IP]),
            kz_cache:store_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP), NotF),
            NotF;
        {'ok', []} ->
            NotF = {'error', 'not_found'},
            lager:debug("no entry in ~s for IP: ~s", [?KZ_SIP_DB, IP]),
            kz_cache:store_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP), NotF),
            NotF;
        {'ok', [Doc|_]} ->
            lager:debug("found IP ~s in db ~s (~s)", [IP, ?KZ_SIP_DB, kz_doc:id(Doc)]),
            AccountCCVs = account_ccvs_from_ip_auth(Doc),
            kz_cache:store_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP), AccountCCVs),
            {'ok', AccountCCVs};
        {'error', _E} = Error ->
            lager:debug("error looking up by IP: ~s: ~p", [IP, _E]),
            Error
    end.

-spec account_ccvs_from_ip_auth(kz_json:object()) -> kz_term:proplist().
account_ccvs_from_ip_auth(Doc) ->
    AccountId = kz_json:get_value([<<"value">>, <<"account_id">>], Doc),
    OwnerId = kz_json:get_value([<<"value">>, <<"owner_id">>], Doc),
    AuthType = kz_json:get_value([<<"value">>, <<"authorizing_type">>], Doc, <<"anonymous">>),

    case are_all_enabled([{<<"account">>, AccountId}
                         ,{<<"owner">>, OwnerId}
                         ,{AuthType, kz_doc:id(Doc)}
                         ])
    of
        {'false', _Reason} ->
            lager:notice("no IP auth info: ~p", [_Reason]),
            [];
        'true' ->
            props:filter_undefined(
              [{<<"Account-ID">>, AccountId}
              ,{<<"Owner-ID">>, OwnerId}
              ,{<<"Authorizing-ID">>, kz_doc:id(Doc)}
              ,{<<"Inception">>, <<"onnet">>}
              ,{<<"Authorizing-Type">>, AuthType}
              ])
    end.

-type not_enabled_error() :: 'device_disabled' |
                             'owner_disabled' |
                             'account_disabled'.
-spec are_all_enabled(kz_term:proplist()) ->
          'true' |
          {'false', {not_enabled_error(), kz_term:ne_binary()}}.
are_all_enabled(Things) ->
    ?MATCH_ACCOUNT_RAW(AccountId) = props:get_value(<<"account">>, Things),
    try lists:all(fun(Thing) -> is_enabled(AccountId, Thing) end, Things)
    catch
        'throw':{'error', Reason} -> {'false', Reason}
    end.

-spec is_enabled(kz_term:ne_binary(), {kz_term:ne_binary(), kz_term:api_ne_binary()}) -> boolean().
is_enabled(_AccountId, {_Type, 'undefined'}) -> 'true';
is_enabled(AccountId, {<<"device">>, DeviceId}) ->
    Default = kapps_config:get_is_true(<<"registrar">>, <<"device_enabled_default">>, 'true'),
    {'ok', DeviceJObj} = kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), DeviceId),
    kzd_devices:enabled(DeviceJObj, Default)
        orelse throw({'error', {'device_disabled', DeviceId}});
is_enabled(AccountId, {<<"owner">>, OwnerId}) ->
    case kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), OwnerId) of
        {'ok', UserJObj} ->
            Default = kapps_config:get_is_true(<<"registrar">>, <<"owner_enabled_default">>, 'true'),
            kzd_users:enabled(UserJObj, Default)
                orelse throw({'error', {'owner_disabled', OwnerId}});
        {'error', _R} ->
            lager:debug("unable to fetch owner doc ~s: ~p", [OwnerId, _R]),
            'true'
    end;
is_enabled(AccountId, {<<"account">>, AccountId}) ->
    kzd_accounts:is_enabled(AccountId)
        orelse throw({'error', {'account_disabled', AccountId}});
is_enabled(_AccountId, {_Type, _Thing}) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Get account db by account's name. Names are one->many with accounts
%% since account names are not unique.
%% @end
%%------------------------------------------------------------------------------
-spec get_accounts_by_name(kz_term:ne_binary()) -> getby_return().
get_accounts_by_name(Name) ->
    get_accounts_by(Name, ?ACCT_BY_NAME_CACHE(Name), ?AGG_LIST_BY_NAME).

-spec get_accounts_by(kz_term:ne_binary(), tuple(), kz_term:ne_binary()) -> getby_return().
get_accounts_by(What, CacheKey, View) ->
    case kz_cache:peek_local(?KAPPS_GETBY_CACHE, CacheKey) of
        {'ok', [AccountDb]} -> {'ok', AccountDb};
        {'ok', [_|_]=AccountDbs} -> {'multiples', AccountDbs};
        {'error', 'not_found'} ->
            do_get_accounts_by(What, CacheKey, View)
    end.

-spec do_get_accounts_by(kz_term:ne_binary(), tuple(), kz_term:ne_binary()) -> getby_return().
do_get_accounts_by(What, CacheKey, View) ->
    ViewOptions = [{'key', What}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, View, ViewOptions) of
        {'ok', [JObj]} ->
            AccountDb = kz_json:get_value([<<"value">>, <<"account_db">>], JObj),
            _ = cache(CacheKey, [AccountDb]),
            {'ok', AccountDb};
        {'ok', [_|_]=JObjs} ->
            AccountDbs = [kz_json:get_value([<<"value">>, <<"account_db">>], JObj) || JObj <- JObjs],
            _ = cache(CacheKey, AccountDbs),
            {'multiples', AccountDbs};
        {'ok', []} ->
            {'error', 'not_found'};
        _E ->
            lager:debug("error while fetching ~s: ~p", [View, _E]),
            {'error', 'not_found'}
    end.

-spec cache(tuple(), kz_term:ne_binaries()) -> 'ok'.
cache(Key, AccountDbs) ->
    kz_cache:store_local(?KAPPS_GETBY_CACHE, Key, AccountDbs, ?GET_BY_CACHE_ORIGIN).

%%------------------------------------------------------------------------------
%% @doc Given an JSON Object for a hangup event, or bridge completion
%% this returns the cause and code for the call termination.
%% @end
%%------------------------------------------------------------------------------
-spec get_call_termination_reason(kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_call_termination_reason(JObj) ->
    {kz_call_event:application_response(JObj, kz_call_event:hangup_cause(JObj, <<"UNSPECIFIED">>))
    ,kz_call_event:hangup_code(JObj, <<"sip:600">>)
    }.

%%------------------------------------------------------------------------------
%% @doc Reads all view files from given `Folder' in the given `App'.
%% @end
%%------------------------------------------------------------------------------
-spec get_views_json(atom(), string()) -> kz_datamgr:views_listing().
get_views_json(App, Folder) ->
    Pattern = filename:join([code:priv_dir(App), "couchdb", Folder, "*.json"]),
    [ViewListing
     || File <- filelib:wildcard(Pattern),
        {?NE_BINARY,_}=ViewListing <- [catch get_view_json(File)]
    ].

-spec get_view_json(atom(), kz_term:text()) -> kz_datamgr:view_listing().
get_view_json(App, File) ->
    Path = filename:join([code:priv_dir(App), "couchdb", File]),
    get_view_json(Path).

-spec get_view_json(kz_term:text()) -> kz_datamgr:view_listing().
get_view_json(Path) ->
    lager:debug("fetching view from ~s", [Path]),
    {'ok', Bin} = file:read_file(Path),
    JObj = kz_json:decode(Bin),
    {kz_doc:id(JObj), JObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_aggregate_device(kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
add_aggregate_device(_, 'undefined') -> 'ok';
add_aggregate_device(Db, Device) ->
    DeviceId = kz_doc:id(Device),
    _ = case kz_datamgr:lookup_doc_rev(?KZ_SIP_DB, DeviceId) of
            {'ok', Rev} ->
                lager:debug("aggregating device ~s/~s", [Db, DeviceId]),
                kz_datamgr:ensure_saved(?KZ_SIP_DB, kz_doc:set_revision(Device, Rev));
            {'error', 'not_found'} ->
                lager:debug("aggregating device ~s/~s", [Db, DeviceId]),
                kz_datamgr:ensure_saved(?KZ_SIP_DB, kz_doc:delete_revision(Device))
        end,
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rm_aggregate_device(kz_term:ne_binary(), kz_term:api_object() | kz_term:api_binary()) -> 'ok'.
rm_aggregate_device(_, 'undefined') -> 'ok';
rm_aggregate_device(Db, DeviceId) when is_binary(DeviceId) ->
    case kz_datamgr:open_doc(?KZ_SIP_DB, DeviceId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', JObj} ->
            lager:debug("removing aggregated device ~s/~s", [Db, DeviceId]),
            _ = kz_datamgr:del_doc(?KZ_SIP_DB, JObj),
            'ok'
    end;
rm_aggregate_device(Db, Device) ->
    rm_aggregate_device(Db, kz_doc:id(Device)).

%%------------------------------------------------------------------------------
%% @doc Extracts the User and Realm from either the Request or To field, configured
%% in the `system_config' DB. Defaults to Request (To is the other option).
%% @end
%%------------------------------------------------------------------------------
-spec get_destination(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
get_destination(JObj, Cat, Key) ->
    case kapps_config:get(Cat, Key, <<"Request">>) of
        <<"To">> ->
            get_destination(JObj, [<<"To">>, <<"Request">>]);
        _Else ->
            get_destination(JObj, [<<"Request">>, <<"To">>])
    end.

-spec get_destination(kz_json:object(), kz_term:ne_binaries()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
get_destination(JObj, [Key|Keys]) ->
    case maybe_split(Key, JObj) of
        [User,Realm] -> {User,Realm};
        'undefined' -> get_destination(JObj, Keys)
    end;
get_destination(JObj, []) ->
    {kz_json:get_value(<<"To-DID">>, JObj)
    ,kz_json:get_value(<<"To-Realm">>, JObj)
    }.

%%------------------------------------------------------------------------------
%% @doc Extracts the User and Realm from either the field configured
%% in the `system_config' DB or the "From" field. Defaults to "From"
%% @end
%%------------------------------------------------------------------------------
-spec get_origination(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
get_origination(JObj, Cat, Key) ->
    case kapps_config:get(Cat, Key, <<"From">>) of
        <<"From">> ->
            get_origination(JObj, [<<"From">>]);
        Else ->
            get_origination(JObj, [Else, <<"From">>])
    end.

-spec get_origination(kz_json:object(), kz_term:ne_binaries()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
get_origination(JObj, [Key|Keys]) ->
    case maybe_split(Key, JObj) of
        [User,Realm] ->
            {User,Realm};
        'undefined' ->
            get_origination(JObj, Keys)
    end;
get_origination(_JObj, []) ->
    {'undefined', 'undefined'}.

maybe_split(Key, JObj) ->
    case kz_json:get_ne_binary_value(Key, JObj) of
        undefined -> undefined;
        <<"nouser@",_/binary>> -> undefined;
        Bin -> binary:split(Bin, <<"@">>)
    end.

-spec write_tts_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' |
          {'error', file:posix() | 'badarg' | 'terminated'}.
write_tts_file(Path, Say) ->
    lager:debug("trying to save TTS media to ~s", [Path]),
    {'ok', _, Wav} = kazoo_tts:create(Say),
    file:write_file(Path, Wav).

-spec to_magic_hash(iodata()) -> kz_term:ne_binary().
to_magic_hash(Bin) ->
    kz_term:to_hex_binary(zlib:zip(Bin)).

-spec from_magic_hash(kz_term:ne_binary()) -> kz_term:ne_binary().
from_magic_hash(Bin) ->
    zlib:unzip(kz_binary:from_hex(Bin)).

-spec get_application() -> atom().
get_application() ->
    case get('application') of
        'undefined' -> find_application();
        Application -> Application
    end.

-spec find_application() -> atom().
find_application() ->
    case application:get_application() of
        {'ok', Application} ->
            Application;
        _Else -> 'undefined'
    end.

-spec put_application(atom()) -> atom().
put_application(Application) ->
    put('application', Application).

-spec epmd_enabled() -> boolean().
epmd_enabled() ->
    init:get_argument('start_epmd') =/= {'ok', [["false"]]}.

-spec epmd_disabled() -> boolean().
epmd_disabled() ->
    init:get_argument('start_epmd') =:= {'ok', [["false"]]}.
