%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Utilities shared by a subset of kapps
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
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
         ,get_account_by_ip/1, get_ccvs_by_ip/1
         ,get_accounts_by_name/1
        ]).
-export([get_master_account_id/0
         ,get_master_account_db/0
        ]).
-export([is_master_account/1]).
-export([account_depth/1]).
-export([account_has_descendants/1]).
-export([get_account_name/1]).
-export([find_oldest_doc/1]).
-export([get_event_type/1]).
-export([get_call_termination_reason/1]).
-export([get_view_json/1, get_view_json/2]).
-export([get_views_json/2]).
-export([update_views/2, update_views/3]).
-export([add_aggregate_device/2]).
-export([rm_aggregate_device/2]).
-export([get_destination/3]).

-export([amqp_pool_send/2]).
-export([amqp_pool_request/3, amqp_pool_request/4
         ,amqp_pool_request_custom/4, amqp_pool_request_custom/5
         ,amqp_pool_collect/2, amqp_pool_collect/3
         ,amqp_pool_collect/4
        ]).

-export([write_tts_file/2]).
-export([to_magic_hash/1
         ,from_magic_hash/1
        ]).

-export([media_local_store_url/2]).
-export([system_report/2, system_report/3]).

-include("kazoo_apps.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(REPLICATE_ENCODING, 'encoded').
-define(AGG_LIST_BY_REALM, <<"accounts/listing_by_realm">>).
-define(AGG_LIST_BY_NAME, <<"accounts/listing_by_name">>).
-define(AGG_LIST_BY_IP, <<"credentials/lookup_by_ip">>).
-define(PROMPTS_CONFIG_CAT, <<"prompts">>).

-define(ACCT_BY_NAME_CACHE(Name), {?MODULE, 'account_by_name', Name}).
-define(ACCT_BY_REALM_CACHE(Name), {?MODULE, 'account_by_realm', Name}).
-define(ACCT_BY_IP_CACHE(IP), {?MODULE, 'account_by_ip', IP}).
-define(GET_BY_CACHE_ORIGIN, [{'origin', [{'db', ?KZ_ACCOUNTS_DB, <<"account">>}]}]).

%%--------------------------------------------------------------------
%% @doc
%% Update a document in each crossbar account database with the
%% file contents.  This is intended for _design docs....
%%
%% @spec update_all_accounts() -> ok | error
%% @end
%%--------------------------------------------------------------------
-spec update_all_accounts(ne_binary()) -> 'ok'.
update_all_accounts(File) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          kz_datamgr:revise_doc_from_file(AccountDb, 'crossbar', File)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will import every .json file found in the given
%% application priv/couchdb/views/ folder into every account
%% @end
%%--------------------------------------------------------------------
-spec revise_whapp_views_in_accounts(atom()) -> 'ok'.
revise_whapp_views_in_accounts(App) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          kz_datamgr:revise_views_from_folder(AccountDb, App)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will replicate the results of the filter from each
%% account db into the target database
%% @end
%%--------------------------------------------------------------------
-spec replicate_from_accounts(ne_binary(), ne_binary()) -> 'ok'.
replicate_from_accounts(TargetDb, FilterDoc) when is_binary(FilterDoc) ->
    lists:foreach(fun(AccountDb) ->
                          timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
                          replicate_from_account(AccountDb, TargetDb, FilterDoc)
                  end, get_all_accounts(?REPLICATE_ENCODING)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will replicate the results of the filter from the
%% source database into the target database
%% @end
%%--------------------------------------------------------------------
-spec replicate_from_account(ne_binary(), ne_binary(), ne_binary()) ->
                                    'ok' | {'error', 'matching_dbs'}.
replicate_from_account(AccountDb, AccountDb, _) ->
    lager:debug("requested to replicate from db ~s to self, skipping", [AccountDb]),
    {'error', 'matching_dbs'};
replicate_from_account(AccountDb, TargetDb, FilterDoc) ->
    ReplicateProps = [{<<"source">>, kz_util:format_account_id(AccountDb, ?REPLICATE_ENCODING)}
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the system admin from the system_config if set, if not
%% set it to the oldest acccount and return that.
%% @end
%%--------------------------------------------------------------------
-spec get_master_account_id() -> {'ok', ne_binary()} |
                                 {'error', atom()}.
get_master_account_id() ->
    case kapps_config:get(?KZ_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>) of
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
    lager:debug("setting ~s.master_account_id to ~s", [?KZ_SYSTEM_CONFIG_ACCOUNT, OldestAccountId]),
    {'ok', _} = kapps_config:set(?KZ_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>, OldestAccountId),
    Ok.

-spec get_master_account_db() -> {'ok', ne_binary()} |
                                 {'error', any()}.
get_master_account_db() ->
    case get_master_account_id() of
        {'error', _}=E -> E;
        {'ok', AccountId} ->
            {'ok', kz_util:format_account_id(AccountId, 'encoded')}
    end.

-spec is_master_account(ne_binary()) -> boolean().
is_master_account(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case get_master_account_id() of
        {'ok', AccountId} -> 'true';
        _Else -> 'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_depth(ne_binary()) -> api(non_neg_integer()).
account_depth(Account) ->
    {'ok', JObj} = kz_account:fetch(Account),
    length(kz_account:tree(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_has_descendants(ne_binary()) -> boolean().
account_has_descendants(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ViewOptions = [{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    {'ok', JObjs} = kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions),
    length([JObj || JObj <- JObjs, kz_account:id(JObj) =/= AccountId]) > 0.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_account_name(ne_binary()) -> ne_binary().
get_account_name(Account) ->
    case kz_account:fetch(Account) of
        {'error', _} -> 'undefined';
        {'ok', JObj} -> kz_account:name(JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a list of accounts this returns the id of the oldest
%% @end
%%--------------------------------------------------------------------
-spec find_oldest_doc(kz_json:objects()) ->
                             {'ok', ne_binary()} |
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a list of all account database names
%% in the requested encoding
%% @end
%%--------------------------------------------------------------------
-spec get_all_accounts() -> ne_binaries().
-spec get_all_accounts('unencoded' | 'encoded' | 'raw') -> ne_binaries().
get_all_accounts() -> get_all_accounts(?REPLICATE_ENCODING).

get_all_accounts(Encoding) ->
    {'ok', Dbs} = kz_datamgr:db_list([{'startkey', <<"account/">>}
                                      ,{'endkey', <<"account/\ufff0">>}
                                     ]),
    [kz_util:format_account_id(Db, Encoding)
     || Db <- Dbs, is_account_db(Db)
    ].

-spec get_all_accounts_and_mods() -> ne_binaries().
-spec get_all_accounts_and_mods('unencoded' | 'encoded' | 'raw') -> ne_binaries().
get_all_accounts_and_mods() ->
    get_all_accounts_and_mods(?REPLICATE_ENCODING).

get_all_accounts_and_mods(Encoding) ->
    {'ok', Databases} = kz_datamgr:db_info(),
    [format_db(Db, Encoding)
     || Db <- Databases,
        is_account_db(Db)
            orelse is_account_mod(Db)
    ].

-spec format_db(ne_binary(), 'unencoded' | 'encoded' | 'raw') -> ne_binary().
format_db(Db, Encoding) ->
    Fs =
        [{fun is_account_db/1, fun kz_util:format_account_id/2}
         ,{fun is_account_mod/1, fun kz_util:format_account_modb/2}
        ],
    format_db(Db, Encoding, Fs).

format_db(Db, Encoding, [{Predicate, Formatter}|Fs]) ->
    case Predicate(Db) of
        'true' -> Formatter(Db, Encoding);
        'false' -> format_db(Db, Encoding, Fs)
    end.

-spec get_all_account_mods() -> ne_binaries().
-spec get_all_account_mods('unencoded' | 'encoded' | 'raw') -> ne_binaries().
get_all_account_mods() ->
    get_all_account_mods(?REPLICATE_ENCODING).

get_all_account_mods(Encoding) ->
    {'ok', Databases} = kz_datamgr:db_info(),
    [kz_util:format_account_modb(Db, Encoding)
     || Db <- Databases,
        is_account_mod(Db)
    ].

-spec get_account_mods(ne_binary()) ->
                              ne_binaries().
-spec get_account_mods(ne_binary(), 'unencoded' | 'encoded' | 'raw') ->
                              ne_binaries().
get_account_mods(AccountId) ->
    get_account_mods(AccountId, ?REPLICATE_ENCODING).

get_account_mods(AccountId, Encoding) ->
    MODs = get_all_account_mods(Encoding),
    [kz_util:format_account_id(MOD, Encoding)
     || MOD <- MODs,
        is_account_mod(MOD),
        is_matched_account_mod(MOD, AccountId)
    ].

-spec is_matched_account_mod(ne_binary(), ne_binary()) -> boolean().
is_matched_account_mod(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, _, _)  %% DbActId
                       ,?MATCH_ACCOUNT_UNENCODED(A, B, Rest)  %% SearchId
                      ) ->
    'true';
is_matched_account_mod(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _, _)  %% DbActId
                       ,?MATCH_ACCOUNT_ENCODED(A, B, Rest)  %% SearchId
                      ) ->
    'true';
is_matched_account_mod(_, _) ->
    'false'.

-spec is_account_mod(ne_binary()) -> boolean().
is_account_mod(Db) -> kz_datamgr:db_classification(Db) =:= 'modb'.

-spec is_account_db(ne_binary()) -> boolean().
is_account_db(Db) -> kz_datamgr:db_classification(Db) =:= 'account'.


-type getby_return() :: {'ok', ne_binary()} |
                        {'multiples', ne_binaries()} |
                        {'error', 'not_found'}.

%%--------------------------------------------------------------------
%% @public
%% @doc Realms are one->one with accounts.
%% @end
%%--------------------------------------------------------------------
-spec get_account_by_realm(ne_binary()) -> getby_return().
get_account_by_realm(RawRealm) ->
    Realm = kz_util:to_lower_binary(RawRealm),
    get_accounts_by(Realm, ?ACCT_BY_REALM_CACHE(Realm), ?AGG_LIST_BY_REALM).

-spec get_account_by_ip(ne_binary()) -> getby_return().
get_account_by_ip(IP) ->
    case get_ccvs_by_ip(IP) of
        {'error', 'not_found'}=E -> E;
        {'ok', AccountCCVs} ->
            {'ok', props:get_value(<<"Account-ID">>, AccountCCVs)}
    end.

-spec get_ccvs_by_ip(ne_binary()) ->
                            {'ok', kz_proplist()} |
                            {'error', 'not_found'}.
get_ccvs_by_ip(IP) ->
    case kz_cache:peek_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP)) of
        {'ok', {'error', 'not_found'}=E} -> E;
        {'error', 'not_found'} -> do_get_ccvs_by_ip(IP);
        {'ok', _AccountCCVs} = OK -> OK
    end.

-spec do_get_ccvs_by_ip(ne_binary()) ->
                               {'ok', kz_proplist()} |
                               {'error', 'not_found'}.
do_get_ccvs_by_ip(IP) ->
    case kz_datamgr:get_results(?KZ_SIP_DB, ?AGG_LIST_BY_IP, [{'key', IP}]) of
        {'ok', []} ->
            lager:debug("no entry in ~s for IP: ~s", [?KZ_SIP_DB, IP]),
            kz_cache:store_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP), {'error', 'not_found'}),
            {'error', 'not_found'};
        {'ok', [Doc|_]} ->
            lager:debug("found IP ~s in db ~s (~s)", [IP, ?KZ_SIP_DB, kz_doc:id(Doc)]),
            AccountCCVs = account_ccvs_from_ip_auth(Doc),
            kz_cache:store_local(?KAPPS_GETBY_CACHE, ?ACCT_BY_IP_CACHE(IP), AccountCCVs),
            {'ok', AccountCCVs};
        {'error', _E} = Error ->
            lager:debug("error looking up by IP: ~s: ~p", [IP, _E]),
            Error
    end.

-spec account_ccvs_from_ip_auth(kz_json:object()) -> kz_proplist().
account_ccvs_from_ip_auth(Doc) ->
    AccountID = kz_json:get_value([<<"value">>, <<"account_id">>], Doc),
    OwnerID = kz_json:get_value([<<"value">>, <<"owner_id">>], Doc),
    AuthType = kz_json:get_value([<<"value">>, <<"authorizing_type">>], Doc, <<"anonymous">>),

    props:filter_undefined(
      [{<<"Account-ID">>, AccountID}
       ,{<<"Owner-ID">>, OwnerID}
       ,{<<"Authorizing-ID">>, kz_doc:id(Doc)}
       ,{<<"Inception">>, <<"on-net">>}
       ,{<<"Authorizing-Type">>, AuthType}
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc Names are one->many with accounts since account names are not
%% unique.
%% @end
%%--------------------------------------------------------------------
-spec get_accounts_by_name(ne_binary()) -> getby_return().
get_accounts_by_name(Name) ->
    get_accounts_by(Name, ?ACCT_BY_NAME_CACHE(Name), ?AGG_LIST_BY_NAME).

-spec get_accounts_by(ne_binary(), tuple(), ne_binary()) -> getby_return().
get_accounts_by(What, CacheKey, View) ->
    case kz_cache:peek_local(?KAPPS_GETBY_CACHE, CacheKey) of
        {'ok', [AccountDb]} -> {'ok', AccountDb};
        {'ok', [_|_]=AccountDbs} -> {'multiples', AccountDbs};
        {'error', 'not_found'} ->
            do_get_accounts_by(What, CacheKey, View)
    end.

-spec do_get_accounts_by(ne_binary(), tuple(), ne_binary()) -> getby_return().
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

-spec cache(tuple(), ne_binaries()) -> 'ok'.
cache(Key, AccountDbs) ->
    kz_cache:store_local(?KAPPS_GETBY_CACHE, Key, AccountDbs, ?GET_BY_CACHE_ORIGIN).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an API JSON object extract the category and name into a
%% tuple for easy processing
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(kz_json:object()) -> {ne_binary(), ne_binary()}.
get_event_type(JObj) -> kz_util:get_event_type(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object for a hangup event, or bridge completion
%% this returns the cause and code for the call termination
%% @end
%%--------------------------------------------------------------------
-spec get_call_termination_reason(kz_json:object()) -> {ne_binary(), ne_binary()}.
get_call_termination_reason(JObj) ->
    Cause = case kz_json:get_ne_value(<<"Application-Response">>, JObj) of
                'undefined' ->
                    kz_json:get_ne_value(<<"Hangup-Cause">>, JObj, <<"UNSPECIFIED">>);
                Response ->
                    Response
            end,
    Code = kz_json:get_value(<<"Hangup-Code">>, JObj, <<"sip:600">>),
    {Cause, Code}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_views_json(atom(), string()) -> kz_json:objects().
get_views_json(App, Folder) ->
    Files = filelib:wildcard(lists:flatten([code:priv_dir(App), "/couchdb/", Folder, "/*.json"])),
    [JObj
     || File <- Files,
        begin
            JObj = (catch(get_view_json(File))),
            case JObj of {'EXIT', _} -> 'false'; _ -> 'true' end
        end
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_view_json(atom(), text()) -> {ne_binary(), kz_json:object()}.
-spec get_view_json(text()) -> {ne_binary(), kz_json:object()}.

get_view_json(App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    get_view_json(Path).

get_view_json(Path) ->
    lager:debug("fetch view from ~s", [Path]),
    {'ok', Bin} = file:read_file(Path),
    JObj = kz_json:decode(Bin),
    {kz_doc:id(JObj), JObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_views(ne_binary(), kz_proplist()) -> 'ok'.
-spec update_views(ne_binary(), kz_proplist(), boolean()) -> 'ok'.

update_views(Db, Views) ->
    update_views(Db, Views, 'false').

update_views(Db, Views, Remove) ->
    kz_datamgr:db_view_update(Db, Views, Remove).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_aggregate_device(ne_binary(), api(binary())) -> 'ok'.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rm_aggregate_device(ne_binary(), api_object() | api(binary())) -> 'ok'.
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

-spec amqp_pool_send(api_terms(), kz_amqp_worker:publish_fun()) ->
                            'ok' | {'error', any()}.
amqp_pool_send(Api, PubFun) when is_function(PubFun, 1) ->
    kz_amqp_worker:cast(Api, PubFun).

-spec amqp_pool_request(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun()) ->
                               kz_amqp_worker:request_return().
-spec amqp_pool_request(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), kz_timeout()) ->
                               kz_amqp_worker:request_return().
amqp_pool_request(Api, PubFun, ValidateFun)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    amqp_pool_request(Api, PubFun, ValidateFun, kz_amqp_worker:default_timeout()).
amqp_pool_request(Api, PubFun, ValidateFun, Timeout)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    kz_amqp_worker:call(Api, PubFun, ValidateFun, Timeout).

-spec amqp_pool_request_custom(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), gen_listener:binding()) ->
                                      kz_amqp_worker:request_return().
-spec amqp_pool_request_custom(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), kz_timeout(), gen_listener:binding()) ->
                                      kz_amqp_worker:request_return().
amqp_pool_request_custom(Api, PubFun, ValidateFun, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    amqp_pool_request_custom(Api, PubFun, ValidateFun, kz_amqp_worker:default_timeout(), Bind).
amqp_pool_request_custom(Api, PubFun, ValidateFun, Timeout, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    kz_amqp_worker:call_custom(Api, PubFun, ValidateFun, Timeout, Bind).

-spec amqp_pool_collect(api_terms(), kz_amqp_worker:publish_fun()) ->
                               {'ok', kz_json:objects()} |
                               {'timeout', kz_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun) ->
    amqp_pool_collect(Api, PubFun, kz_amqp_worker:default_timeout()).

-spec amqp_pool_collect(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:timeout_or_until()) ->
                               {'ok', kz_json:objects()} |
                               {'timeout', kz_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun, TimeoutOrUntil) ->
    kz_amqp_worker:call_collect(Api, PubFun, TimeoutOrUntil).

-spec amqp_pool_collect(api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:collect_until(), kz_timeout()) ->
                               {'ok', kz_json:objects()} |
                               {'timeout', kz_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun, Until, Timeout) ->
    kz_amqp_worker:call_collect(Api, PubFun, Until, Timeout).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Extracts the User and Realm from either the Request or To field, configured
%% in the system_config DB. Defaults to Request (To is the other option)
%% @end
%%--------------------------------------------------------------------
-spec get_destination(kz_json:object(), ne_binary(), ne_binary()) ->
                             {ne_binary(), ne_binary()}.
get_destination(JObj, Cat, Key) ->
    case kapps_config:get(Cat, Key, <<"Request">>) of
        <<"To">> ->
            get_destination(JObj, [<<"To">>, <<"Request">>]);
        _Else ->
            get_destination(JObj, [<<"Request">>, <<"To">>])
    end.

-spec get_destination(kz_json:object(), ne_binaries()) ->
                             {ne_binary(), ne_binary()}.
get_destination(JObj, [Key|Keys]) ->
    case try_split(Key, JObj) of
        {_,_}=UserRealm -> UserRealm;
        'undefined' -> get_destination(JObj, Keys)
    end;
get_destination(JObj, []) ->
    {kz_json:get_value(<<"To-DID">>, JObj)
     ,kz_json:get_value(<<"To-Realm">>, JObj)
    }.

-spec try_split(api(binary())) ->
                       api({ne_binary(), ne_binary()}).
-spec try_split(ne_binary(), kz_json:object()) ->
                       api({ne_binary(), ne_binary()}).
try_split(Key, JObj) ->
    try_split(kz_json:get_value(Key, JObj)).

try_split('undefined') -> 'undefined';
try_split(<<"nouser@", _/binary>>) -> 'undefined';
try_split(<<_/binary>> = Bin) ->
    [_, _] = Dest = binary:split(Bin, <<"@">>),
    list_to_tuple(Dest).

-spec write_tts_file(ne_binary(), ne_binary()) ->
                            'ok' |
                            {'error', file:posix() | 'badarg' | 'terminated'}.
write_tts_file(Path, Say) ->
    lager:debug("trying to save TTS media to ~s", [Path]),
    {'ok', _, Wav} = kapps_speech:create(Say),
    file:write_file(Path, Wav).

-spec to_magic_hash(ne_binary()) -> ne_binary().
to_magic_hash(Bin) ->
    kz_util:to_hex_binary(zlib:zip(Bin)).

-spec from_magic_hash(ne_binary()) -> ne_binary().
from_magic_hash(Bin) ->
    zlib:unzip(kz_util:from_hex_binary(Bin)).

-spec media_local_store_url(kapps_call:call(), kz_json:object()) -> ne_binary().
media_local_store_url(Call, JObj) ->
    AccountDb = kapps_call:account_db(Call),
    MediaId = kz_doc:id(JObj),
    MediaName = kz_json:get_value(<<"name">>, JObj),
    kz_datamgr:attachment_url(AccountDb, MediaId, MediaName).

-spec system_report({text(), text()} | text(), kapps_call:call()) -> 'ok'.
system_report({Subject, Msg}, Call) ->
    system_report(Subject, Msg, Call);
system_report(Msg, Call) ->
    system_report(Msg, Msg, Call).

-spec system_report(text(), text(), kapps_call:call()) -> 'ok'.
system_report(Subject, Msg, Call) ->
    AppName = kapps_call:application_name(Call),
    AppVersion = kapps_call:application_version(Call),
    Notify = props:filter_undefined(
               [{<<"Subject">>, Subject}
                ,{<<"Message">>, iolist_to_binary(Msg)}
                ,{<<"Details">>, kapps_call:to_json(Call)}
                ,{<<"Account-ID">>, kapps_call:account_id(Call)}
                | kz_api:default_headers(AppName, AppVersion)
               ]),
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).
