%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Utilities shared by a subset of whapps
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whapps_util).

-export([update_all_accounts/1]).
-export([replicate_from_accounts/2, replicate_from_account/3]).
-export([revise_whapp_views_in_accounts/1]).
-export([get_all_accounts/0
         ,get_all_accounts/1
         ,get_all_accounts_and_mods/0
         ,get_all_accounts_and_mods/1
         ,get_all_account_mods/0
         ,get_all_account_mods/1
        ]).
-export([is_account_db/1
        ,is_account_mod/1
        ]).
-export([get_account_by_realm/1,get_accounts_by_name/1]).
-export([get_master_account_id/0]).
-export([find_oldest_doc/1]).
-export([get_event_type/1, put_callid/1]).
-export([get_call_termination_reason/1]).
-export([get_view_json/1, get_view_json/2]).
-export([get_views_json/2]).
-export([update_views/2, update_views/3]).
-export([add_aggregate_device/2]).
-export([rm_aggregate_device/2]).
-export([get_destination/3]).
-export([get_prompt/2, get_prompt/3]).
-export([amqp_pool_send/2]).
-export([amqp_pool_request/3, amqp_pool_request/4
         ,amqp_pool_request_custom/4, amqp_pool_request_custom/5
         ,amqp_pool_collect/2, amqp_pool_collect/3
         ,amqp_pool_collect/4
        ]).
-export([write_tts_file/2]).
-export([decr_timeout/2]).

-include("whistle_apps.hrl").

-define(REPLICATE_ENCODING, 'encoded').
-define(AGG_LIST_BY_REALM, <<"accounts/listing_by_realm">>).
-define(AGG_LIST_BY_NAME, <<"accounts/listing_by_name">>).
-define(PROMPTS_CONFIG_CAT, <<"prompts">>).

-define(ACCT_BY_NAME_CACHE(Name), {?MODULE, 'account_by_name', Name}).
-define(ACCT_BY_REALM_CACHE(Name), {?MODULE, 'account_by_realm', Name}).

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
                          timer:sleep(2000),
                          couch_mgr:revise_doc_from_file(AccountDb, 'crossbar', File)
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
                          timer:sleep(2000),
                          couch_mgr:revise_views_from_folder(AccountDb, App)
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
                          timer:sleep(2000),
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
    ReplicateProps = [{<<"source">>, wh_util:format_account_id(AccountDb, ?REPLICATE_ENCODING)}
                      ,{<<"target">>, TargetDb}
                      ,{<<"filter">>, FilterDoc}
                      ,{<<"create_target">>, 'true'}
                     ],
    try couch_mgr:db_replicate(ReplicateProps) of
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
    case whapps_config:get(?WH_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>) of
        'undefined' ->
            R = couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_id">>, ['include_docs']),
            get_master_account_id(R);
        Default -> {'ok', Default}
    end.

get_master_account_id({'error', _}=E) -> E;
get_master_account_id({'ok', []}) -> {'error', 'no_accounts'};
get_master_account_id({'ok', Accounts}) ->
    {'ok', OldestAccountId}=Ok = find_oldest_doc([wh_json:get_value(<<"doc">>, Account)
                                                  || Account <- Accounts
                                                 ]),
    lager:debug("setting ~s.master_account_id to ~s", [?WH_SYSTEM_CONFIG_ACCOUNT, OldestAccountId]),
    {'ok', _} = whapps_config:set(?WH_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>, OldestAccountId),
    Ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a list of accounts this returns the id of the oldest
%% @end
%%--------------------------------------------------------------------
-spec find_oldest_doc(wh_json:objects()) ->
                             {'ok', ne_binary()} |
                             {'error', 'no_docs'}.
find_oldest_doc([]) -> {'error', 'no_docs'};
find_oldest_doc([First|Docs]) ->
    {_, OldestDocID} =
        lists:foldl(fun(Doc, {Created, _}=Eldest) ->
                            Older = wh_json:get_integer_value(<<"pvt_created">>, Doc),
                            case Older < Created  of
                                'true' -> {Older, wh_json:get_value(<<"_id">>, Doc)};
                                'false' -> Eldest
                            end
                    end
                    ,{wh_json:get_integer_value(<<"pvt_created">>, First)
                      ,wh_json:get_value(<<"_id">>, First)
                     }
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
    {'ok', Databases} = couch_mgr:db_info(),
    [wh_util:format_account_id(Db, Encoding) || Db <- Databases, is_account_db(Db)].

get_all_accounts_and_mods() -> get_all_accounts_and_mods(?REPLICATE_ENCODING).

get_all_accounts_and_mods(Encoding) ->
    {'ok', Databases} = couch_mgr:db_info(),
    [wh_util:format_account_id(Db, Encoding) || Db <- Databases, is_account_db(Db) orelse is_account_mod(Db)].

get_all_account_mods() -> get_all_account_mods(?REPLICATE_ENCODING).

get_all_account_mods(Encoding) ->
    {'ok', Databases} = couch_mgr:db_info(),
    [wh_util:format_account_id(Db, Encoding) || Db <- Databases, is_account_mod(Db)].
                                                 
-spec is_account_mod(ne_binary()) -> boolean().
is_account_mod(<<"account/", _AccountId:34/binary, "-", _Date:6/binary>>) -> 'true';
is_account_mod(<<"account%2F", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'true';
is_account_mod(_) -> 'false'. 

-spec is_account_db(ne_binary()) -> boolean().
is_account_db(<<"account/", _AccountId:34/binary, "-", _Date:6/binary>>) -> 'false';
is_account_db(<<"account%2F", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'false'; 
is_account_db(<<"account/", _/binary>>) -> 'true';
is_account_db(<<"account%2f", _/binary>>) -> 'true';
is_account_db(<<"account%2F", _/binary>>) -> 'true';
is_account_db(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc Realms are one->one with accounts.
%% @end
%%--------------------------------------------------------------------
-spec get_account_by_realm(ne_binary()) ->
                                  {'ok', wh_json:key()} |
                                  {'multiples', wh_json:key()} |
                                  {'error', 'not_found'}.
get_account_by_realm(RawRealm) ->
    Realm = wh_util:to_lower_binary(RawRealm),
    case wh_cache:peek(?ACCT_BY_REALM_CACHE(Realm)) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_LIST_BY_REALM, [{'key', Realm}]) of
                {'ok', [JObj]} ->
                    AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], JObj),
                    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
                    CacheProps = [{'expires', 86400}
                                  ,{'origin', {'db', AccountDb, AccountId}}
                                 ],
                    wh_cache:store(?ACCT_BY_REALM_CACHE(Realm), {'ok', AccountDb}, CacheProps),
                    {'ok', AccountDb};
                {'ok', []} ->
                    {'error', 'not_found'};
                {'ok', [_|_]=JObjs} ->
                    AccountDbs = [wh_json:get_value([<<"value">>, <<"account_db">>], JObj) || JObj <- JObjs],
                    CacheProps = [{'expires', 86400}
                                  ,{'origin', [{'db', AccountDb, wh_util:format_account_id(AccountDb, 'raw')}
                                               || AccountDb <- AccountDbs
                                              ]}
                                 ],
                    wh_cache:store(?ACCT_BY_REALM_CACHE(Realm), {'multiples', AccountDbs}, CacheProps),
                    {'multiples', AccountDbs};
                _E ->
                    lager:debug("error while fetching accounts by realm: ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Names are one->many with accounts since account names are not
%% unique.
%% @end
%%--------------------------------------------------------------------
-spec get_accounts_by_name(ne_binary()) ->
                                  {'ok', wh_json:key()} |
                                  {'multiples', wh_json:key()} |
                                  {'error', 'not_found'}.
get_accounts_by_name(Name) ->
    case wh_cache:peek(?ACCT_BY_NAME_CACHE(Name)) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_LIST_BY_NAME, [{'key', Name}]) of
                {'ok', [JObj]} ->
                    AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], JObj),
                    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
                    CacheProps = [{'expires', 86400}
                                  ,{'origin', {'db', AccountDb, AccountId}}
                                 ],
                    wh_cache:store(?ACCT_BY_NAME_CACHE(Name), {'ok', AccountDb}, CacheProps),
                    {'ok', AccountDb};
                {'ok', []} -> {'error', 'not_found'};
                {'ok', [_|_]=JObjs} ->
                    AccountDbs = [wh_json:get_value([<<"value">>, <<"account_db">>], JObj) || JObj <- JObjs],
                    CacheProps = [{'expires', 86400}
                                  ,{'origin', [{'db', AccountDb, wh_util:format_account_id(AccountDb, 'raw')}
                                               || AccountDb <- AccountDbs
                                              ]}
                                 ],
                    wh_cache:store(?ACCT_BY_NAME_CACHE(Name), {'multiples', AccountDbs}, CacheProps),
                    {'multiples', AccountDbs};
                _E ->
                    lager:debug("error while fetching accounts by name: ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an API JSON object extract the category and name into a
%% tuple for easy processing
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(wh_json:object()) -> {ne_binary(), ne_binary()}.
get_event_type(JObj) -> wh_util:get_event_type(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object extracts the Call-ID into the processes
%% dictionary, failing that the Msg-ID and finally a generic
%% @end
%%--------------------------------------------------------------------
-spec put_callid(wh_json:object()) -> api_binary().
put_callid(JObj) -> wh_util:put_callid(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object for a hangup event, or bridge completion
%% this returns the cause and code for the call termination
%% @end
%%--------------------------------------------------------------------
-spec get_call_termination_reason(wh_json:object()) -> {ne_binary(), ne_binary()}.
get_call_termination_reason(JObj) ->
    Cause = case wh_json:get_ne_value(<<"Application-Response">>, JObj) of
                'undefined' ->
                    wh_json:get_ne_value(<<"Hangup-Cause">>, JObj, <<"UNSPECIFIED">>);
                Response ->
                    Response
            end,
    Code = wh_json:get_value(<<"Hangup-Code">>, JObj, <<"sip:600">>),
    {Cause, Code}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_views_json(atom(), string()) -> wh_json:objects().
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
-spec get_view_json(atom(), text()) -> {ne_binary(), wh_json:object()}.
-spec get_view_json(text()) -> {ne_binary(), wh_json:object()}.

get_view_json(App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", File]),
    get_view_json(Path).

get_view_json(Path) ->
    lager:debug("fetch view from ~s", [Path]),
    {'ok', Bin} = file:read_file(Path),
    JObj = wh_json:decode(Bin),
    {wh_json:get_value(<<"_id">>, JObj), JObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_views(ne_binary(), wh_proplist()) -> 'ok'.
-spec update_views(ne_binary(), wh_proplist(), boolean()) -> 'ok'.
-spec update_views(wh_json:objects(), ne_binary(), wh_proplist(), boolean()) -> 'ok'.

update_views(Db, Views) ->
    update_views(Db, Views, 'false').

update_views(Db, Views, Remove) ->
    case couch_mgr:all_design_docs(Db, ['include_docs']) of
        {'ok', Found} -> update_views(Found, Db, Views, Remove);
        {'error', _R} ->
            lager:debug("unable to fetch current design docs: ~p", [_R])
    end.

update_views([], _, [], _) -> 'ok';
update_views([], Db, [{Id,View}|Views], Remove) ->
    lager:debug("adding view '~s' to '~s'", [Id, Db]),
    _ = couch_mgr:ensure_saved(Db, View),
    update_views([], Db, Views, Remove);
update_views([Found|Finds], Db, Views, Remove) ->
    Id = wh_json:get_value(<<"id">>, Found),
    Doc = wh_json:get_value(<<"doc">>, Found),
    RawDoc = wh_json:delete_key(<<"_rev">>, Doc),
    case props:get_value(Id, Views) of
        'undefined' when Remove ->
            lager:debug("removing view '~s' from '~s'", [Id, Db]),
            _ = couch_mgr:del_doc(Db, Doc),
            update_views(Finds, Db, props:delete(Id, Views), Remove);
        'undefined' ->
            update_views(Finds, Db, props:delete(Id, Views), Remove);
        View1 when View1 =:= RawDoc ->
            lager:debug("view '~s' matches the raw doc, skipping", [Id]),
            update_views(Finds, Db, props:delete(Id, Views), Remove);
        View2 ->
            lager:debug("updating view '~s' in '~s'", [Id, Db]),
            Rev = wh_json:get_value(<<"_rev">>, Doc),
            _ = couch_mgr:ensure_saved(Db, wh_json:set_value(<<"_rev">>, Rev, View2)),
            update_views(Finds, Db, props:delete(Id, Views), Remove)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_aggregate_device(ne_binary(), api_binary()) -> 'ok'.
add_aggregate_device(_, 'undefined') -> 'ok';
add_aggregate_device(Db, Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),
    _ = case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
            {'ok', Rev} ->
                lager:debug("aggregating device ~s/~s", [Db, DeviceId]),
                couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Device));
            {'error', 'not_found'} ->
                lager:debug("aggregating device ~s/~s", [Db, DeviceId]),
                couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Device))
        end,
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rm_aggregate_device(ne_binary(), api_object() | api_binary()) -> 'ok'.
rm_aggregate_device(_, 'undefined') -> 'ok';
rm_aggregate_device(Db, DeviceId) when is_binary(DeviceId) ->
    case couch_mgr:open_doc(?WH_SIP_DB, DeviceId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', JObj} ->
            lager:debug("removing aggregated device ~s/~s", [Db, DeviceId]),
            _ = couch_mgr:del_doc(?WH_SIP_DB, JObj),
            'ok'
    end;
rm_aggregate_device(Db, Device) ->
    rm_aggregate_device(Db, wh_json:get_value(<<"_id">>, Device)).

-spec amqp_pool_send(api_terms(), wh_amqp_worker:publish_fun()) ->
                            'ok' | {'error', any()}.
amqp_pool_send(Api, PubFun) when is_function(PubFun, 1) ->
    wh_amqp_worker:cast(?WHAPPS_AMQP_POOL, Api, PubFun).

-spec amqp_pool_request(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun()) ->
                               {'ok', wh_json:object()} |
                               {'error', any()}.
-spec amqp_pool_request(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), wh_timeout()) ->
                               {'ok', wh_json:object()} |
                               {'error', any()}.
amqp_pool_request(Api, PubFun, ValidateFun)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    amqp_pool_request(Api, PubFun, ValidateFun, wh_amqp_worker:default_timeout()).
amqp_pool_request(Api, PubFun, ValidateFun, Timeout)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    wh_amqp_worker:call(?WHAPPS_AMQP_POOL, Api, PubFun, ValidateFun, Timeout).

-spec amqp_pool_request_custom(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), gen_listener:binding()) ->
                               {'ok', wh_json:object()} |
                               {'error', any()}.
-spec amqp_pool_request_custom(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:validate_fun(), wh_timeout(), gen_listener:binding()) ->
                               {'ok', wh_json:object()} |
                               {'error', any()}.
amqp_pool_request_custom(Api, PubFun, ValidateFun, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1) ->
    amqp_pool_request_custom(Api, PubFun, ValidateFun, wh_amqp_worker:default_timeout(), Bind).
amqp_pool_request_custom(Api, PubFun, ValidateFun, Timeout, Bind)
  when is_function(PubFun, 1),
       is_function(ValidateFun, 1),
       ((is_integer(Timeout) andalso Timeout >= 0)
        orelse Timeout =:= 'infinity') ->
    wh_amqp_worker:call_custom(?WHAPPS_AMQP_POOL, Api, PubFun, ValidateFun, Timeout, Bind).

-spec amqp_pool_collect(api_terms(), wh_amqp_worker:publish_fun()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun) ->
    amqp_pool_collect(Api, PubFun, wh_amqp_worker:default_timeout()).

-spec amqp_pool_collect(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:timeout_or_until()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun, TimeoutOrUntil) ->
    wh_amqp_worker:call_collect(?WHAPPS_AMQP_POOL, Api, PubFun, TimeoutOrUntil).

-spec amqp_pool_collect(api_terms(), wh_amqp_worker:publish_fun(), wh_amqp_worker:collect_until(), wh_timeout()) ->
                               {'ok', wh_json:objects()} |
                               {'timeout', wh_json:objects()} |
                               {'error', any()}.
amqp_pool_collect(Api, PubFun, Until, Timeout) ->
    wh_amqp_worker:call_collect(?WHAPPS_AMQP_POOL, Api, PubFun, Until, Timeout).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Extracts the User and Realm from either the Request or To field, configured
%% in the system_config DB. Defaults to Request (To is the other option)
%% @end
%%--------------------------------------------------------------------
-spec get_destination(wh_json:object(), ne_binary(), ne_binary()) ->
                             {ne_binary(), ne_binary()}.
get_destination(JObj, Cat, Key) ->
    case whapps_config:get(Cat, Key, <<"Request">>) of
        <<"To">> ->
            case try_split(<<"To">>, JObj) of
                {_,_}=UserRealm -> UserRealm;
                'undefined' ->
                    case try_split(<<"Request">>, JObj) of
                        {_,_}=UserRealm -> UserRealm;
                        'undefined' ->
                            {wh_json:get_value(<<"To-DID">>, JObj)
                             ,wh_json:get_value(<<"To-Realm">>, JObj)
                            }
                    end
            end;
        _ ->
            case try_split(<<"Request">>, JObj) of
                {_,_}=UserRealm -> UserRealm;
                'undefined' ->
                    case try_split(<<"To">>, JObj) of
                        {_,_}=UserRealm -> UserRealm;
                        'undefined' ->
                            {wh_json:get_value(<<"To-DID">>, JObj)
                             ,wh_json:get_value(<<"To-Realm">>, JObj)
                            }
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_prompt(ne_binary(), 'undefined' | whapps_call:call()) -> ne_binary().
-spec get_prompt(ne_binary(), ne_binary(), 'undefined' | whapps_call:call()) -> ne_binary().

get_prompt(Name, Call) ->
    get_prompt(Name, <<"en">>, Call).

get_prompt(Name, Lang, 'undefined') ->
    whapps_config:get(?PROMPTS_CONFIG_CAT, [Lang, Name], <<"/system_media/", Name/binary>>);
get_prompt(Name, Lang, Call) ->
    DefaultPrompt = whapps_config:get(?PROMPTS_CONFIG_CAT, [Lang, Name], <<"/system_media/", Name/binary>>),
    JObj = whapps_account_config:get(whapps_call:account_id(Call), ?PROMPTS_CONFIG_CAT),
    wh_json:get_value([Lang, Name], JObj, DefaultPrompt).

-spec try_split(ne_binary(), wh_json:object()) ->
                       {ne_binary(), ne_binary()} |
                       'undefined'.
try_split(Key, JObj) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' -> 'undefined';
        Bin when is_binary(Bin) ->
            case binary:split(Bin, <<"@">>) of
                [<<"nouser">>, _] -> 'undefined';
                [_, _]=Dest -> list_to_tuple(Dest)
            end
    end.

-spec write_tts_file(ne_binary(), ne_binary()) ->
                            'ok' |
                            {'error', file:posix() | 'badarg' | 'terminated'}.
write_tts_file(Path, Say) ->
    lager:debug("trying to save TTS media to ~s", [Path]),
    {'ok', _, Wav} = whapps_speech:create(Say),
    file:write_file(Path, Wav).

-spec decr_timeout(wh_timeout(), non_neg_integer() | wh_now()) -> wh_timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, Elapsed) when is_integer(Elapsed) -> Timeout - Elapsed;
decr_timeout(Timeout, Start) -> decr_timeout(Timeout, wh_util:elapsed_ms(Start)).
