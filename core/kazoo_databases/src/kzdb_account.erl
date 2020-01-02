%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzdb_account).

-export([create/2
        ,delete/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-export_type([delete_error/0
             ,delete_errors/0
             ]).

-define(ACCOUNTS_CONFIG_CAT, <<"crossbar.accounts">>).

-define(AGG_VIEW_NAME, <<"accounts/listing_by_name">>).

-type delete_error() :: {'error', kz_term:ne_binary(), pos_integer()}.
-type delete_errors() :: [delete_error()].

-spec delete(kz_term:api_ne_binary()) ->
          {'ok', kzd_accounts:doc() | 'undefined'} |
          kz_datamgr:data_error() |
          {'error', delete_errors()}.
delete('undefined') -> {'ok', 'undefined'};
delete(?MATCH_ACCOUNT_RAW(AccountId)) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _}=Error -> Error;
        {'ok', AccountJObj} ->
            lager:info("attempting to delete ~s(~s)", [AccountId, kz_doc:revision(AccountJObj)]),
            delete(AccountId, AccountJObj)
    end.

-spec delete(kz_term:ne_binary(), kzd_accounts:doc()) ->
          {'ok', kzd_accounts:doc()} |
          {'error', delete_errors()}.
delete(_AccountId, AccountJObj) ->
    DeleteRoutines = [fun delete_remove_services/1
                     ,fun delete_free_numbers/1
                     ,fun delete_remove_sip_aggregates/1
                     ,fun delete_aux_services/1
                     ,fun delete_mod_dbs/1
                     ,fun delete_remove_db/1
                     ,fun delete_remove_from_accounts/1
                     ],
    case lists:foldl(fun(F, A) -> F(A) end
                    ,{AccountJObj, []}
                    ,DeleteRoutines
                    )
    of
        {DeletedAccount, []} -> {'ok', DeletedAccount};
        {_AccountJObj, Errors} -> {'error', Errors}
    end.

-type delete_acc() :: {kzd_accounts:doc(), delete_errors()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_remove_services(delete_acc()) -> delete_acc().
delete_remove_services({AccountJObj, Errors}=Acc) ->
    try kz_services:delete(kz_doc:id(AccountJObj)) of
        _S ->
            lager:info("deleted account's services"),
            Acc
    catch
        _E:_R ->
            lager:error("failed to delete services: ~s: ~p", [_E, _R]),
            {AccountJObj, [{'error', <<"unable to cancel services">>, 500} | Errors]}
    end.

-spec delete_free_numbers(delete_acc()) -> delete_acc().
delete_free_numbers({AccountJObj, _Errors}=Acc) ->
    _Freed = knm_numbers:free(kz_doc:id(AccountJObj)),
    lager:debug("freed account's numbers: ~p", [_Freed]),
    Acc.

-spec delete_remove_sip_aggregates(delete_acc()) -> delete_acc().
delete_remove_sip_aggregates({AccountJObj, _Errors}=Acc) ->
    ViewOptions = ['include_docs'
                  ,{'key', kz_doc:id(AccountJObj)}
                  ],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup_by_account">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to clean sip_auth: ~p", [_R]);
        {'ok', JObjs} ->
            Docs = [kz_json:get_json_value(<<"doc">>, JObj) || JObj <- JObjs],
            _ = kz_datamgr:del_docs(?KZ_SIP_DB, Docs),
            lager:debug("deleted ~p docs from ~s", [length(Docs), ?KZ_SIP_DB])
    end,
    Acc.

delete_aux_services({_AccountJObj, _Errors}=Acc) ->
    Acc.

-spec delete_remove_db(delete_acc()) -> delete_acc().
delete_remove_db({AccountJObj, Errors}=Acc) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:db_delete(AccountDb) of
        'true' ->
            lager:debug("deleted db ~s", [AccountDb]),
            Acc;
        'false' ->
            lager:debug("failed to remove database ~s", [AccountDb]),
            {AccountJObj, [{'error', <<"unable to remove database">>, 500} | Errors]}
    end.

-spec delete_mod_dbs(delete_acc()) -> delete_acc().
delete_mod_dbs({AccountJObj, _Errors}=Acc) ->
    MODBs = kapps_util:get_account_mods(kz_doc:id(AccountJObj), 'encoded'),
    _ = [kz_datamgr:db_delete(MODB) || MODB <- MODBs],
    Acc.

-spec delete_remove_from_accounts(delete_acc()) -> delete_acc().
delete_remove_from_accounts({AccountJObj, Errors}=Acc) ->
    case kzd_accounts:fetch(kz_doc:id(AccountJObj), 'accounts') of
        {'ok', AccountsJObj} ->
            _Deleted = kz_datamgr:del_doc(?KZ_ACCOUNTS_DB, AccountsJObj),
            lager:info("deleted 'accounts' account doc: ~p", [_Deleted]),
            Acc;
        {'error', 'not_found'} ->
            lager:info("'accounts' account doc not found"),
            Acc;
        {'error', _R} ->
            lager:info("failed to fetch 'accounts' account doc: ~p", [_R]),
            {AccountJObj, [{'error', <<"unable to remove account definition">>, 500} | Errors]}
    end.

-spec create(kz_term:ne_binary(), kz_json:object()) -> kzd_accounts:doc() | 'undefined'.
create(AccountId, ReqJObj) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kapps_util:is_account_db(AccountDb)
        andalso kz_datamgr:db_create(AccountDb)
    of
        'false' ->
            lager:info("failed to create database: ~s", [AccountDb]),
            'undefined';
        'true' ->
            lager:info("created account database: ~s", [AccountDb]),

            Doc = lists:foldl(fun({F, V}, J) -> F(J, V) end
                             ,ReqJObj
                             ,[{fun kz_doc:set_id/2, AccountId}
                              ,{fun kz_doc:set_account_id/2, AccountId}
                              ,{fun kz_doc:update_pvt_parameters/2, AccountDb}
                              ,{fun kz_doc:set_type/2, kzd_accounts:type()}
                              ]
                             ),
            create(Doc)
    end.

-spec create(kz_json:object()) -> kzd_accounts:doc().
create(ReqJObj) ->
    lists:foldl(fun(F, Req) -> F(Req) end
               ,ReqJObj
               ,[fun create_account_definition/1
                ,fun load_initial_views/1
                ,fun create_account_mod/1
                ,fun reconcile/1
                ,fun create_first_transaction/1
                ,fun set_notification_preference/1
                ,fun add_apps_store_doc/1
                ]
               ).

-spec create_account_definition(kz_json:object()) -> kz_json:object().
create_account_definition(ReqJObj) ->
    JObj = maybe_set_trial_expires(ReqJObj),
    case kzd_accounts:save(JObj) of
        {'ok', AccountDef} -> AccountDef;
        {'error', _R} ->
            lager:info("unable to create account definition: ~p", [_R]),
            throw('datastore_fault')
    end.

-spec load_initial_views(kzd_accounts:doc()) -> kzd_accounts:doc().
load_initial_views(AccountDoc) ->
    _ = kz_datamgr:refresh_views(kz_doc:account_db(AccountDoc)),
    lager:info("loaded initial views"),
    AccountDoc.

-spec create_account_mod(kzd_accounts:doc()) -> kzd_accounts:doc().
create_account_mod(AccountDoc) ->
    Db = kzs_util:format_account_mod_id(kz_doc:account_id(AccountDoc)),
    case kazoo_modb:create(Db) of
        'true' ->
            lager:info("created this month's MODb for account"),
            AccountDoc;
        'false' ->
            lager:error("failed to create modb for account"),
            throw('datastore_fault')
    end.

-spec reconcile(kzd_accounts:doc()) -> kzd_accounts:doc().
reconcile(AccountDoc) ->
    _Services = kz_services:reconcile(kz_doc:account_db(AccountDoc)),
    lager:info("performed initial services reconcile"),
    AccountDoc.

-spec create_first_transaction(kzd_accounts:doc()) -> kzd_accounts:doc().
create_first_transaction(AccountDoc) ->
    {Year, Month, _} = erlang:date(),
    AccountId = kz_doc:account_id(AccountDoc),
    {'ok', _} = kz_currency:rollover(AccountId, Year, Month, 0),
    lager:info("created first transaction for account"),
    AccountDoc.

-spec set_notification_preference(kzd_accounts:doc()) -> kzd_accounts:doc().
set_notification_preference(AccountDoc) ->
    lager:info("set notification preference"),
    AccountId = kz_doc:account_id(AccountDoc),
    ResellerId = kz_services_reseller:find_id(AccountId),
    case kzd_accounts:fetch(ResellerId) of
        {'error', _E} ->
            lager:error("failed to open reseller '~s': ~p", [ResellerId, _E]),
            AccountDoc;
        {'ok', AccountJObj} ->
            case kzd_accounts:notification_preference(AccountJObj) of
                'undefined' ->
                    lager:info("notification preference not set on reseller '~s'", [ResellerId]),
                    AccountDoc;
                Preference ->
                    set_notification_preference(AccountDoc, Preference)
            end
    end.

-spec set_notification_preference(kzd_accounts:doc(), kz_term:ne_binary()) -> kzd_accounts:doc().
set_notification_preference(AccountDoc, Preference) ->
    AccountDefinition = kzd_accounts:set_notification_preference(AccountDoc, Preference),
    case kzd_accounts:save(AccountDefinition) of
        {'error', _R} ->
            lager:error("failed to update account definition: ~p", [_R]),
            AccountDoc;
        {'ok', UpdatedAccountDoc} ->
            lager:info("notification_preference set to '~s'", [Preference]),
            UpdatedAccountDoc
    end.

-spec maybe_set_trial_expires(kz_json:object()) -> kz_json:object().
maybe_set_trial_expires(JObj) ->
    case kzd_accounts:is_trial_account(JObj) of
        'false' -> JObj;
        'true' -> set_trial_expires(JObj)
    end.

-spec set_trial_expires(kz_json:object()) -> kz_json:object().
set_trial_expires(JObj) ->
    TrialTime = kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"trial_time">>, ?SECONDS_IN_DAY * 14),
    Expires = kz_time:now_s() + TrialTime,
    kzd_accounts:set_trial_expiration(JObj, Expires).

-spec add_apps_store_doc(kzd_accounts:doc()) -> kzd_accounts:doc().
add_apps_store_doc(AccountJObj) ->
    AppsStoreDoc = kzd_apps_store:new(kz_doc:id(AccountJObj)),
    {'ok', _} = kz_datamgr:save_doc(kz_doc:account_db(AccountJObj), AppsStoreDoc),
    lager:info("created initial apps store doc"),
    AccountJObj.
