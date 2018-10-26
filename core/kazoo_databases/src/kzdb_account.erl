-module(kzdb_account).

-export([create/2
        ,validate/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ACCOUNTS_CONFIG_CAT, <<"crossbar.accounts">>).

-spec create(kz_term:ne_binary(), kz_json:object()) -> kzd_accounts:doc() | 'undefined'.
create(AccountId, ReqJObj) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kapps_util:is_account_db(AccountDb)
        andalso kz_datamgr:db_create(AccountDb)
    of
        'false' ->
            lager:info("failed to create database: ~s", [AccountDb]),
            'undefined';
        'true' ->
            lager:debug("created account database: ~s", [AccountDb]),
            Doc = kz_doc:set_account_db(kz_doc:set_account_id(ReqJObj, AccountId)
                                       ,AccountDb
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
                ]
               ).

-spec create_account_definition(kz_json:object()) -> kz_json:object().
create_account_definition(ReqJObj) ->
    JObj = maybe_set_trial_expires(ReqJObj),

    case kzd_accounts:save(JObj) of
        {'ok', AccountDef} ->
            lager:debug("account definition created: ~s", [kz_doc:revision(AccountDef)]),
            AccountDef;
        {'error', _R} ->
            lager:debug("unable to create account definition: ~p", [_R]),
            throw('datastore_fault')
    end.

-spec load_initial_views(kzd_accounts:doc()) -> kzd_accounts:doc().
load_initial_views(AccountDoc) ->
    _ = kz_datamgr:refresh_views(kz_doc:account_db(AccountDoc)),
    lager:debug("loaded initial views"),
    AccountDoc.

-spec create_account_mod(kzd_accounts:doc()) -> kzd_accounts:doc().
create_account_mod(AccountDoc) ->
    case kazoo_modb:create(kz_doc:account_db(AccountDoc)) of
        'true' ->
            lager:debug("created this month's MODb for account"),
            AccountDoc;
        'false' ->
            lager:error("failed to create modb for account"),
            throw('datastore_fault')
    end.

-spec reconcile(kzd_accounts:doc()) -> kzd_accounts:doc().
reconcile(AccountDoc) ->
    _Services = kz_services:reconcile(kz_doc:account_db(AccountDoc)),
    lager:debug("performed initial services reconcile"),
    AccountDoc.

-spec create_first_transaction(kzd_accounts:doc()) -> kzd_accounts:doc().
create_first_transaction(AccountDoc) ->
    {Year, Month, _} = erlang:date(),
    AccountId = kz_doc:account_id(AccountDoc),
    {'ok', _} = kz_currency:rollover(AccountId, Year, Month, 0),
    lager:debug("created first transaction for account"),
    AccountDoc.

-spec set_notification_preference(kzd_accounts:doc()) -> kzd_accounts:doc().
set_notification_preference(AccountDoc) ->
    lager:debug("set notification preference"),
    AccountId = kz_doc:account_id(AccountDoc),
    ResellerId = kz_services_reseller:find_id(AccountId),
    case kzd_accounts:fetch(ResellerId) of
        {'error', _E} ->
            lager:error("failed to open reseller '~s': ~p", [ResellerId, _E]),
            AccountDoc;
        {'ok', AccountJObj} ->
            case kzd_accounts:notification_preference(AccountJObj) of
                'undefined' ->
                    lager:debug("notification preference not set on reseller '~s'", [ResellerId]),
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


%%------------------------------------------------------------------------------
%% @doc This function will determine if the account name is unique
%% @end
%%------------------------------------------------------------------------------
-spec maybe_is_unique_account_name(kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
maybe_is_unique_account_name(AccountId, Name) ->
    case kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"ensure_unique_name">>, 'true') of
        'true' -> is_unique_account_name(AccountId, Name);
        'false' -> 'true'
    end.

-spec is_unique_account_name(kz_term:api_ne_binary(), kz_term:ne_binary()) -> boolean().
is_unique_account_name(AccountId, Name) ->
    AccountName = kzd_accounts:normalize_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_NAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'error', 'not_found'} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= AccountId;
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?AGG_VIEW_NAME, ?KZ_ACCOUNTS_DB]),
            'false'
    end.
