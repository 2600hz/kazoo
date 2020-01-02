%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_services_maintenance).

-export([update_tree/3]).

-export([flush/0]).
-export([db_init/0]).
-export([refresh/0
        ,register_views/0
        ]).
-export([migrate_service_plans/0
        ,migrate_service_plans/1
        ,migrate_service_plan/2]).
-export([migrate_services/0]).
-export([fix_plan_apps/1]).
-export([reconcile/0
        ,reconcile/1
        ,remove_orphaned_services/0
        ]).
-export([sync/1]).
-export([sync_descendants/1]).
-export([make_reseller/1]).
-export([demote_reseller/1]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).
-export([rebuild_services_db/0
        ,attempt_services_recovery/0
        ]).
-export([disable_topup/0]).
-export([enable_topup/0]).
-export([topup_status/0
        ,topup_status/1
        ]).
-export([get_accounts_by_depth/0]).
-export([get_accounts_with_plans/0]).

-include("services.hrl").

-define(UI_APPS_KEY(PlanId), [<<"plans">>, PlanId, <<"overrides">>, <<"ui_apps">>]).
-define(UI_APP_KEY(PlanId, Name), [<<"plans">>, PlanId, <<"overrides">>, <<"ui_apps">>, Name]).
-define(KZ_SERVICES_DB_TMP, <<"services_backup">>).

%%------------------------------------------------------------------------------
%% @doc TODO: where does this belong, also needs testing
%% @end
%%------------------------------------------------------------------------------
-spec update_tree(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_services:services().
update_tree(Account, NewTree, NewResellerId) ->
    AccountId = kzs_util:format_account_id(Account),
    Services = kz_services:fetch(AccountId, ['skip_cache']),
    ServicesJObj = kz_services:services_jobj(Services),
    Props = props:filter_undefined(
              [{?SERVICES_PVT_TREE, NewTree}
              ,{?SERVICES_PVT_TREE_PREVIOUSLY, kzd_accounts:tree(ServicesJObj)}
              ,{?SERVICES_PVT_MODIFIED, kz_time:now_s()}
              ,{?SERVICES_PVT_RESELLER_ID, NewResellerId}
              ]),
    %%FIXME: do something about setting pvt_auth_*_id
    {'ok', NewServicesJObj} = kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Props, ServicesJObj)),
    kz_services:set_services_jobj(Services, NewServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).

%%------------------------------------------------------------------------------
%% @doc Creates the services db, adds the required view to the services db
%% and registers the quantifier view for account dbs.
%% @end
%%------------------------------------------------------------------------------
-spec db_init() -> 'ok'.
db_init() ->
    _ = kz_datamgr:del_doc(?KZ_DATA_DB, <<"account-kazoo_apps-services">>),
    refresh().

-spec register_views() -> 'ok'.
register_views() ->
    _ = kz_datamgr:register_views_from_folder('kazoo_services'),
    _ = kz_datamgr:register_view('kazoo_services', {<<"_design/services">>, js_design_doc()}),
    'ok'.

-spec js_design_doc() -> kz_json:object().
js_design_doc() ->
    Quantify = [{<<"map">>, js_quantify_map()}
               ,{<<"reduce">>, <<"_sum">>}
               ],
    ObjectPlans = [{<<"map">>, js_object_plans_map()}],
    Bookkeepers = [{<<"map">>, js_bookkeepers_map()}],
    ServicePlans = [{<<"map">>, js_service_plans_map()}],
    Views = [{<<"quantify">>, kz_json:from_list(Quantify)}
            ,{<<"object_plans">>, kz_json:from_list(ObjectPlans)}
            ,{<<"bookkeepers">>, kz_json:from_list(Bookkeepers)}
            ,{<<"plans">>, kz_json:from_list(ServicePlans)}
            ],
    ViewMap = kz_json:from_list([{<<"classification">>, <<"account">>}]),
    Kazoo = kz_json:from_list([{<<"view_map">>, [ViewMap]}]),
    Props = [{<<"_id">>, <<"_design/services">>}
            ,{<<"kazoo">>, Kazoo}
            ,{<<"views">>, kz_json:from_list(Views)}
            ],
    kz_json:from_list(Props).

-spec js_service_plans_map() -> kz_term:ne_binary().
js_service_plans_map() ->
    iolist_to_binary(
      ["function(doc) {"
       "    if (doc.pvt_type != 'service_plan' || doc.pvt_deleted) return;"
       "    emit(doc._id, {'id': doc._id, 'name': doc.name, 'description': doc.description, 'category': doc.category});"
       "}"
      ]
     ).

-spec js_bookkeepers_map() -> kz_term:ne_binary().
js_bookkeepers_map() ->
    iolist_to_binary(
      ["function(doc) {"
       "    if (doc.pvt_type != 'bookkeeper' || doc.pvt_deleted) return;"
       "    emit(doc.bookkeeper, {'id': doc._id, 'name': doc.name, 'description': doc.description});"
       "}"
      ]
     ).

-spec js_object_plans_map() -> kz_term:ne_binary().
js_object_plans_map() ->
    iolist_to_binary(
      ["function(doc) {"
       "    if(!doc.service || !doc.service.plans || doc.pvt_deleted) return;"
       "    emit(doc.pvt_type, doc.service.plans);"
       "}"
      ]
     ).

-spec js_quantify_map() -> kz_term:ne_binary().
js_quantify_map() ->
    FunMatchBlock = fun(Class) -> [" emit(['phone_numbers', '", Class, "'], 1);"] end,
    iolist_to_binary(
      ["function(doc) {"
       "    if (doc.pvt_deleted) return;"
       "    if (doc.hasOwnProperty('enabled') && !doc.enabled) return;"
       "    switch (doc.pvt_type || doc._id) {"
       "        case 'user':"
       "            emit(['users', doc.priv_level], 1);"
       "            if (doc.qubicle && doc.qubicle.enabled) {"
       "                emit(['qubicle', 'recipients'], 1);"
       "            }"
       "            break;"
       "        case 'device':"
       "            emit(['devices', doc.device_type || 'sip_device'], 1);"
       "            break;"
       "        case 'limits':"
       "            if (doc.twoway_trunks && doc.twoway_trunks > 0) {"
       "                emit(['limits', 'twoway_trunks'], doc.twoway_trunks);"
       "            }"
       "            if (doc.inbound_trunks && doc.inbound_trunks > 0) {"
       "                emit(['limits', 'inbound_trunks'], doc.inbound_trunks);"
       "            }"
       "            if (doc.outbound_trunks && doc.outbound_trunks > 0) {"
       "                emit(['limits', 'outbound_trunks'], doc.outbound_trunks);"
       "            }"
       "            break;"
       "        case 'whitelabel':"
       "            emit(['branding', 'whitelabel'], 1);"
       "            break;"
       "        case 'dedicated_ip':"
       "            emit(['ips', 'dedicated'], 1);"
       "            break;"
       "        case 'apps_store':"
       "            var blacklist = doc.blacklist || [];"
       "            for (var index in doc.apps) {"
       "                if (!doc.apps.hasOwnProperty(index)) {"
       "                    continue;"
       "                }"
       "                if (blacklist.indexOf(index) != -1) {"
       "                    continue;"
       "                }"
       "                switch (doc.apps[index].allowed_users) {"
       "                    case 'all':"
       "                        emit(['user_apps', doc.apps[index].name], -1);"
       "                        break;"
       "                    case 'admins':"
       "                        emit(['user_apps', doc.apps[index].name], -2);"
       "                        break;"
       "                    case 'specific':"
       "                        emit(['user_apps', doc.apps[index].name], doc.apps[index].users.length);"
       "                        break;"
       "                }"
       "                emit(['account_apps', doc.apps[index].name], 1);"
       "            }"
       "            break;"
       "        case 'number':"
       "            emit(['number_carriers', doc.pvt_module_name], 1);"
       "            var features = doc.pvt_features || {};"
       "            for (var index in features) {"
       "                if (features.hasOwnProperty(index)) {"
       "                    emit(['number_services', index], 1);"
       "                }"
       "            }"
       "            if (features.local) return;"
      ,kazoo_numbers_maintenance:generate_js_classifiers(FunMatchBlock),
       "            break;"
       "        case 'qubicle_queue':"
       "            emit(['qubicle', 'queues'], 1);"
       "            break;"
       "        case 'vmbox':"
       "            emit(['voicemails', 'mailbox'], 1);"
       "            if (doc.transcribe) {"
       "                emit(['voicemails', 'transcription'], 1);"
       "            }"
       "            break;"
       "        case 'faxbox':"
       "            emit(['faxes', 'mailbox'], 1);"
       "            break;"
       "        case 'conference':"
       "            emit(['conferences', 'conference'], 1);"
       "            break;"
       "    }"
       "    if (doc.billing) {"
       "        for (index in doc.billing) {"
       "            emit(['billing', index], doc.billing[index]);"
       "        }"
       "    }"
       "}"
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc maintenance function for the services db
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:db_create(?KZ_SERVICES_DB),
    _ = kapps_maintenance:refresh(?KZ_SERVICES_DB),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_service_plans() -> 'no_return'.
migrate_service_plans() ->
    {'ok', AccountDb} = kapps_util:get_master_account_db(),
    migrate_service_plans(AccountDb).

-spec migrate_service_plans(kz_term:ne_binary()) -> 'no_return'.
migrate_service_plans(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    ViewOptions = ['include_docs'],
    {'ok', JObjs} = kz_datamgr:get_results(AccountDb, <<"services/plans">>, ViewOptions),
    io:format("verifying ~p service plans have been migrated~n", [length(JObjs)]),
    migrate_service_plans(AccountDb, [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]).

-spec migrate_service_plans(kz_term:ne_binary(), kz_json:objects()) -> 'no_return'.
migrate_service_plans(_AccountDb, []) -> 'no_return';
migrate_service_plans(AccountDb, [JObj|JObjs]) ->
    case kz_json:get_integer_value(<<"pvt_vsn">>, JObj, 0) > 1 of
        'true' -> migrate_service_plans(AccountDb, JObjs);
        'false' ->
            _ = migrate_service_plan(AccountDb, JObj),
            migrate_service_plans(AccountDb, JObjs)
    end.

-spec migrate_service_plan(kz_term:ne_binary(), kz_json:object()) -> kzd_services:doc().
migrate_service_plan(AccountDb, JObj) ->
    io:format("~s/~s requires migration~n", [AccountDb, kz_doc:id(JObj)]),
    Routines = [fun migrate_service_plans_bookkeeper/2
               ,fun migrate_service_plans_apps/2
               ,fun migrate_service_plans_pvt_parameters/2
               ,fun migrate_service_plans_ratedeck/2
               ,fun store_migrated_services_plan/2
               ],
    lists:foldl(fun(F, J) -> F(AccountDb, J) end, JObj, Routines).

-spec migrate_service_plans_bookkeeper(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
migrate_service_plans_bookkeeper(AccountDb, JObj) ->
    Keys = kz_json:get_keys(<<"bookkeepers">>, JObj),
    Cleaned = kz_json:delete_key(<<"bookkeepers">>, JObj),
    case migrate_bookkeepers(AccountDb, JObj, Keys) of
        'undefined' -> Cleaned;
        BookkeeperId ->
            AccountId = kzs_util:format_account_id(AccountDb),
            Setters = [{fun kzd_service_plan:set_bookkeeper_id/2, BookkeeperId}
                      ,{fun kzd_service_plan:set_bookkeeper_vendor_id/2, AccountId}
                      ,{fun kzd_service_plan:set_bookkeeper_type/2, <<"braintree">>}
                      ],
            kz_doc:setters(Cleaned, Setters)
    end.

-spec migrate_bookkeepers(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) -> kz_term:api_binary().
migrate_bookkeepers(AccountDb, JObj, Keys) ->
    migrate_bookkeepers(AccountDb, JObj, Keys, 'undefined').

-spec migrate_bookkeepers(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries(), kz_term:api_binary()) -> kz_term:api_binary().
migrate_bookkeepers(_AccountDb, _JObj, [], BraintreeId) -> BraintreeId;
migrate_bookkeepers(AccountDb, JObj, [Key|Keys], BraintreeId) ->
    Mappings = kz_json:get_ne_json_value([<<"bookkeepers">>, Key], JObj, kz_json:new()),
    ViewOptions = [{'key', Key}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"services/bookkeepers">>, ViewOptions) of
        {'ok', [Result|_]} when Key =:= <<"braintree">> ->
            Bookkeeper = kz_json:get_value(<<"doc">>, Result),
            OtherId = merge_bookkeeper(AccountDb, Bookkeeper, Mappings),
            migrate_bookkeepers(AccountDb, JObj, Keys, OtherId);
        {'ok', [Result|_]} ->
            Bookkeeper = kz_json:get_value(<<"doc">>, Result),
            _OtherId = merge_bookkeeper(AccountDb, Bookkeeper, Mappings),
            migrate_bookkeepers(AccountDb, JObj, Keys, BraintreeId);
        _Else when Key =:= <<"braintree">> ->
            OtherId = create_bookkeeper(AccountDb, Mappings, Key),
            migrate_bookkeepers(AccountDb, JObj, Keys, OtherId);
        _Else ->
            _OtherId = create_bookkeeper(AccountDb, Mappings, Key),
            migrate_bookkeepers(AccountDb, JObj, Keys, BraintreeId)
    end.

-spec merge_bookkeeper(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> kz_term:api_binary().
merge_bookkeeper(AccountDb, Bookkeeper, NewMappings) ->
    CurrentMappings = kzd_bookkeeper:mappings(Bookkeeper),
    Mappings = kz_json:merge_recursive(CurrentMappings, NewMappings),
    {'ok', JObj} = kz_datamgr:save_doc(AccountDb, kzd_bookkeeper:set_mappings(Bookkeeper, Mappings)),
    kz_doc:id(JObj).

-spec create_bookkeeper(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binary().
create_bookkeeper(AccountDb, Mappings, Key) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    BaseJObj = kz_doc:update_pvt_parameters(kz_json:new()
                                           ,AccountDb
                                           ,[{'account_id', AccountId}
                                            ,{'crossbar_doc_vsn', 2}
                                            ,{'type', kzd_bookkeeper:type()}
                                            ]
                                           ),
    Name = <<"Migrated from Service Plans for 4.3+">>,
    Setters = [{fun kzd_bookkeeper:set_name/2, Name}
              ,{fun kzd_bookkeeper:set_bookkeeper_type/2, Key}
              ,{fun kzd_bookkeeper:set_mappings/2, Mappings}
              ],
    Bookkeeper = kz_doc:setters(BaseJObj, Setters),
    {'ok', JObj} = kz_datamgr:save_doc(AccountDb, Bookkeeper),
    kz_doc:id(JObj).

-spec migrate_service_plans_apps(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
migrate_service_plans_apps(_AccountDb, JObj) ->
    Plan = kzd_service_plan:plan(JObj),
    Apps = migrate_ui_apps(Plan),
    Cleaned = kz_json:delete_key([<<"plan">>, <<"ui_apps">>], JObj),
    kzd_service_plan:set_applications(Cleaned, Apps).

-spec migrate_service_plans_pvt_parameters(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
migrate_service_plans_pvt_parameters(_AccountDb, JObj) ->
    Options = [{'crossbar_doc_vsn', 2}],
    kz_doc:update_pvt_parameters(JObj, ?KZ_SERVICES_DB, Options).


-spec migrate_service_plans_ratedeck(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
migrate_service_plans_ratedeck(_AccountDb, JObj) ->
    Plan = kzd_service_plan:plan(JObj),
    Setters = props:filter_empty(
                [{fun kzd_service_plan:set_ratedeck_id/2
                 ,get_ratedeck_binary(<<"ratedeck">>, Plan)
                 }
                ,{fun kzd_service_plan:set_ratedeck_name/2
                 ,get_ratedeck_binary(<<"ratedeck_name">>, Plan)
                 }
                ]
               ),
    Keys = [[<<"plan">>, <<"ratedeck">>]
           ,[<<"plan">>, <<"ratedeck_name">>]
           ],
    Cleaned = kz_json:delete_keys(Keys, JObj),
    kz_doc:setters(Cleaned, Setters).

-spec store_migrated_services_plan(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
store_migrated_services_plan(AccountDb, JObj) ->
    io:format("  storing migrated service plan ~s/~s~n", [AccountDb, kz_doc:id(JObj)]),
    {'ok', UpdatedJObj} = kz_datamgr:save_doc(AccountDb, JObj),
    UpdatedJObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_services() -> 'ok'.
migrate_services() ->
    migrate_services(
      get_accounts_by_depth()
     ).

-spec migrate_services(kz_term:ne_binaries()|kz_services:services()) -> 'ok'.
migrate_services([]) -> 'ok';
migrate_services([AccountId|AccountIds]) ->
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'skip_cache'
                   ],
    _ = migrate_services(
          kz_services:fetch(AccountId, FetchOptions)
         ),
    migrate_services(AccountIds);
migrate_services(Services) ->
    JObj = kz_services:services_jobj(Services),
    case kz_term:to_integer(kz_doc:vsn(JObj, 0)) > 1 of
        'true' -> 'ok';
        'false' ->
            _ = kz_datamgr:save_doc(?KZ_SERVICES_DB
                                   ,migrate_services_doc(Services, JObj)
                                   ),
            'ok'
    end.

-spec migrate_services_doc(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_services_doc(Services, JObj) ->
    %% NOTE: order is important here...
    Routines = [fun migrate_services_ratedeck/2
               ,fun migrate_services_remove_keys/2
               ,fun migrate_manual_quantities/2
               ,fun migrate_account_quantities/2
               ,fun migrate_cascade_quantities/2
               ,fun migrate_services_plans/2
               ,fun migrate_services_pvt_parameters/2
               ],
    lists:foldl(fun(F, J) -> F(Services, J) end, JObj, Routines).

-spec migrate_services_ratedeck(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_services_ratedeck(Services, JObj) ->
    CurrentJObj = kz_services:current_services_jobj(Services),
    Quantities = kz_json:get_json_value(<<"quantities">>, CurrentJObj, kz_json:new()),
    Setters = props:filter_empty(
                [{fun kzd_services:set_ratedeck_id/2
                 ,get_ratedeck_binary(<<"ratedeck">>, Quantities)
                 }
                ,{fun kzd_services:set_ratedeck_name/2
                 ,get_ratedeck_binary(<<"ratedeck_name">>, Quantities)
                 }
                ]
               ),
    kz_doc:setters(JObj, Setters).

-spec get_ratedeck_binary(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
get_ratedeck_binary(Key, Quantities) ->
    case kz_json:get_keys(Key, Quantities) of
        [] -> 'undefined';
        [Binary|_] -> Binary
    end.

-spec migrate_services_remove_keys(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_services_remove_keys(_Services, JObj) ->
    Keys = [<<"pvt_dirty">>
           ,<<"billing_id">>
           ,<<"quantities">>
           ,<<"transactions">>
           ,<<"account_quantities">>
           ,<<"cascade_quantities">>
           ],
    kz_json:delete_keys(Keys, JObj).

-spec migrate_manual_quantities(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_manual_quantities(Services, JObj) ->
    Keys = [<<"users">>
           ,<<"devices">>
           ,<<"ips">>
           ,<<"limits">>
           ,<<"number_services">>
           ,<<"number_carriers">>
           ,<<"phone_numbers">>
           ,<<"ledgers">>
           ,<<"qubicle">>
           ,<<"branding">>
           ,<<"ui_apps">>
           ,<<"billing">>
           ,<<"ratedeck">>
           ,<<"ratedeck_name">>
           ,<<"faxes">>
           ,<<"voicemails">>
           ,<<"conferences">>
           ],
    CurrentJObj = kz_services:current_services_jobj(Services),
    Quantities = kz_json:get_json_value(<<"quantities">>, CurrentJObj, kz_json:new()),
    ManualQuantities = kz_json:delete_keys(Keys, Quantities),
    kzd_services:set_manual_quantities(JObj, ManualQuantities).

-spec migrate_account_quantities(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_account_quantities(Services, JObj) ->
    Quantities = kz_services:account_quantities(Services),
    kzd_services:set_account_quantities(JObj, Quantities).

-spec migrate_cascade_quantities(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_cascade_quantities(Services, JObj) ->
    Quantities = kz_services:cascade_quantities(Services),
    kzd_services:set_cascade_quantities(JObj, Quantities).

-spec migrate_services_plans(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_services_plans(Services, JObj) ->
    CurrentJObj = kz_services:current_services_jobj(Services),
    PlanIds = kzd_services:plan_ids(CurrentJObj),
    Plans = migrate_services_plans(CurrentJObj, PlanIds, kz_json:new()),
    kzd_services:set_plans(JObj, Plans).

-spec migrate_services_plans(kz_json:object(), kz_term:ne_binaries(), kz_json:object()) -> kz_json:object().
migrate_services_plans(_JObj, [], Plans) -> Plans;
migrate_services_plans(JObj, [PlanId|PlanIds], Plans) ->
    Plan = kzd_services:plan(JObj, PlanId),
    VendorId = plan_vendor_id(Plan),
    Overrides = plan_overrides(Plan),
    Props = [{[PlanId, <<"overrides">>], Overrides}
            ,{[PlanId, <<"vendor_id">>], VendorId}
            ],
    migrate_services_plans(JObj, PlanIds, kz_json:set_values(Props, Plans)).

-spec plan_overrides(kz_json:object()) -> kz_json:object().
plan_overrides(Plan) ->
    PlanOverrides = kz_json:get_ne_json_value(<<"overrides">>
                                             ,Plan
                                             ,kz_json:new()
                                             ),
    kz_json:from_list(
      props:filter_empty(
        [{<<"plan">>, migrate_account_apps(PlanOverrides)}
        ,{<<"applications">>, migrate_ui_apps(PlanOverrides)}
        ]
       )
     ).

-spec migrate_account_apps(kz_json:object()) -> kz_json:object().
migrate_account_apps(PlanOverrides) ->
    AppNames = kz_json:get_keys(<<"ui_apps">>, PlanOverrides),
    migrate_account_apps(PlanOverrides, AppNames).

migrate_account_apps(PlanOverrides, []) ->
    kz_json:delete_key(<<"ui_apps">>, PlanOverrides);
migrate_account_apps(PlanOverrides, [AppName|AppNames]) ->
    App = kz_json:get_ne_json_value([<<"ui_apps">>, AppName], PlanOverrides, kz_json:new()),
    Keys = [<<"app_id">>
           ,<<"account_id">>
           ,<<"account_db">>
           ,<<"enabled">>
           ,<<"category">>
           ,<<"name">>
           ],
    Cleaned = kz_json:delete_keys(Keys, App),
    case kz_term:is_empty(Cleaned) of
        'true' -> migrate_account_apps(PlanOverrides, AppNames);
        'false' ->
            JObj = kz_json:set_value([<<"account_apps">>, AppName]
                                    ,Cleaned
                                    ,PlanOverrides
                                    ),
            migrate_account_apps(JObj, AppNames)
    end.

-spec plan_vendor_id(kz_json:object()) -> kz_term:ne_binary().
plan_vendor_id(JObj) ->
    Account = kz_json:get_first_defined([<<"account_id">>
                                        ,<<"account_db">>
                                        ,<<"vendor_id">>
                                        ]
                                       ,JObj
                                       ),
    case kz_term:is_empty(Account) of
        'false' ->
            kzs_util:format_account_id(Account);
        'true' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            MasterAccountId
    end.

-spec migrate_services_pvt_parameters(kz_services:services(), kz_json:object()) -> kz_json:object().
migrate_services_pvt_parameters(_Services, JObj) ->
    Options = [{'crossbar_doc_vsn', 2}],
    kz_doc:update_pvt_parameters(JObj, ?KZ_SERVICES_DB, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_ui_apps(kz_json:object()) -> kz_json:object().
migrate_ui_apps(JObj) ->
    AppNames = kz_json:get_keys(<<"ui_apps">>, JObj),
    migrate_ui_apps(JObj, AppNames, kz_json:new()).

-spec migrate_ui_apps(kz_json:object(), kz_term:ne_binaries(), kz_json:object()) -> kz_json:object().
migrate_ui_apps(_JObj, [], Apps) -> Apps;
migrate_ui_apps(JObj, [AppName|AppNames], Apps) ->
    case kz_json:get_ne_json_value([<<"ui_apps">>, AppName], JObj) of
        'undefined' -> migrate_ui_apps(JObj, AppNames, Apps);
        App ->
            Id = kz_json:get_ne_binary_value(<<"app_id">>, App),
            VendorId = find_ui_apps_vendor(App),
            Enabled = kz_json:is_true(<<"enabled">>, App, 'true'),
            UpdatedApps = maybe_add_ui_app(AppName, Id, VendorId, Enabled, Apps),
            migrate_ui_apps(JObj, AppNames, UpdatedApps)
    end.

-spec maybe_add_ui_app(kz_term:ne_binary(), kz_term:api_binary(), kz_term:ne_binary(), boolean(), kz_json:object()) -> kz_json:object().
maybe_add_ui_app(AppName, Id, VendorId, Enabled, Apps) ->
    case kz_term:is_empty(Id) of
        'true' -> Apps;
        'false' ->
            Props = [{[Id, <<"name">>], AppName}
                    ,{[Id, <<"vendor_id">>], VendorId}
                    ,{[Id, <<"enabled">>], Enabled}
                    ],
            kz_json:set_values(Props, Apps)
    end.

-spec find_ui_apps_vendor(kz_json:object()) -> kz_term:ne_binary().
find_ui_apps_vendor(JObj) ->
    Account = kz_json:get_first_defined([<<"account_id">>
                                        ,<<"account_db">>
                                        ,<<"vendor_id">>
                                        ]
                                       ,JObj
                                       ),
    case kz_term:is_empty(Account) of
        'false' -> kzs_util:format_account_id(Account);
        'true' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            MasterAccountId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_plan_apps(kz_term:ne_binary()) -> 'ok'.
fix_plan_apps(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    ViewOptions = ['include_docs'],
    {'ok', JObjs} = kz_datamgr:get_results(AccountDb, <<"services/plans">>, ViewOptions),
    io:format("verifying ~p service plans have valid applications~n", [length(JObjs)]),
    fix_plan_apps(AccountDb, [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]).

-spec fix_plan_apps(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
fix_plan_apps(_AccountDb, []) -> 'ok';
fix_plan_apps(AccountDb, [JObj|JObjs]) ->
    _ = fix_plan_app(AccountDb, JObj),
    fix_plan_apps(AccountDb, JObjs).

-spec fix_plan_app(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
fix_plan_app(AccountDb, JObj) ->
    io:format("verifying service plan ~s/~s applications~n"
             ,[kzs_util:format_account_id(AccountDb)
              ,kz_doc:id(JObj)
              ]
             ),
    Apps = kzd_service_plan:applications(JObj),
    Keys = kz_json:get_keys(Apps),
    UpdatedJObj = fix_plan_app(AccountDb, JObj, Keys),
    case kz_json:are_equal(JObj, UpdatedJObj) of
        'true' -> 'ok';
        'false' ->
            io:format("  storing updated service plan ~s/~s~n", [AccountDb, kz_doc:id(JObj)]),
            _ = kz_datamgr:save_doc(AccountDb, UpdatedJObj),
            'ok'
    end.

-spec fix_plan_app(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
fix_plan_app(_AccountDb, JObj, []) -> JObj;
fix_plan_app(AccountDb, JObj, [AppId|AppIds]) ->
    PlanApps = kzd_service_plan:applications(JObj),
    PlanApp = kz_json:get_value(AppId, PlanApps),
    VendorId = get_plan_app_vendor(PlanApp),
    Props = maybe_open_app(VendorId, AppId, PlanApp),
    case kz_term:is_empty(Props) of
        'true' -> fix_plan_app(AccountDb, JObj, AppIds);
        'false' ->
            UpdatedApps =
                kz_json:set_values(Props
                                  ,kz_json:delete_key(AppId, PlanApps)
                                  ),
            fix_plan_app(AccountDb, kzd_service_plan:set_applications(JObj, UpdatedApps), AppIds)
    end.

-spec maybe_open_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
maybe_open_app(VendorId, Id, PlanApp) ->
    VendorDb = kzs_util:format_account_db(VendorId),
    case kz_datamgr:open_cache_doc(VendorDb, Id) of
        {'ok', App} ->
            [{[Id, <<"name">>], kzd_app:name(App)}
            ,{[Id, <<"vendor_id">>], VendorId}
            ];
        {'error', 'not_found'} ->
            io:format("  service plan has an invalid application ~s/~s~n", [VendorId, Id]),
            Name = kz_json:get_ne_binary_value(<<"name">>, PlanApp),
            maybe_find_app(VendorId, Name);
        {'error', _} -> []
    end.

-spec maybe_find_app(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:proplist().
maybe_find_app(_VendorId, 'undefined') -> [];
maybe_find_app(?NE_BINARY = VendorId, Name) ->
    ViewOptions = [{'key', Name}],
    VendorDb = kzs_util:format_account_db(VendorId),
    case kz_datamgr:get_results(VendorDb, <<"apps_store/crossbar_listing">>, ViewOptions) of
        {'ok', [App|_]} ->
            Id = kz_doc:id(App),
            io:format("  correcting invalid application ~s with ~s/~s~n"
                     ,[Name, VendorId, Id]
                     ),
            [{[Id, <<"name">>], Name}
            ,{[Id, <<"vendor_id">>], VendorId}
            ];
        _Else ->
            io:format("  unable to find valid application for ~s in vendor ~s~n"
                     ,[Name, VendorId]
                     ),
            []
    end.

-spec get_plan_app_vendor(kz_json:object()) -> kz_term:ne_binary().
get_plan_app_vendor(JObj) ->
    case kz_json:get_ne_binary_value(<<"vendor_id">>, JObj) of
        'undefined' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            MasterAccountId;
        Account -> kzs_util:format_account_id(Account)
    end.

%%------------------------------------------------------------------------------
%% @doc schedules an eventual sync with the bookkeeper and will dirty the
%% full reseller tree (as it normally does when changes occur)
%% @end
%%------------------------------------------------------------------------------
-spec reconcile() -> 'no_return'.
reconcile() ->
    reconcile('all').

-spec reconcile(kz_term:text()) -> 'no_return'.
reconcile('all') ->
    Accounts = get_accounts_by_depth(),
    Total = length(Accounts),
    _ = lists:foldr(fun(Account, Current) ->
                            io:format("reconcile services (~p/~p) '~s'~n", [Current, Total, Account]),
                            try reconcile(Account)
                            catch
                                _E:_R ->
                                    io:format("    unable to reconcile!~n~p~n", [_R])
                            end,
                            Current + 1
                    end, 1, Accounts),
    'no_return';
reconcile(Account) when not is_binary(Account) ->
    reconcile(kz_term:to_binary(Account));
reconcile(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ,'skip_cache'
                   ],
    CurrentServices = kz_services:fetch(AccountId, FetchOptions),
    CurrentJObj = kz_services:current_services_jobj(CurrentServices),
    Services = kz_services:maybe_save_services_jobj(CurrentServices),
    case kz_services:is_dirty(Services) of
        'false' -> 'no_return';
        'true' ->
            ReconciledJObj = kz_services:services_jobj(Services),
            log_discrepancy_correction(CurrentJObj, ReconciledJObj)
    end.

-spec log_discrepancy_correction(kz_json:object(), kz_json:object()) -> 'no_return'.
log_discrepancy_correction(CurrentJObj, ReconciledJObj) ->
    io:format("    quantity discrepancy corrected!~n", []),
    CurrentQuantities = kz_json:get_ne_json_value(<<"quantities">>, CurrentJObj, kz_json:new()),
    ReconciledQuantities = kz_json:get_ne_json_value(<<"quantities">>, ReconciledJObj, kz_json:new()),
    FlattenedCurrent = flatten_quantities(CurrentQuantities),
    _ = [io:format("      updated ~s from ~p to ~p~n"
                  ,[kz_binary:join(Key, <<".">>)
                   ,CurrentValue
                   ,ReconciledValue
                   ]
                  )
         || {Key, ReconciledValue} <- flatten_quantities(ReconciledQuantities)
                ,(CurrentValue = proplists:get_value(Key, FlattenedCurrent)) =/= ReconciledValue
        ],
    'no_return'.

-spec flatten_quantities(kz_json:object()) -> kz_term:proplist().
flatten_quantities(JObj) ->
    [{[Type, Category, Item]
     ,kz_json:get_integer_value([Type, Category, Item], JObj, 0)
     }
     || Type <- [<<"manual">>, <<"account">>, <<"cascade">>]
            ,Category <- kz_json:get_keys(Type, JObj)
            ,Item <- kz_json:get_keys([Type, Category], JObj)
    ].

%%------------------------------------------------------------------------------
%% @doc runs an immediate sync with a bookkeeper without dirtying the
%% reseller tree (only the one account is affected)
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:text()) -> 'ok'.
sync(Account) when not is_binary(Account) ->
    sync(kz_term:to_binary(Account));
sync(Account) ->
    _ = kz_services_bookkeeper:sync(Account),
    'ok'.

-spec sync_descendants(kz_term:text()) -> 'ok'.
sync_descendants(Account) when not is_binary(Account) ->
    sync_descendants(kz_term:to_binary(Account));
sync_descendants(Account) ->
    Descendants = kapps_util:account_descendants(Account),
    io:format("syncing ~p descendants of ~s", [length(Descendants), Account]),
    do_sync_descendants(Descendants).

-spec do_sync_descendants(kz_term:ne_binaries()) -> 'ok'.
do_sync_descendants([]) -> 'ok';
do_sync_descendants([Descendant|Descendants]) ->
    io:format("  syncing ~s, ~p accounts remaining", [Descendant, length(Descendants)]),
    _ = sync(Descendant),
    do_sync_descendants(Descendants).

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on the provided account
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(kz_term:text(), kz_term:text()) -> 'ok'.
set_reseller_id(Reseller, Account) when not is_binary(Account) ->
    set_reseller_id(Reseller, kz_term:to_binary(Account));
set_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    set_reseller_id(kz_term:to_binary(Reseller), Account);
set_reseller_id(Reseller, Account) ->
    kz_services_reseller:set_reseller_id(Reseller, Account).

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%------------------------------------------------------------------------------
-spec cascade_reseller_id(kz_term:text(), kz_term:text()) -> 'ok'.
cascade_reseller_id(Reseller, Account) when not is_binary(Account) ->
    cascade_reseller_id(Reseller, kz_term:to_binary(Account));
cascade_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    cascade_reseller_id(kz_term:to_binary(Reseller), Account);
cascade_reseller_id(Reseller, Account) ->
    kz_services_reseller:cascade_reseller_id(Reseller, Account).

%%------------------------------------------------------------------------------
%% @doc Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%------------------------------------------------------------------------------
-spec demote_reseller(kz_term:text()) -> 'ok'.
demote_reseller(Account) when not is_binary(Account) ->
    demote_reseller(kz_term:to_binary(Account));
demote_reseller(Account) ->
    kz_services_reseller:demote(Account).

%%------------------------------------------------------------------------------
%% @doc Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%------------------------------------------------------------------------------
-spec make_reseller(kz_term:text()) -> 'ok'.
make_reseller(Account) when not is_binary(Account) ->
    make_reseller(kz_term:to_binary(Account));
make_reseller(Account) ->
    kz_services_reseller:promote(Account).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attempt_services_recovery() -> 'ok'.
attempt_services_recovery() ->
    JObjs = fetch_all_service_docs(?KZ_SERVICES_DB_TMP),
    _ = kapps_maintenance:refresh(?KZ_SERVICES_DB),
    {'ok', Results} = kz_datamgr:save_docs(?KZ_SERVICES_DB, JObjs),
    log_services_recovery_results(Results).

-spec log_services_recovery_results(kz_json:objects()) -> 'ok'.
log_services_recovery_results([]) -> 'ok';
log_services_recovery_results([JObj|JObjs]) ->
    Id = kz_doc:id(JObj),
    _ = case kz_json:get_value(<<"reason">>, JObj) of
            'undefined' -> io:format("  restored document ~s~n", [Id]);
            Reason -> io:format("  failed to restore ~s: ~s~n", [Id, Reason])
        end,
    log_services_recovery_results(JObjs).

-spec rebuild_services_db() -> 'ok'.
rebuild_services_db() ->
    JObjs = fetch_all_service_docs(?KZ_SERVICES_DB),
    'ok' = backup_service_docs(JObjs),
    'true' = compare_services_backup(),
    _ = rebuild_services_db(JObjs),
    case compare_services_backup() of
        'true' -> io:format("rebuild completed successfully~n", []);
        'false' ->
            io:format("discrepancies found between restored and backed up service documents!~n", []),
            io:format("try: sup kazoo_services_maintenance attempt_services_recovery~n", [])
    end.

-spec rebuild_services_db(kz_json:objects()) -> 'ok'.
rebuild_services_db(JObjs) ->
    io:format("removing services database~n", []),
    kz_datamgr:db_delete(?KZ_SERVICES_DB),
    timer:sleep(5000),
    io:format("rebuilding services database views~n", []),
    refresh(),
    io:format("restoring service documents~n", []),
    _ = kz_datamgr:save_docs(?KZ_SERVICES_DB, JObjs),
    io:format("rebuild complete~n", []).

-spec compare_services_backup() -> boolean().
compare_services_backup() ->
    _ = io:format("comparing original and backed up service documents~n", []),
    Originals = [{kz_doc:id(JObj), JObj} || JObj <- fetch_all_service_docs(?KZ_SERVICES_DB)],
    Backups = [{kz_doc:id(JObj), JObj} || JObj <- fetch_all_service_docs(?KZ_SERVICES_DB_TMP)],
    lists:all(fun({Id, Original}) -> kz_json:are_equal(props:get_value(Id, Backups), Original) end, Originals).

-spec backup_service_docs(kz_json:objects()) -> 'ok' | 'error'.
backup_service_docs(JObjs) ->
    _ = io:format("saving all service documents to ~s~n", [?KZ_SERVICES_DB_TMP]),
    'true' = kz_datamgr:db_create(?KZ_SERVICES_DB_TMP),
    {'ok', Results} = kz_datamgr:save_docs(?KZ_SERVICES_DB_TMP, JObjs),
    case find_services_backup_failures(Results) of
        [] -> 'ok';
        Errors -> log_services_backup_failures(Errors)
    end.

-spec log_services_backup_failures(kz_json:objects()) -> 'error'.
log_services_backup_failures([]) -> 'error';
log_services_backup_failures([JObj|JObjs]) ->
    _ = io:format("  failed to backup ~s: ~s~n"
                 ,[kz_doc:id(JObj)
                  ,kz_json:get_value(<<"reason">>, JObj)
                  ]
                 ),
    log_services_backup_failures(JObjs).

-spec find_services_backup_failures(kz_json:objects()) -> kz_json:objects().
find_services_backup_failures(JObjs) ->
    [JObj || JObj <- JObjs, kz_json:get_ne_value(<<"error">>, JObj) =/= 'undefined'].

-spec fetch_all_service_docs(kz_term:ne_binary()) -> kz_json:objects().
fetch_all_service_docs(Database) ->
    _ = io:format("fetching all service docs from '~s'~n", [Database]),
    {'ok', JObjs} = kz_datamgr:all_docs(Database, ['include_docs']),
    [prepare_service_doc(JObj) || JObj <- JObjs, not_design_doc(JObj)].

-spec not_design_doc(kz_json:object()) -> boolean().
not_design_doc(JObj) ->
    case kz_doc:id(JObj) of
        <<"_design", _/binary>> -> 'false';
        _Else -> 'true'
    end.

-spec prepare_service_doc(kz_json:object()) -> kz_json:object().
prepare_service_doc(JObj) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    kz_json:delete_key(<<"_rev">>, Doc).

-spec remove_orphaned_services() -> 'no_return'.
remove_orphaned_services() ->
    {'ok', ServiceDocs} = kz_datamgr:all_docs(?KZ_SERVICES_DB),
    Count = lists:foldl(fun maybe_remove_orphan/2, 0, ServiceDocs),
    Count > 0
        andalso io:format("removed ~p service docs~n", [Count]),
    'no_return'.

-spec maybe_remove_orphan(kz_json:object() | kz_term:ne_binary(), non_neg_integer()) ->
          non_neg_integer().
maybe_remove_orphan(<<"_design/", _/binary>>, Count) -> Count;
maybe_remove_orphan(<<_/binary>> = AccountId, Count) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', _AccountDoc} -> Count;
        {'error', 'not_found'} ->
            {'ok', _} = kz_datamgr:del_doc(?KZ_SERVICES_DB, AccountId),
            io:format("account ~s not found, removing services~n", [AccountId]),
            Count+1
    end;
maybe_remove_orphan(ViewResult, Count) ->
    maybe_remove_orphan(kz_doc:id(ViewResult), Count).

%%------------------------------------------------------------------------------
%% @doc Enable auto top up
%% @end
%%------------------------------------------------------------------------------
-spec enable_topup() -> 'ok'.
enable_topup() ->
    _ = kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'true'),
    io:format("auto top up enabled ~n").

%%------------------------------------------------------------------------------
%% @doc Disable auto top up
%% @end
%%------------------------------------------------------------------------------
-spec disable_topup() -> 'ok'.
disable_topup() ->
    _ = kapps_config:set(?TOPUP_CONFIG, <<"enable">>, 'false'),
    io:format("auto top up disabled ~n", []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec topup_status() -> 'ok'.
topup_status() ->
    IsEnabled = case kapps_config:get_is_true(?TOPUP_CONFIG, <<"enable">>) of
                    'true' -> "enabled";
                    _ -> "disabled"
                end,
    io:format("auto top up is ~s~n", [IsEnabled]).

-spec topup_status(kz_term:ne_binary()) -> 'ok'.
topup_status(AccountId) ->
    {'ok', ReplinishUnits, ThresholdUnits} = kz_services_topup:get_topup(AccountId),
    io:format("+-----------------+------------------+------------------+---------------+-------------------------+~n"),
    io:format("| Current Balance | Notify threshold | Top up threshold | Top up Amount | Auto top up in 24 hours |~n"),
    io:format("+=================+==================+==================+===============+=========================+~n"),
    io:format("| ~-15w | ~-16w | ~-16w | ~-13w | ~-23w |~n"
             ,[kz_currency:available_dollars(AccountId, 0)
              ,get_notify_threshold(AccountId)
              ,kz_currency:units_to_dollars(ThresholdUnits)
              ,kz_currency:units_to_dollars(ReplinishUnits)
              ,kz_services_topup:should_topup(AccountId)
              ]),
    io:format("+-----------------+------------------+------------------+---------------+-------------------------+~n").

-spec get_notify_threshold(kz_term:ne_binary()) -> kz_term:api_float().
get_notify_threshold(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _Reason} ->
            io:format("failed to open account ~p: ~p", [AccountId, _Reason]),
            'undefined';
        {'ok', JObj} ->
            kzd_accounts:notifications_low_balance_threshold(JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_accounts_by_depth() -> kz_term:ne_binaries().
get_accounts_by_depth() ->
    ViewOptions = [],
    {'ok', JObjs} = kz_datamgr:get_results(?KZ_SERVICES_DB
                                          ,<<"services/by_tree_length">>
                                          ,ViewOptions
                                          ),
    [kz_doc:id(JObj) || JObj <- JObjs].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_accounts_with_plans() -> kz_term:ne_binaries().
get_accounts_with_plans() ->
    ViewOptions = [],
    {'ok', JObjs} = kz_datamgr:get_results(?KZ_SERVICES_DB
                                          ,<<"services/by_tree_length_with_plans">>
                                          ,ViewOptions
                                          ),
    [kz_doc:id(JObj) || JObj <- JObjs].
