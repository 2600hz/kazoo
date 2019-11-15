## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `add_allowed_feature_on_number/2` | `(_,_)` | |
| `add_allowed_feature_on_reseller_of/2` | `(_,_)` | |
| `add_allowed_feature_on_system_config/1` | `(_)` | |
| `add_denied_feature_on_number/2` | `(_,_)` | |
| `add_denied_feature_on_reseller_of/2` | `(_,_)` | |
| `all_features/0` |  | |
| `app_using/2` | `(Num,AccountDb)` | |
| `carrier_module_usage/0` |  | |
| `carrier_module_usage/1` | `(Prefix)` | |
| `convert_carrier_module/2` | `(Source,Target)` | |
| `convert_carrier_module/3` | `(Source,Target,Prefix)` | |
| `convert_carrier_module_number/2` | `(Num,Target)` | |
| `copy_accounts_to_number_dbs/0` |  | |
| `copy_accounts_to_number_dbs/1` | `(AccountIds)` | |
| `copy_assigned_number_dbs_to_account/1` | `(Account)` | |
| `copy_number_dbs_to_accounts/0` |  | |
| `copy_number_dbs_to_accounts/1` | `(NumberDbs)` | |
| `copy_number_from_accountdb_to_numbdb/2` | `(Account,Number)` | |
| `copy_number_from_db_to_assigned_account/1` | `(Num)` | |
| `copy_numbers_from_accountdb_to_numbdbs/2` | `(Account,Numbers)` | |
| `copy_numbers_from_dbs_to_assigned_accounts/1` | `(Numbers)` | |
| `copy_single_account_to_number_dbs/1` | `(AccountId)` | |
| `copy_single_assigned_number_db_to_account/2` | `(Account,NumberDb)` | |
| `copy_single_number_db_to_accounts/1` | `(NumberDb)` | |
| `copy_single_number_to_account_db/2` | `(Num,Account)` | |
| `delete/1` | `(Num)` | |
| `destructively_remove_from_account_db_if_not_in_numdb/0` |  | |
| `destructively_remove_from_number_dbs_if_not_in_accountdb/0` |  | |
| `destructively_remove_from_single_account_db_if_not_in_numdb/1` | `(AccountDb)` | |
| `destructively_remove_from_single_numdb_if_not_in_accountdb/1` | `(NumberDb)` | |
| `ensure_adminonly_features_are_reachable/0` |  | |
| `feature_permissions_on_number/1` | `(Num)` | |
| `feature_permissions_on_reseller_of/1` | `(_)` | |
| `feature_permissions_on_system_config/0` |  | |
| `fix_account_db_numbers/1` | `(Account)` | |
| `fix_apps_for_account_dbs/0` |  | |
| `fix_apps_for_account_dbs/1` | `(AccountDbs)` | |
| `fix_apps_for_number_dbs/0` |  | |
| `fix_apps_for_number_dbs/1` | `(NumberDbs)` | |
| `fix_apps_for_single_account_db/1` | `(Account)` | |
| `fix_apps_for_single_number_db/1` | `(NumberDb)` | |
| `fix_apps_in_number_dbs_for_accounts/1` | `(Accounts)` | |
| `fix_apps_in_number_dbs_for_single_account/1` | `(Account)` | |
| `fix_number/2` | `(Account,Number)` | |
| `fix_numbers/2` | `(Account,Numbers)` | |
| `fix_numbers_apps_in_accountdb/2` | `(Account,Numbers)` | |
| `fix_numbers_apps_in_numbdb/1` | `(Numbers)` | |
| `generate_js_classifiers/1` | `(FunMatchBlock)` | |
| `generate_numbers/4` | `(Type,AccountId,StartingNumber,Quantity)` | |
| `init_dbs/0` |  | |
| `migrate/0` |  | |
| `migrate_port_requests/0` |  | |
| `migrate_unassigned_numbers/0` |  | |
| `migrate_unassigned_numbers/1` | `(Number) | (_)` | |
| `purge_deleted/0` |  | |
| `purge_deleted/1` | `(Prefix)` | |
| `purge_discovery/0` |  | |
| `purge_discovery/1` | `(Prefix)` | |
| `refresh_numbers_db/1` | `(_) | (_Thing)` | |
| `refresh_numbers_dbs/0` |  | |
| `register_views/0` |  | |
| `remove_allowed_feature_on_number/2` | `(_,_)` | |
| `remove_allowed_feature_on_reseller_of/2` | `(_,_)` | |
| `remove_allowed_feature_on_system_config/1` | `(_)` | |
| `remove_denied_feature_on_number/2` | `(_,_)` | |
| `remove_denied_feature_on_reseller_of/2` | `(_,_)` | |
| `remove_wrong_assigned_from_accounts/0` |  | |
| `remove_wrong_assigned_from_single_accountdb/1` | `(Account)` | |
| `reset_allowed_features_to_defaults_on_system_config/0` |  | |
| `update_number_services_view/1` | `(_)` | |
