## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `bind/3` | `(Event,M,F)` | |
| `bind_and_register_views/3` | `(_AppName,Module,Function)` | |
| `binding/1` | `(migrate) | (refresh) | (refresh_account) | (register_views) | (_)` | |
| `call_id_status/1` | `(CallId)` | |
| `call_id_status/2` | `(CallId,Verbose)` | |
| `check_release/0` |  | |
| `check_system_config/1` | `(SystemConfig)` | |
| `check_system_configs/0` |  | |
| `cleanup_aggregated_account/1` | `(Account)` | |
| `cleanup_aggregated_accounts/0` |  | |
| `cleanup_aggregated_device/1` | `(DocId)` | |
| `cleanup_aggregated_devices/0` |  | |
| `cleanup_orphan_modbs/0` |  | |
| `cleanup_system_config/1` | `(Id)` | |
| `cleanup_system_configs/0` |  | |
| `cleanup_voicemail_media/1` | `(Account)` | |
| `ensure_aggregate/1` | `(Account)` | |
| `ensure_aggregate_account/1` | `(Account)` | |
| `ensure_aggregate_accounts/0` |  | |
| `ensure_aggregate_device/1` | `(Account)` | |
| `ensure_aggregate_devices/0` |  | |
| `ensure_aggregate_faxbox/1` | `(Account)` | |
| `ensure_aggregate_faxboxes/0` |  | |
| `ensure_aggregates/0` |  | |
| `ensure_reseller_id_account/1` | `(Account)` | |
| `ensure_reseller_id_account/2` | `(Account,ResellerId)` | |
| `ensure_reseller_id_accounts/0` |  | |
| `ensure_reseller_id_accounts/1` | `(Accounts)` | |
| `ensure_reseller_id_services/2` | `(AccountId,ResellerId)` | |
| `ensure_tree_account/1` | `(Account)` | |
| `ensure_tree_account_dry_run/1` | `(Account)` | |
| `ensure_tree_accounts/0` |  | |
| `ensure_tree_accounts/1` | `(Accounts)` | |
| `ensure_tree_accounts_dry_run/0` |  | |
| `ensure_tree_accounts_dry_run/1` | `(Accounts)` | |
| `find_invalid_acccount_dbs/0` |  | |
| `flush_account_views/0` |  | |
| `flush_getby_cache/0` |  | |
| `get_all_account_views/0` |  | |
| `import_account/2` | `(Account,Parent)` | |
| `init_system/0` |  | |
| `maybe_delete_db/1` | `(Database)` | |
| `migrate/0` |  | |
| `migrate/1` | `(Pause)` | |
| `migrate_limits/0` |  | |
| `migrate_limits/1` | `(Account)` | |
| `migrate_media/0` |  | |
| `migrate_media/1` | `(Account)` | |
| `migrate_modbs/0` |  | |
| `migrate_modbs/1` | `(Pause)` | |
| `migrate_modbs_ranged/1` | `(Start)` | |
| `migrate_modbs_ranged/2` | `(Pause,Start)` | |
| `migrate_modbs_ranged/3` | `(Pause,Start,End)` | |
| `migrate_system/0` |  | |
| `migrate_to_4_0/0` |  | |
| `parallel_migrate/1` | `(Workers)` | |
| `parallel_migrate/2` | `(Workers,Pause)` | |
| `parallel_migrate_modbs/1` | `(Workers)` | |
| `parallel_migrate_modbs/2` | `(Workers,Pause)` | |
| `parallel_migrate_modbs_ranged/2` | `(Workers,Start)` | |
| `parallel_migrate_modbs_ranged/3` | `(Workers,Pause,Start)` | |
| `parallel_migrate_modbs_ranged/4` | `(Workers,Pause,Start,End)` | |
| `purge_doc_type/2` | `(Type,Account) | ([],_Account) | (_,Account)` | |
| `purge_doc_type/3` | `(Type,Account,ChunkSize)` | |
| `read_all_account_views/0` |  | |
| `rebuild_db/1` | `(Database)` | |
| `rebuild_db/2` | `(Database,Pause)` | |
| `refresh/0` |  | |
| `refresh/1` | `(Database)` | |
| `refresh_account/1` | `(AccountId)` | |
| `refresh_account_db/1` | `(Database)` | |
| `refresh_system_views/0` |  | |
| `register_account_views/0` |  | |
| `register_system_dbs_views/0` |  | |
| `register_views/0` |  | |
| `register_views/1` | `(App) | (_)` | |
| `remove_aggregated_account/1` | `(Account)` | |
| `remove_deprecated_databases/0` |  | |
| `unbind/3` | `(Event,M,F)` | |
| `validate_system_config/1` | `(Id)` | |
| `validate_system_configs/0` |  | |
