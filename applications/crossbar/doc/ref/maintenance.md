## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `allow_account_number_additions/1` | `(AccountId)` | |
| `app/1` | `(AppNameOrId)` | |
| `apps/0` |  | |
| `create_account/4` | `(AccountName,Realm,Username,Password)` | |
| `db_init/0` |  | |
| `demote_account/1` | `(AccountId)` | |
| `descendants_count/0` |  | |
| `descendants_count/1` | `(AccountId)` | |
| `disable_account/1` | `(AccountId)` | |
| `disallow_account_number_additions/1` | `(AccountId)` | |
| `does_schema_exist/1` | `(Schema)` | |
| `enable_account/1` | `(AccountId)` | |
| `find_account_by_id/1` | `(Id)` | |
| `find_account_by_name/1` | `(Name)` | |
| `find_account_by_number/1` | `(Number)` | |
| `find_account_by_realm/1` | `(Realm)` | |
| `flush/0` |  | |
| `init_app/1` | `(AppPath)` | |
| `init_app/2` | `(AppPath,AppUrl)` | |
| `init_apps/1` | `(AppsPath)` | |
| `init_apps/2` | `(AppsPath,AppUrl)` | |
| `migrate/0` |  | |
| `migrate_account_data/1` | `(Account)` | |
| `migrate_accounts_data/0` |  | |
| `move_account/2` | `(Account,ToAccount)` | |
| `promote_account/1` | `(AccountId)` | |
| `refresh/0` |  | |
| `refresh/1` | `(Value)` | |
| `refresh_app/1` | `(AppPath)` | |
| `refresh_app/2` | `(AppPath,AppUrl)` | |
| `refresh_apps/1` | `(AppsPath)` | |
| `refresh_apps/2` | `(AppsPath,AppUrl)` | |
| `register_views/0` |  | |
| `running_modules/0` |  | |
| `set_app_description/2` | `(AppId,Value)` | |
| `set_app_extended_description/2` | `(AppId,Value)` | |
| `set_app_features/2` | `(AppId,Value)` | |
| `set_app_field/3` | `(AppId,Field,Value)` | |
| `set_app_icon/2` | `(AppId,PathToPNGIcon)` | |
| `set_app_label/2` | `(AppId,Value)` | |
| `set_app_screenshots/2` | `(AppId,PathToScreenshotsFolder)` | |
| `start_module/1` | `(Module)` | |
| `stop_module/1` | `(Module)` | |
| `update_schemas/0` |  | |
