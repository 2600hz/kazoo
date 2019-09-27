# Crossbar Maintenance Functions

## SUP-able functions

| Function                              | Arguments                               | Description                                                                                          |
| --------                              | ---------                               | -----------                                                                                          |
| `allow_account_number_additions/1`    | `(AccountId)`                           | Allows the account to add its own numbers                                                            |
| `app/1`                               | `(AppNameOrId)`                         | Fetch and print an App's metadata                                                                    |
| `apps/0`                              |                                         | Fetch and print all known Apps' metadata                                                             |
| `create_account/4`                    | `(AccountName,Realm,Username,Password)` | Create an account - typically used for the master account                                            |
| `db_init/0`                           |                                         | Updates system schemas using files in Crossbar `priv' folder during startup                          |
| `demote_account/1`                    | `(AccountId)`                           | Remove `superduper_admin` status from the account                                                    |
| `descendants_count/0`                 |                                         | Reconciles all accounts' descendant count                                                            |
| `descendants_count/1`                 | `(AccountId)`                           | Reconciles an account's descendant count                                                             |
| `disable_account/1`                   | `(AccountId)`                           | Marks an account as disabled                                                                         |
| `disallow_account_number_additions/1` | `(AccountId)`                           | Remove ability for account to add its own numbers                                                    |
| `does_schema_exist/1`                 | `(Schema)`                              | Checks that a given schema (and any referenced schemas) exist                                        |
| `enable_account/1`                    | `(AccountId)`                           | Marks an account as enabled                                                                          |
| `find_account_by_id/1`                | `(Id)`                                  | Given an ID, print a summary of the account                                                          |
| `find_account_by_name/1`              | `(Name)`                                | Given an account name, search for and print a summary of the account with that name                  |
| `find_account_by_number/1`            | `(Number)`                              | Given a phone number, search for and print a summary of the account that owns the number             |
| `find_account_by_realm/1`             | `(Realm)`                               | Given an SIP realm, search for and print a summary of the account with that realm                    |
| `flush/0`                             |                                         | Flushes the cached `system_config/crossbar` document and crossbar's local cache                      |
| `init_app/1`                          | `(AppPath)`                             | `AppPath` is the filesystem path to the app's `metadata/app.json`                                    |
| `init_app/2`                          | `(AppPath,AppUrl)`                      | Initializes (or updates) the app to use the `AppUrl` for API requests                                |
| `init_apps/1`                         | `(AppsPath)`                            | `AppsPath` is the filesystem path to a directory of apps (which each have a `metadata/app.json` file |
| `init_apps/2`                         | `(AppsPath,AppUrl)`                     | Initializes (or updates) the apps to use the `AppUrl` for API requests                               |
| `migrate/0`                           |                                         | Applies any data migrations needed between versions of KAZOO                                         |
| `migrate_account_data/1`              | `(Account)`                             | Performs just the account-specific migrations necessary on the Account                               |
| `migrate_accounts_data/0`             |                                         | Performs just the account-specific migrations necessary on all accounts                              |
| `move_account/2`                      | `(Account,ToAccount)`                   | Moves `Account` to be a child of `ToAccount`                                                         |
| `promote_account/1`                   | `(AccountId)`                           | Add the `superduper_admin` status to the account                                                     |
| `refresh/0`                           |                                         | Deprecated: use `kapps_maintenance refresh` instead                                                  |
| `refresh/1`                           | `(Value)`                               | Deprecated: use `kapps_maintenance refresh Value` instead                                            |
| `refresh_app/1`                       | `(AppPath)`                             | Updates app's metadata                                                                               |
| `refresh_app/2`                       | `(AppPath,AppUrl)`                      | Updates app's metadata and API URL                                                                   |
| `refresh_apps/1`                      | `(AppsPath)`                            | Updates all apps' metadata                                                                           |
| `refresh_apps/2`                      | `(AppsPath,AppUrl)`                     | Updates all apps' metadata and API URL                                                               |
| `register_views/0`                    |                                         | Registers Crossbar's CouchDB views into the `system_data` database                                   |
| `running_modules/0`                   |                                         | Lists the currently running API endpoints                                                            |
| `set_app_description/2`               | `(AppId,Value)`                         | Updates the app's description metadata                                                               |
| `set_app_extended_description/2`      | `(AppId,Value)`                         | Updates the app's extended description metadata                                                      |
| `set_app_features/2`                  | `(AppId,Value)`                         | Updates the app's features metadata. `1features` must be @-separated if more than one is supplied.   |
| `set_app_field/3`                     | `(AppId,Field,Value)`                   | Updates the app's metadata with `Field => Value`                                                     |
| `set_app_icon/2`                      | `(AppId,PathToPNGIcon)`                 | Updates the app's icon                                                                               |
| `set_app_label/2`                     | `(AppId,Value)`                         | Updates the app's label metadata                                                                     |
| `set_app_screenshots/2`               | `(AppId,PathToScreenshotsFolder)`       | Updates the app's screenshot metadata                                                                |
| `start_module/1`                      | `(Module)`                              | Starts an API endpoint and persists it to the startup list                                           |
| `stop_module/1`                       | `(Module)`                              | Stops and API endpoint and removes it from the startup list                                          |
| `update_schemas/0`                    |                                         | Updates the `system_schemas` database with the on-disk version of the schema                         |
