### Migrations

#### About Migrations

The migrations API allows you to perform various account based migrations on an account, or an account and its sub-accounts.

#### List

Lists all available migrations.

> GET /v2/accounts/{ACCOUNT_ID}/migrations

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations
```

#### Summary

Provides a summary of a migration, including information on if it was performed and by whom and when.

> GET /v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}
```

#### Perform Migration

Allows you to perform a migration, with the following potential parameters:

Name|Values|Description
----|------|-----------
perform_migration|now|Performs the migration now (now is the only currently supported option)
include_descendants|true / false (default: false)|If true the migration is performed on the account and sub-accounts
include_resellers|true / false (default: false)|If true the migration is performed on reseller sub-accounts

> POST /v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}
```

