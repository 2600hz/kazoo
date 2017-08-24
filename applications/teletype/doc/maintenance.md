
# Overview

Most operators will be interested in whether emails are sending or not.

## See Receipts

    #> sup teletype_maintenance receipts
      |                                Call or Msg ID | Receipt                                       | To                             | From                           | Time
    1 |              40c4bcd636a9539f2a4dc2673958e378 | Queued as 9EDEB840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-32-48
    2 |              560a2618f1d6def1444b455bc7a50f9f | Queued as E0338840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-32-49
    3 |              19a56c7f364f9e0a49b93a4f1dbc843a | Queued as 75362840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-49-08
    4 |              ca6d835975d41d54411af94ea3ad0d30 | Queued as 34930840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-51-12
    ok

If the relay is a postfix running locally, grep /var/log/mail.log (or equivalent) for the receipt ID.

## Start a Teletype Module

Running `sup teletype_maintenance start_module {MODULE_NAME}` will start the specified teletype
module. If this is not already in `autoload_modules` then it will be added.

## Stop a Teletype Module

Running `sup teletype_maintenance stop_module {MODULE_NAME}` will unbind the specified teletype
module from notification events. This has the effect of disabling the module. If the module is in
`autoload_modules` then it will be removed.

## Restore System Templates

Running this command will restore system templates to the original/initial state by reading templates from the file system.

##### Restoring All Templates
```shell
sup teletype_maintenance restore_system_templates
```

##### Restoring a Specific Template
```shell
sup teletype_maintenance restore_system_templates {TEMPLATE_NAME}
```

## Remove Account's Template Customization

Removes the customization made to an account.

##### Remove All Account's Customizations

```shell
sup teletype_maintenance remove_customization {ACCOUNT_ID}
```

##### Remove a Specific Account's Customization

```shell
sup teletype_maintenance remove_customization {ACCOUNT_ID} {TEMPLATE_NAME}
```

## Force System's Templates on an Account

Will force system default templates to an account by removing all customization on the account and copying system's templates to the account's DB.

##### Force All System's Templates

```shell
sup teletype_maintenance force_system_default {ACCOUNT_ID}
```

##### Force a Specific System's Template

```shell
sup teletype_maintenance force_system_default {ACCOUNT_ID} {TEMPLATE_NAME}
```
