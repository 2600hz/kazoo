/*
Section: Crossbar
Title: Accounts
Language: en-US
*/

# Kazoo Accounts
Learn how to use the 2600hz mobile API set manage accounts.


## Moving an account

### Setting Up

An account can only be moved by a "superduper_admin" or  if enabled by anyone above the desired account.

You can enable that feature by editing the document `crossbar.accounts` in you `system_config` database and set the value to `tree`.

````
"default": {
    "allow_move": "tree" // Default to "superduper_admin"
}
````
### API

`POST` request on `accounts/ACCOUNT_ID_TO_MOVE/move`

With the following data payload:

`````
{
    "data": {
        "to": "ACCOUNT_ID_DESTINATION"
    }
}
`````
