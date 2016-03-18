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

`POST` request on `accounts/{ACCOUNT_ID_TO_MOVE}/move`

With the following data payload:

`````
{
    "data": {
        "to": "ACCOUNT_ID_DESTINATION"
    }
}
`````

## Listing Sibling Accounts

By default a user account under an admin/reseller account can view all the other accounts under that reseller. If you would like current account only will be able to query its child accounts' sibling and not other accounts then set `allow_sibling_listing` in `system_config/crossbar.accounts` to `false`. Admin account can unrestrictedly list siblings.

## The Account Tree

Since accounts can be the child of 0 or more parent accounts, it is necessary to track each account's lineage. This is tracked in the account document (_id = ID of the account) in the `pvt_tree` array. The order of the list is from most-ancestral to parent.

So given `"pvt_tree":["1", "2", "3"]`, it can be determined that "3" is the parent account, "2" the grand-parent, and "1" is the great-grandparent. `"pvt_tree":[]` indicates the master (or Highlander) account; there should only be one!

## Retrieving Account Tree

### Request

`GET` request on `http://{SERVER}/v2/accounts/{ACCOUNT_ID}/tree`

### Response

`````
{
    "data": [
        {
            "id": "{JOHN_ACCOUNT_ID}",
            "name": "John's account"
        },
        {
            "id": "{JANE_ACCOUNT_ID}",
            "name": "Jane's account"
        }
    ]
}
`````
