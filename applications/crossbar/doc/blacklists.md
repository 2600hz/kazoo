

A blacklist is a map of caller id numbers that can be then apply to the account to block these callers to call the system.

#### Structure

The structure is really simple:

* `name`: A freindly name for your blacklist.
* `numbers`: A map of caller id to block
Ex:
```
{
    "name": "Main Blacklist",
    "numbers": {
        "+14151234567": {
        }
    }
}
```

When you upload numbers they will be converted to e164 format.  In the event that you want to bypass this feature you can upload numbers in a "raw" list which will skip the formaters.  If the number exists in both lists then the re-formated number will be prefered.

```
{
    "name": "Raw Blacklist",
    "raw_numbers": {
        "0000000000": {
        }
    }
}
```

#### Usage

Once you created your blacklists using the api you can apply them to the account by adding the blacklist ids to the account, like the following:

```
"blacklists": [
    "dbfc14854a06bab3014898b6b4e1ffa0", // Main blacklist
    "davb14854a06bab3542132b6b4e1ffa0"  // Secondary blacklist
]
```

#### Crossbar

Using Crossbar to modify Blacklist is very simple:

* GET - Gets the current set(s).
* PUT - Add a set.
* POST - Updates a set.
* DELETE - Removes a set.

##### Account Blacklist URI

`/v2/accounts/{ACCOUNT_ID}/blacklists`

###### GET - Fetch account blacklists:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/blacklists

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}

###### PUT - Add account blacklists:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/blacklists -d '{"data": {"name": "Main Blacklis","numbers": {"+14151234567": {}}}}'

###### POST - Update account blacklists:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID} -d '{"data": {"name": "Main Blacklis","numbers": {"+14151234567": {}}}}'

###### DELETE - Remove account blacklists:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}
