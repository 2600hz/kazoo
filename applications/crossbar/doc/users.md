/*
Section: Crossbar
Title: Users
Language: en-US
*/

### Kazoo Users


#### Static Presence ID
You can set a presence ID at the user level. By adding: `presence_id` field on your user document. This will fix any presence issue regarding inbound/outbound call on a cellphone user.

#### QuickCall

Additional info for [QuickCall is on the wiki.](https://2600hz.atlassian.net/wiki/display/APIs/QuickCall+API)


```
http://{your kazoo domain}:8000/v1/accounts/{account_id}/users/{user_id}/quickcall/{number_to_call}?auth_token={API key or Auth Token}&cid-number={caller ID number}&cid-name={caller ID name}
```

If you are using HTTPs don't forget to update the port as well!
```
https://{your kazoo domain}:8443/v1/accounts/{account_id}/users/{user_id}/quickcall/{number_to_call}?auth_token={API key or Auth Token}&cid-number={caller ID number}&cid-name={caller ID name}
```

_Both cid-number and cid-name are optional_

Above are all the options of QuickCall (As Far as I can tell) They should be added to the examples.

Also under Resource Parameters add "cid-name" info to the "cid-number" info

esoare


#### Sync

See [the documentation on devices](../devices.md) for more info on `check-sync`.

One can add the field `"sync": true` to the JSON blob of a POST request to `/users/`,
in order to attempt a `check-sync` on every registered device this user has.
