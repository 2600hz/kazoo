

A temporal rule set is a collection of temporal rules that can be used in a callflow to match more that one rule. And can also be re-used.

#### Structure

The structure is really simple:

* `name`: A freindly name for your rule set.
* `temporal_rules`: A list of temporal rule IDs

Ex:
```
    "name": "July",
    "temporal_rules": [
        "452d5706f66377970996b2ec1c0fc04a",
        "b771eb3eee6ea48f4321e3cc31c050ab"
   ]
```

#### Crossbar

Using Crossbar to modify Temporal Rules Sets is very simple:

* GET - Gets the current set(s).
* PUT - Add a set.
* POST - Updates a set.
* DELETE - Removes a set.

##### Account Temporal Rules Sets URI

`/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets`

###### GET - Fetch account temporal_rules_sets:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets/{SET_ID

###### PUT - Add account temporal_rules_sets:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets -d '{"data": {"name": "July","temporal_rules": ["452d5706f66377970996b2ec1c0fc04a","b771eb3eee6ea48f4321e3cc31c050ab"]}}'

###### POST - Update account temporal_rules_sets:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets/{SET_ID} -d '{"data": {"name": "July","temporal_rules": ["452d5706f66377970996b2ec1c0fc04a","b771eb3eee6ea48f4321e3cc31c050ab"]}}'

###### DELETE - Remove account temporal_rules_sets:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/temporal_rules_sets/{SET_ID}
