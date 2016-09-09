### Search

#### About Search

The Search API allows queries on databases.

You can search for accounts by:

* name
* realm
* id

#### Schema



#### Fetch search results

> GET /v2/search

> GET /v2/accounts/{ACCOUNT_ID}/search


    t = document type
    q = view name
      * all views must be defined in _design/search
      * the view used is q=name => search_by_[name]
    v = value to search

    database - the search api will search in the accountdb if API with /accounts/{ACCOUNT_ID}/ is used
               otherwise will search the accounts database using authenticated account


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/search?t=account&q=name&v=nat
```

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/xxxxxxx/search?t=user&q=name&v=j
```

```json
{
    "data": [
        {
            "id": "4c004a584ca5fda8084a5bcb24430ab9",
            "name": "Natoma Office",
            "realm": "eb0dcc.sip.90e9.com"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": [
        "5ba01ad7ad1611d436b1860d8c552897",
        "account",
        "nat"
    ],
    "status": "success"
}
```

#### Multi Search

> GET /v2/search/multi

> GET /v2/accounts/{ACCOUNT_ID}/search/multi

    t = document type
    by_{view_name} = value


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/search/multi?t=account&by_name=test&by_realm=test&by_id=test
```

```json
{
    "data": {
        "realm": [{
            "id": "8b77383bbdaebab09abc6372503335a5eab9a4f",
            "descendants_count": 1,
            "name": "test_account",
            "realm": "test.sip.2600hz.com"
        }],
        "name": [{
            "id": "8b77383bbdaebab09abc6372503335a5eab9a4f",
            "descendants_count": 1,
            "name": "test_account",
            "realm": "test.sip.2600hz.com"
        }, {
            "id": "3977383bbdaebab09abc6372503335a5eab9a4f",
            "descendants_count": 0,
            "name": "test_account_2",
            "realm": "62b63f.sip.2600hz.com"
        }],
        "id": []
    },
    "status": "success",
}
```
