### Search

#### About Search

The Search API allows queries on databases.
It will search in the accountdb if accountid is provided in the url path.
If not, it will search the accounts database using the authenticated account.

You can search for accounts by:
* name
* realm
* id

#### Search accounts database using authenticated account

##### Query Parameters

    t = document type
    q = view name
      * all views must be defined in _design/search
      * the view used is q=name => search_by_[name]
    v = value to search

> GET http://{SERVER}:8000/v2/search?t=account&q=name&v=nat

```curl
curl -x GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}/v2/search?t=account&q=name&v=nat
{
    "page_size":1,
    "start_key":["5ba01ad7ad1611d436b1860d8c552897","account", "nat"],
    "data": [
        {
            "id": "4c004a584ca5fda8084a5bcb24430ab9",
            "name": "Natoma Office",
            "realm": "eb0dcc.sip.90e9.com"
        }
    ],
    "revision": "04e880720e84b0536389bcc4de7e69e8",
    "request_id":"53975b8ebbc595680b0da753f55c3c19",
    "status":"success"
}
```

#### Search within accountid specified in URL

##### Query Parameters

    t = document type
    q = view name
      * all views must be defined in _design/search
      * the view used is q=name => search_by_[name]
    v = value to search

> GET http://{SERVER}:8000/v2/accounts/xxxxxxx/search?t=account&q=name&v=nat

```curl
    curl -x GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://localhost:8000/v2/accounts/xxxxxxx/search?t=user&q=name&v=j
```


##### Multi Search

######Query Parameters
    t = document type
    by_{view_name} = value

> GET http://{SERVER}:8000/v2/search/multi?t=account&by_name=test&by_realm=test&by_id=test

```curl
    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://localhost:8000/v2/search/multi?t=account&by_name=test&by_realm=test&by_id=test
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
