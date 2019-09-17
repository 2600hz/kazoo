### Search

#### About Search

The Search API allows query a value on databases. You can query different type of entity commonly like accounts (on `accounts` database), users, devices, callflows, numbers. It possible to search for any document base on it's ID.

Generally you have to provide which document type to search, which field in the document should be searched and a value to search with. How to specify these query parameters is depend on you want to search with a single value or multiple values.

##### Databases To Search On, Fields To Search With

Search for accounts is done on `accounts` database. The parameters that you can search with are:

* `name`: Account's name
* `realm`: Account's realm
* `id`: Account's ID

For other type of documents search is done on Account's database. The parameters to search with are:

* `name`: `name` field of the document if it has one or the ID of the document
* `name_and_number`: search either by `name` field or by `numbers`
* `number`: search by `numbers` field, usually used to search callflow.

##### Document Types

You have to specify what kind of piece of information is you're looking for (`t` query string parameter). Typically you're interested to search for an account.

Frequent types are:

* `account`
* `callflow`
* `device`
* `directory`
* `faxbox`
* `limits`
* `media`
* `menu`
* `user`
* `vmbox`

##### Create Custom Search Views (Advanced)

Search in Kazoo is possible with specific CouchDB view with design document ID `search` in account's database and `accounts` database. View index defined in this design document are exposing the common fields (`name`, `id`, etc...) in documents for search.

With that in mind, a system administrator can create a custom index to expose extra fields out of document to view. This requires full understanding of how to write CouchDB design documents and a bit of the internal structure of the document in the database. You can look at `{ACCOUNT_DB}/_design/search` to see how Kazoo default search view is implemented.

It's require to name your custom search view index name prefixed with `search_by_{YOUR_CUSTOM_VIEW_NAME}`, which conventionally `{YOUR_CUSTOM_VIEW_NAME}` is the name of the field in the document that you're creating this view for.

#### Search with Single Value

To look up a single value in the specified field. These should defined in request query string.

> GET /v2/accounts/{ACCOUNT_ID}/search

##### Query String Parameters

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`t` | document type | `string()` |  | `true`
`q` | the field to look in for value | `string()` |  | `true`
`v` | value to search for | `string()` or `boolean()` |  | `true`


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/6134cc9aa43ffaee3e3f0c9a84113d6e/search?t=account&q=name&v=nat
```

##### Response

```json
{
    "page_size": 1,
    "start_key": "g2wAAAADbQAAACA2MTM0Y2M5YWE0M2ZmYWVlM2UzZjBjOWE4NDExM2Q2ZW0AAAAHYWNjb3VudG0AAAAHbmF0ZmZmMGo",
    "data": [
        {
            "id": "a391d64a083b99232f6d2633c47432e3",
            "descendants_count": 2,
            "name": "natalie",
            "realm": "natalie.2600hz.com"
        }
    ],
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}",
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

##### Searching for an Account

If you're searching for an account you can remove `/accounts/{ACCOUNT_ID}` from the URL.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/search?t=account&q=name&v=nat
```

#### Multi Search

To search with multiple values in a single shot. Search parameters should defined in request query string.

> GET /v2/accounts/{ACCOUNT_ID}/search/multi


##### Query String Parameters

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`t` | document type | `string()` |  | `true`
`by_{view_name}` | the values to search for in `{view_name}` results | `string()` |  | `true`

Here `{view_name}` is referring in what field is available to search. See [above](#databases-to-search-on-fields-to-search-with).

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/accounts/6134cc9aa43ffaee3e3f0c9a84113d6e/v2/search/multi?t=account&by_name=test&by_realm=test&by_id=test
```

##### Response

```json
{
    "data": {
        "name": [
            {
                "id": "a391d64a083b99232f6d2633c47432e3",
                "descendants_count": 2,
                "name": "test",
                "realm": "test.2600hz.com"
            }
        ],
        "realm": [
            {
                "id": "a391d64a083b99232f6d2633c47432e3",
                "descendants_count": 2,
                "name": "test",
                "realm": "test.2600hz.com"
            }
        ]
    },
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}",
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

##### Searching for Account

If you're searching for account you can remove `/accounts/{ACCOUNT_ID}` from the URL.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/search/multi?t=account&by_name=test&by_realm=test
```
