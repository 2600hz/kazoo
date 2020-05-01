# Lists

Lists API provide to create list of numbers ore prefix numbers to match and route calls.

In the new behavior of List API, list is a collection of list entries (number, user, prefix numbers) allows you more flexibility to create and assign multi entries to different match list.

#### Schema

Schema for a match list



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`description` | A friendly list description | `string(1..128)` |   | `false` |  
`name` | A friendly match list name | `string(1..128)` |   | `true` |  
`org` | Full legal name of the organization | `string()` |   | `false` |  



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists
```


#### Add new list (beware: no entries)

> PUT /v2/accounts/{ACCOUNT_ID}/lists

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"name": "list name"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists
```


#### Delete list and its entries

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```


#### Get list properties (doesn't return entries)

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```


#### Updating list (without entries)

> PATCH /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"description": "desc"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```


#### Rewrite list

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"name": "New List name"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```


#### Delete all entries from list

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```


#### Get list entries

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```


#### Add an entry to a list

> PUT /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"number": "0123", "displayname" : "List Entry"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```


#### Delete entry from the list

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}
```


#### List entry properties

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}
```


#### Update list entry

> PATCH /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"firstname" : "First name"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}
```

#### Replace list entry

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"number": "0123", "displayname" : "New List Entry"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}
```


#### Add photo to List entry

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}/photo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}/photo
```


#### List entry vcard

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}/vcard

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries/{LIST_ENTRY_ID}/vcard
```
