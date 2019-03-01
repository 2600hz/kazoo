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


#### v1 examples.

##### Get lists and their entries
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists

##### Create new list
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists -d '{"data": {"name": "List name"}}'

##### Get list with LIST_ID
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

##### Add new entry to list
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID} -d '{"data": {"pattern": "345"}}'

##### Delete list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

##### Get entry {LIST_ENTRY_ID} from list {LIST_ID}
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{LIST_ENTRY_ID}

##### Rewrite entry {LIST_ENTRY_ID} in list {LIST_ID}
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{LIST_ENTRY_ID} -d "{"data": {"132", "321"}}"

##### Delete entry from list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{LIST_ENTRY_ID}
