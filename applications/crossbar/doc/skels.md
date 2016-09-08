<!--
Section: Crossbar
Title: Skels
Language: en-US
-->

### Skels

* [Description](#description)
* [Stuture](#structure)
    * [Default Properties](#default-properties)
    * [Conditional Properties](#conditional-properties)
* [Crossbar](#crossbar)
    * [GET](#get-get-all-resources)
    * [GET](#get-get-a-resource)
    * [PUT](#put-create-a-resource)
    * [POST](#post-update-a-resource)
    * [DELETE](#delete-delete-a-resource)

#### About Skels

Description of the skel module goes here.


#### Schema

Property  | Description | Type | Validation
------------- | ------------- | ------------- | -------------
name   | your name obviously | String | required
email  | email, we won't spam | String | required, email
sutff | this is some good stuff | Array | none
age   | your age obviously | Integer | none
city  | where you live | String | none
zipcode  | also where you live | Integer | none


#### Get all resources

> GET /v2/accounts/{ACCOUNT_ID}/skels

More description if needed!

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/skels
```

```json
{
    "data": [
        {
            "name": "John",
            "email": "john@email.com",
            "stuff": [
                "stuff1"
            ],
            "age": 30,
            "city": "San francisco",
            "zipcode": 94109
        },
        {
            "name": "Jane",
            "email": "Jane@email.com",
            "stuff": [
                "stuff2"
            ],
            "age": 28,
            "city": "San francisco",
            "zipcode": 94109
        }
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Create a resource

> PUT /v2/accounts/{ACCOUNT_ID}/skels

More description if needed!

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "name": "Jane",
        "email": "Jane@email.com",
        "stuff": ["stuff2"],
        "age": 28,
        "city": "San francisco",
        "zipcode": 94109
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/skels
```

```json
{
    "data": {
        "name": "Jane",
        "email": "Jane@email.com",
        "stuff": [
            "stuff2"
        ],
        "age": 28,
        "city": "San francisco",
        "zipcode": 94109
    },
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Delete a resource

> DELETE /v2/accounts/{ACCOUNT_ID}/skels/{THING}

More description if needed!

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/skels/{THING}
```

```json
{
    "data": {},
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Get a resource

> GET /v2/accounts/{ACCOUNT_ID}/skels/{THING}

More description if needed!

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/skels/{THING}
```

```json
{
    "data": {
        "name": "Jane",
        "email": "Jane@email.com",
        "stuff": [
            "stuff2"
        ],
        "age": 28,
        "city": "San francisco",
        "zipcode": 94109
    },
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Update a resource

> POST /v2/accounts/{ACCOUNT_ID}/skels/{THING}

> PATCH /v2/accounts/{ACCOUNT_ID}/skels/{THING}

More description if needed!

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "name": "Jane",
        "email": "jane@email.com",
        "stuff": ["some new stuff"],
        "age": 29,
        "city": "San francisco",
        "zipcode": 94109
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/skels/{THING}
```

```json
{
    "data": {
        "name": "Jane",
        "email": "jane@email.com",
        "stuff": [
            "some new stuff"
        ],
        "age": 29,
        "city": "San francisco",
        "zipcode": 94109
    },
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```
