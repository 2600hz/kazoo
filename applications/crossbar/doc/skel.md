<!--
Section: Crossbar
Title: Skel
Language: en-US
-->

### Skel

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

### Description

Description of the skel module goes here.

### Structure

#### Default Properties

Property  | Description | Type | Validation
------------- | ------------- | ------------- | -------------
name   | your name obviously | String | required
email  | email, we won't spam | String | required, email
sutff | this is some good stuff | Array | none

##### Conditional Properties

Property  | Description | Type | Validation
------------- | ------------- | ------------- | -------------
age   | your age obviously | Integer | none
city  | where you live | String | none
zipcode  | also where you live | Integer | none

``` javascript
{
    "name": "John",
    "email": "john@email.com",
    "stuff": [
        "stuff1"
    ],
    "age": 30,
    "city": "San francisco",
    "zipcode": 94109

}x
```

### Crossbar

Using Crossbar to modify Skel is very simple:

Verb  | Url | Description
------------- | ------------- | -------------
[GET](#get-get-all-resources)   | `v2/accounts/{account_id}/skel` | get all resources
[GET](#get-get-a-resource) | `v2/accounts/{account_id}/skel/{skel_id}` | get current resource
[PUT](#put-create-a-resource)  | `v2/accounts/{account_id}/skel` | create a resource
[POST](#post-update-a-resource)  | `v2/accounts/{account_id}/skel/{skel_id}` | update current resource
[DELETE](#delete-delete-a-resource)  | `v2/accounts/{account_id}/skel/{skel_id}` | delete current resource


#### GET - Get all resources

##### Descripton

More description if needed!

##### Request

    curl -v -X GET -H "X-Auth-Token: {auth_token}" http://server:8000/v2/accounts/{account_id}/skel

##### Response

``` javascript
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
    "request_id": "{request_id}",
    "status": "success",
    "auth_token": "{auth_token}"
}
```

#### GET - Get a resource

##### Descripton

More description if needed!

##### Request

    curl -v -X GET -H "X-Auth-Token: {auth_token}" http://server:8000/v2/accounts/{account_id}/skel/{id}

##### Response

``` javascript
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
    "request_id": "{request_id}",
    "status": "success",
    "auth_token": "{auth_token}"
}
```

#### PUT - Create a resource:

##### Descripton

More description if needed!

##### Request

    curl -v -X PUT -H "X-Auth-Token: {auth_token}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{account_id}/skel -d '{"name": "Jane","email": "Jane@email.com","stuff": ["stuff2"],"age": 28,"city": "San francisco","zipcode": 94109}'

##### Response

``` javascript
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
    "request_id": "{request_id}",
    "status": "success",
    "auth_token": "{auth_token}"
}
```

#### POST - Update a resource:

##### Descripton

More description if needed!

##### Request

    curl -v -X POST -H "X-Auth-Token: {auth_token}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{account_id}/skel -d '{"name": "Jane","email": "jane@email.com","stuff": ["some new stuff"],"age": 29,"city": "San francisco","zipcode": 94109}'

##### Response

``` javascript
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
    "request_id": "{request_id}",
    "status": "success",
    "auth_token": "{auth_token}"
}
```

#### DELETE - Delete a resource:

##### Descripton

More description if needed!

##### Request

    curl -v -X DELETE -H "X-Auth-Token: {auth_token}" http://server:8000/v2/accounts/{account_id}/skel/{id}

##### Response

``` javascript
{
    "data": {},
    "request_id": "{request_id}",
    "status": "success",
    "auth_token": "{auth_token}"
}
```