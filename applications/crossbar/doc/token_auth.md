

Authentication tokens are generated using one of the authentication endpoints exposed by Crossbar. See [User Authentication](./user_authentication.md) and [API Authentication](./api_authentication.md) as examples of generating authentication tokens.

Once you have an authentication token, you can access various Crossbar resource endpoints to manipulate the system or your account (provided you have the access).

Authentication tokens refresh their pvt\_modified timestamp each time they are used in an API request. Once an authentication token's pvt\_modified timestamp has passed a configurable timeout (usually one hour), it is automatically cleaned up by the system and no longer valid.

#### Token Restrictions

The authentication token can be created with restrictions on what resource URIs (and HTTP methods) can be accessed by the requestor. This payload is added to the authentication payload used in any of the authentication methods provided ([User](./user_authentication.md), [API](./api_authentication.md), etc).

For example, when creating an authentication token via [API key](./api_authentication.md), include the following object to restrict the resultant authentication token to read-only:

```json
{
    "data":{
        "api_key":"{API_KEY}",
        "restrictions":{
            "get":["#"]
        }
    }
}
```

AMQP binding tokens are used (`#` and `*`) to denote wildcards. An example with more fine-grained restrictions:

``json
{
    "data":{
        "api_key":"{API_KEY}",
        "restrictions":{
            "get":[
                "accounts/{ACCOUNT_ID}/users",
                "accounts/{ACCOUNT_ID}/users/*",
                "accounts/{ACCOUNT_ID}/users/*/*"
            ],
            "put":[
                "accounts/{ACCOUNT_ID}/users"
            ],
            "post":[
                "accounts/{ACCOUNT_ID}/users/*"
            ],
            "delete":[
                "accounts/{ACCOUNT_ID}/users/*"
            ]
        }
    }
}
```

This would restrict the authentication token to only be able to access {ACCOUNT_ID}'s users resource and perform all of the CRUD actions (as well as quickcall and channel listings for a user). We can simply this restrictions object by using `*` for the method and `#` to match any URI with `/users`:

```json
{
    "data":{
        "api_key":"{API_KEY}",
        "restrictions":{
            "*":[
                "accounts/{ACCOUNT_ID}/users/#"
            ]
        }
    }
}
```

Here the `#` matches 0 or more segments after `/users`.

#### API Endpoint

URL segment: `/token_auth`

#### Sample cURL Requests

##### Delete an authentication token

If you'd like to invalidate an authentication token programmatically (versus letting the system expire the token), you can issue a `DELETE`:

```shell
curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/token_auth -d '{"data": {}}'
```

```json
{
    "request_id": "1465c97856e5a77636b7476cd59916f7",
    "revision": "undefined",
    "status": "success"
}
```
