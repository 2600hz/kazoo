### Things

#### About Things

Things do stuff. The more things you have, the more stuff you can do.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
name | The name of the thing | string(32) | | Y
stuff | A list of stuff to do | array(string()) | [] | N

#### Fetch a summary list of Things

> GET /v2/accounts/{ACCOUNT_ID}/things

```curl
curl -x GET \
     -H "X-Auth-Token: {AUTH_TOKEN}" \
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [{"name":"thing1", "id":"{THING_ID}"}],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Create a new Thing

If your client supports it, the full URI of the successfully-created thing will be in the `Location` header

> PUT /v2/accounts/{ACCOUNT_ID}/things

```curl
curl -X PUT \
     -H "X-Auth-Token: {AUTH_TOKEN}" \
     -d '{"data":{"name":"thing2"}}'
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {"name":"thing2", "id":"{THING_ID}"}],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch the details of a Thing

> GET /v2/accounts/{ACCOUNT_ID}/things/{THING_ID}

```curl
curl -X GET \
     -H "X-Auth-Token: {AUTH_TOKEN}" \
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things/{THING_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {"name":"thing1",
             "id":"{THING_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Edit the details of a Thing

> POST /v2/accounts/{ACCOUNT_ID}/things/{THING_ID}

```curl
curl -X POST
     -H "X-Auth-Token: {AUTH_TOKEN}"
     -d '{"data":{"name":"thing2", "
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things/{THING_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {"name":"thing2",
             "id":"{THING_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Patch a field in a Thing

> PATCH /v2/accounts/{ACCOUNT_ID}/things/{THING_ID}

```curl
curl -X PATCH
     -H "X-Auth-Token: {AUTH_TOKEN}"
     -d '{"data":{"stuff":["object"]}}
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things/{THING_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {"name":"thing2",
             "id":"{THING_ID}",
             "stuff":["object"]
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Delete a Thing

> DELETE /v2/accounts/{ACCOUNT_ID}/things/{THING_ID}

```curl
curl -X DELETE
     -H "X-Auth-Token: {AUTH_TOKEN}"
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things/{THING_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {"name":"thing2",
             "id":"{THING_ID}",
             "stuff":["object"]
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Execute the stuff of a Thing

When you want to get stuff done for a thing!

> POST /v2/accounts/{ACCOUNT_ID}/things/{THING_ID}/stuff

```curl
curl -X POST
     -H "X-Auth-Token: {AUTH_TOKEN}"
     http://{SERVER}/v2/accounts/{ACCOUNT_ID}/things/{THING_ID}/stuff
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [{"object":"done"}]
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```
