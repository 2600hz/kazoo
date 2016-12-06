### Directories

#### About Directories

Directories provide the ability to route a caller to a user by having the caller enter DTMF corresponding to the directory users' first or last names (versus having to know the user's extension).

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`confirm_match` | When one match is found, require caller to confirm the match before connecting | `boolean` | `true` | `false`
`max_dtmf` | Cap the number of DTMF characters collected from a caller, 0 for unlimited | `integer` | `0` | `false`
`min_dtmf` | How many DTMF characters to collect from a caller before processing the directory | `integer` | `3` | `false`
`name` | The name of the directory | `string(1..)` |   | `true`
`sort_by` | What field to sort on in matching documents when a caller enters characters | `string('first_name', 'last_name')` | `last_name` | `false`
`users` | The list of users associated with this directory | `array(string)` | `[]` | `false`
`users.[]` |   | `string` |   | `false`


#### List directories

> GET /v2/accounts/{ACCOUNT_ID}/directories

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "77dfb38ff2353624e35bf4df91acda94",
            "name": "SmartPBX Directory"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/directories

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

#### Fetch a directory listing

> GET /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/77dfb38ff2353624e35bf4df91acda94
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "confirm_match": false,
        "id": "77dfb38ff2353624e35bf4df91acda94",
        "max_dtmf": 0,
        "min_dtmf": 3,
        "name": "SmartPBX Directory",
        "sort_by": "last_name",
        "ui_metadata": {
            "origin": "voip",
            "ui": "monster-ui",
            "version": "3.23"
        },
        "users": []
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

It is possible to fetch the directory as a PDF for download (such as a company directory, a sales department directory, etc).

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf"
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

```
Streams back a PDF document.
```

If your client does not support setting the `Accept` header, you can append `?accept=pdf` to the URI and Kazoo will pretend you sent the proper `Accept` header.

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```
