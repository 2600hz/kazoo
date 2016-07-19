### Directories

#### About Directories

Directories provide the ability to route a caller to a user by having the caller enter DTMF corresponding to the directory users' first orlast names (versus having to know the user's extension).

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

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/directories

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/directories

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

#### Fetch a directory listing

> GET /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

#### Fetch a directory as a PDF

It is possible to fetch the directory as a PDF for download (such as a company direcotry, a sales department directory, etc)

> GET /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf"
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

If your client doesn't support setting the `Accept` header, you can append `?accept=pdf` to the URI and Kazoo will pretend you sent the proper `Accept` header

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```
