### Directories

#### About Directories

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`confirm_match` | When one match is found, require caller to confirm the match before connecting | `boolean` | `true` | `false`
`max_dtmf` | Cap the number of DTMF characters collected from a caller, 0 for unlimited | `integer` | `0` | `false`
`min_dtmf` | How many DTMF characters to collect from a caller before processing the directory | `integer` | `3` | `false`
`name` | The name of the directory | `string(1..)` |   | `true`
`sort_by` | What field to sort on in matching documents when a caller enters characters | `string('first_name', 'last_name')` | `last_name` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/directories

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/directories

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/directories/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/directories/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/directories/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/directories/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{ID}
```

