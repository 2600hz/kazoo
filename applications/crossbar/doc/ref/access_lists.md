### Access_lists

#### About Access_lists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cidrs` | Classless Inter-Domain Routing IP notation for use on the access lists | `array(string)` |   | `false`
`cidrs.[]` |   | `string` |   | `false`
`order` | Allow-Deny or Deny-Allow? | `string('allow,deny', 'deny,allow')` |   | `true`
`user_agent` | RegExp to match valid user agent strings | `string` |   | `false`


#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/access_lists

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/access_lists
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/access_lists

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/access_lists
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/access_lists

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/access_lists
```

