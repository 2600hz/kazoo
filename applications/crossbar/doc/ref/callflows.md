### Callflows

#### About Callflows

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`featurecode` | When the callflow is used as a featurecode this object tracks the intended match of the pattern and name of the feature | `object` |   | `false`
`featurecode.name` |   | `string(1..128)` |   | `false`
`featurecode.number` |   | `string(1..30)` |   | `false`
`flow` | A callflow node defines a module to execute, data to provide to that module, and one or more children to branch to | `object` |   | `true`
`flow.children` | Children callflows | `object` | `{}` | `false`
`flow.data` | The data/arguments of the callflow module | `object` | `{}` | `true`
`flow.module` | The name of the callflow module to excute at this node | `string(1..64)` |   | `true`
`metaflow` | Actions applied to a call outside of the normal callflow, initiated by the caller(s) | `#/definitions/metaflows` |   | `false`
`numbers` | A list of static numbers that the callflow should execute for | `array(string(1..36))` | `[]` | `false`
`numbers.[]` |   | `string` |   | `false`
`patterns` | A list of regular expressions that the callflow should execute for, with optional capture groups | `array(string(1..))` | `[]` | `false`
`patterns.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

