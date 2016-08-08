### Callflows

#### About Callflows

Callflows are the instructions Kazoo uses to process a call. A callflow includes a list of numbers or regex patterns used by Kazoo to determine what callflow is used when a call comes in for an account. The `flow` parameter defines the tree of actions, allowing branching (such as in the `menu` action) and chaining actions together. You can also branch to other callflows and execute its `flow` (useful to avoid recreating the same sub-flow structure).

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`featurecode` | When the callflow is used as a featurecode this object tracks the intended match of the pattern and name of the feature | `object` |   | `false`
`featurecode.name` |   | `string(1..128)` |   | `false`
`featurecode.number` |   | `string(1..30)` |   | `false`
`flow` | A callflow node defines a module to execute, data to provide to that module, and zero or more children to branch to | `object` |   | `true`
`flow.children` | Children callflows | `object` | `{}` | `false`
`flow.data` | The data/arguments of the callflow module | `object` | `{}` | `true`
`flow.module` | The name of the callflow module to excute at this node | `string(1..64)` |   | `true`
`metaflow` | Actions applied to a call outside of the normal callflow, initiated by the caller(s) | `#/definitions/metaflows` |   | `false`
`numbers` | A list of static numbers that the callflow should execute for | `array(string(1..36))` | `[]` | `false`
`numbers.[]` |   | `string` |   | `false`
`patterns` | A list of regular expressions that the callflow should execute for, with optional capture groups | `array(string(1..))` | `[]` | `false`
`patterns.[]` |   | `string` |   | `false`

#### Fetch an account's callflows

> GET /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Create a new callflow

> PUT /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Remove a callflow

> DELETE /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Fetch a callflow's details

> GET /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Patch a callflow object

> PATCH /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Change a callflow object

> POST /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```
