### Callflows

#### About Callflows

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`featurecode` | When the callflow is used as a featurecode this object tracks the intended match of the pattern and name of the feature | `object` |   | `false`
`featurecode.name` |   | `string` |   | `false`
`featurecode.number` |   | `string` |   | `false`
`flow` | A callflow node defines a module to execute, data to provide to that module, and one or more children to branch to | `object` |   | `true`
`flow.children` | Children callflows | `object` | `{}` | `false`
`flow.data` | The data/arguments of the callflow module | `object` | `{}` | `true`
`flow.module` | The name of the callflow module to excute at this node | `string` |   | `true`
`metaflow` | Actions applied to a call outside of the normal callflow, initiated by the caller(s) |   |   | `false`
`numbers` | A list of static numbers that the callflow should execute for | `array` | `[]` | `false`
`numbers.[]` |   | `string` |   | `false`
`patterns` | A list of regular expressions that the callflow should execute for, with optional capture groups | `array` | `[]` | `false`
`patterns.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/callflows

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/callflows

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/callflows/{MEDIAID}
```

