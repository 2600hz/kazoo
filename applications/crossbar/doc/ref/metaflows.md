### Metaflows

#### About Metaflows

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#')` | `*` | `false`
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence | `integer` |   | `false`
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both', 'self', 'peer')` |   | `false`
`numbers` | A list of static numbers that the metaflow should match for | `object` |   | `false`
`patterns` | The metaflow patterns | `object` |   | `false`


#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/metaflows

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/metaflows
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/metaflows

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/metaflows
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/metaflows

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/metaflows
```

