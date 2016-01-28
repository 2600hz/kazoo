### Cccps

#### About Cccps

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`active` | Show's whether CID/PIN active | `boolean` | `false` | `false`
`cid` | CID to authorize | `string` |   | `false`
`outbound_cid` | CID you would like to call out with | `string` |   | `false`
`pin` | PIN to authorize | `string` |   | `false`
`user_id` | The ID of the user object that 'owns' cid/pin | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/cccps

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/cccps

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/cccps/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/cccps/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/cccps/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/cccps/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/cccps/{ID}
```

