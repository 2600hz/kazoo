### User_auth

#### About User_auth

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account_name` | The account name of the user | `string(1..128)` |   | `false`
`account_realm` | The account realm of the user | `string(1..64)` |   | `false`
`credentials` | A hash of the uses credentials | `string(1..64)` |   | `true`
`method` | The hash method | `string('md5', 'sha')` | `md5` | `false`
`phone_number` | A phone number assigned to the users account | `string(1..64)` |   | `false`


#### Create

> PUT v2/user_auth

```curl
curl -v http://{SERVER}:8000/v2/user_auth
```

#### Fetch

> GET v2/user_auth/{ID}

```curl
curl -v http://{SERVER}:8000/v2/user_auth/{ID}
```

#### Create

> PUT v2/user_auth/recovery

```curl
curl -v http://{SERVER}:8000/v2/user_auth/recovery
```

