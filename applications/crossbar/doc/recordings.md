### Recordings

#### About Recordings

Recordings endpoint provides a way to access call recordings.

#### Fetch recordings

> GET /v2/accounts/{ACCOUNT_ID}/recordings
> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/recordings

lists the call recording with pagination and filtering

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings
```

#### Fetch recording

> GET /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

gets a specific recording document

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

#### Fetch recording media

> GET /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

gets a specific recording document attachment if available.
mind `Accept` header in example below.
for clients that do not support setting the `Accept` header, a querystring parameter can be included: ?accept=text/html

optional parameter `inline` true|false

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: audio/mpeg" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```
