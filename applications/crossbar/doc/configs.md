### Configs

Certain settings are available per account. What settings are understood by Kazoo
is determined by account_config.{CONFIG_ID}.json schemas. Please note not all
possible configuration options are available on per-account basis, some (most)
of them are system-wide.

#### About Configs

All requests returns merged configuration. Following documents are merged (if they
are exists): account-specific configuration, account's reseller account-specific
configuration, then default section of system config of the same name, and then
default values deduced from system config schema.

On PUT/POST/PATCH operations a difference of parent configuration is calculated and written
as account-specific config document. Therefore if a provided value is equal to default
it will not be stored in account-specific config document.

If the difference with default is empty, then account-specific configuration document
will be deleted.


#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

```json
{
  "data": {
    "entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "moderator_entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "moderator_exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "number_timeout": 5000,
    "pin_timeout": 5000,
    "support_name_announcement": true,
    "id": "configs_conferences"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

PUT is completely equal to POST

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d @data.json \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

Request's data.json:
```json
{
  "data": {
    "entry_tone": "tone_stream://%(1000,0,2600)",
    "exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "moderator_entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "moderator_exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "number_timeout": 5000,
    "pin_timeout": 5000,
    "support_name_announcement": true,
    "id": "configs_conferences"
  }
}
```

Response:
```json
{
  "data": {
    "entry_tone": "tone_stream://%(1000,0,2600)",
    "exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "moderator_entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "moderator_exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "number_timeout": 5000,
    "pin_timeout": 5000,
    "support_name_announcement": true,
    "id": "configs_conferences"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```


#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"pin_timeout":3000}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

```json
{
  "data": {
    "entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "moderator_entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "moderator_exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "number_timeout": 5000,
    "pin_timeout": 3000,
    "support_name_announcement": true,
    "id": "configs_conferences"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

Configuration is effectively reset to default:

```json
{
  "data": {
    "entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "moderator_entry_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)",
    "moderator_exit_tone": "tone_stream://v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)",
    "number_timeout": 5000,
    "pin_timeout": 5000,
    "support_name_announcement": true,
    "id": "configs_conferences"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```
