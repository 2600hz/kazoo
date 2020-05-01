# Menus

## About Menus

#### Schema

Schema for a menus



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`allow_record_from_offnet` | Determines if the record pin can be used by external calls | `boolean()` | `false` | `false` | `supported`
`flags.[]` |   | `string()` |   | `false` | `supported`
`flags` | Flags set by external applications | `array(string())` |   | `false` | `supported`
`hunt` | Determines if the callers can dial internal extensions directly | `boolean()` | `true` | `false` | `supported`
`hunt_allow` | A regular expression that an extension the caller dialed must match to be allowed to continue | `string(1..256)` |   | `false` | `supported`
`hunt_deny` | A regular expression that if matched does not allow the caller to dial directly | `string(1..256)` |   | `false` | `supported`
`interdigit_timeout` | The amount of time (in milliseconds) to wait for the caller to press the next digit after pressing a digit | `integer()` |   | `false` | `supported`
`max_extension_length` | The maximum number of digits that can be collected | `integer()` | `4` | `false` | `supported`
`media.exit_media` | When a call is transferred from the menu after all retries exhausted this media can be played (prior to transfer if enabled) | `boolean() | string(3..2048)` |   | `false` | `supported`
`media.greeting` | The ID of a media object that should be used as the menu greeting | `string(3..2048)` |   | `false` | `supported`
`media.invalid_media` | When the collected digits don't result in a match or hunt this media can be played | `boolean() | string(3..2048)` |   | `false` | `supported`
`media.transfer_media` | When a call is transferred from the menu, either after all retries exhausted or a successful hunt, this media can be played | `boolean() | string(3..2048)` |   | `false` | `supported`
`media` | The media (prompt) parameters | `object()` | `{}` | `false` | `supported`
`name` | A friendly name for the menu | `string(1..128)` |   | `true` | `supported`
`record_pin` | The pin number used to record the menu prompt | `string(3..6)` |   | `false` | `supported`
`retries` | The number of times a menu should be played until a valid entry is collected | `integer()` | `3` | `false` | `supported`
`suppress_media` | Determines if the playing of 'Invalid Entry' is suppressed. | `boolean()` | `false` | `false` | `supported`
`timeout` | The amount of time (in milliseconds) to wait for the caller to begin entering digits | `integer()` |   | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/menus

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/menus

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
```

