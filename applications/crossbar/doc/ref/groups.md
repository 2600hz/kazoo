# Groups

## About Groups

#### Schema

Validator for the group



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`endpoints` | Endpoints included into group | `object()` | `{}` | `true` | `supported`
`music_on_hold.media_id` | The ID of a media object that should be used as music on hold | `string(0..128)` |   | `false` |  
`music_on_hold` | The music on hold parameters | `object()` | `{}` | `false` | `beta`
`name` | A friendly name for the group | `string(1..128)` |   | `true` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/groups

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/groups

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/groups/{GROUP_ID}
```

