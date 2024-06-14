# Call Recordings

## About Recordings

Recordings endpoint provides a way to access call recordings.

## Fetch recordings

> GET /v2/accounts/{ACCOUNT_ID}/recordings
> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/recordings

Lists the call recording with pagination and filtering.
soft deleted docs are filtered out by default to include them add `include=soft_delete` to the Query string
See Patch section below for info on soft_delete

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings?include=soft_delete
```

## Fetch recording media or document

> GET /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

Gets a specific recording document.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

Gets a specific recording document attachment if available.
Mind the `Accept` header in example below.
For clients that do not support setting the `Accept` header, a query string parameter can be included: `?accept=audio/mpeg`.

Optional parameter `inline` can be either `true` or `false`.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: audio/mpeg" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

## Remove a recording

> DELETE /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

This will delete the metadata document. If the binary data is stored on the metadata document (instead of on a storage provider), it will also be deleted. Recordings stored on storage providers will not be deleted.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

## Patch a call recording doc

> PATCH /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

`"soft_delete"` and `"soft_restore"` objects can be PATCHed so that the call recordings document appear to be deleted to normal users but still actually exist in the DB.

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data" : {"soft_delete": {"deleted_by": {"account_id": {ACCOUNT_ID}, "user_id": {USER_ID}},
				                   "deleted_at": "63755032655"}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
{
    "data": {
        ...data...
        "soft_delete": {
            "deleted_by": {
                "user_id": {USER_ID},
                "account_id": {ACCOUNT_ID}
            },
            "deleted_at": 63755032655
        },
        ...data...
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "timestamp": "2020-05-12T15:36:52Z",
    "version": {VERSION},
    "node": {NODE_ID},
    "status": "success",
    "auth_token": {AUTH_TOKEN}
}
```

## Patch a Collection of call recording docs

> PATCH /v2/accounts/{ACCOUNT_ID}/recordings/collection

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{
	"data" : {"recordings": [ {RECORDING_ID_1},
                              {RECORDING_ID_2],
                "soft_delete": {"deleted_by": {"account_id": {ACCOUNT_ID}, "user_id": {USER_ID}},
				                "deleted_at": "63755032655"}}}}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/collection
{
    "data": {
        "success": {
            {RECORDING_ID_1}: {
                ...data...
                "soft_delete": {
                    "deleted_by": {
                        "user_id": {USER_ID},
                        "account_id": {ACCOUNT_ID}
                    },
                    "deleted_at": 63755032655
                },
                ...data...
            },
           {RECORDING_ID_2}: {
                ...data...
                "soft_delete": {
                    "deleted_by": {
                        "user_id": {USER_ID},
                        "account_id": {ACCOUNT_ID}
                    },
                    "deleted_at": 63755032655
                },
                ...data...
            }
        }
    },
    "timestamp": "2020-05-12T15:49:09Z",
    "version": {VERSION},
    "node": {NODE},
    "request_id": {REQUEST_ID}",
    "status": "success",
    "auth_token": {AUTH_TOKEN}
}
```
