# Rate Limits

## About Rate Limits

The rate-limits API allows setting per-second and per-minute incoming SIP packets rate limits for devices and whole accounts (realms) that can be used at SBC level. The system level packet rate limits can also be set to protect the whole cluster.

### Modify Rate Limits Using API

Using Crossbar to modify rate limits is very simple. There are only three actions:

* `GET` - Gets the current rate limits on the document (account or device)
* `POST` - Updates the rate limits on the document (account or device)
* `DELETE` - Removes the rate limits object from the document (account or device)

JSON object has self-describing structure. The name of the root key is `rate_limits`.

The application-level system-wide configuration resides in `system_config/frontier` document and its syntax is equal to the account-level documents.

## Account-Level Document

Account-level document contains two sections:

* The `account` section sets rate limits for the entire account
* The `device` section sets rate limits for devices inside the account (if not overwritten by device-level rate limits)
* Both sections contain `registrations`, `invites` and `total_packets`" keys with integer values of possible number of packets within `per_minute` and `per_second` sections

**Doc Example**

```json
{
    "data": {
        "account": {
            "per_minute": {
                "registrations": 100,
                "invites": 100,
                "total_packets": 1000
            },
            "per_second": {
                "registrations": 5,
                "invites": 5,
                "total_packets": 20
            }
        },
        "device": {
            "per_minute": {
                "registrations": 10,
                "invites": 10,
                "total_packets": 100
            },
            "per_second": {
                "registrations": 2,
                "invites": 4,
                "total_packets": 10
            }
        }
    }
}
```

### Fetch Account's Rate Limits

> GET /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

### Update Account's Rate Limits

> POST /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"account": {"per_minute": {"total_packets": 3000},"per_second": {"total_packets": 50}},"device": {"per_minute": {"total_packets": 300},"per_second": {"total_packets": 5}}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

### Remove Account's Rate Limits

> DELETE /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

## Device-Level Document

Device-level document contains is one level "higher" and contains only the "device" part which contains `registrations`, `invites` and `total_packets` keys with integer values of possible number of packets within `per_minute` and `per_second` sections.

**Device-Level Example**

```json
{
    "data": {
        "per_minute": {
            "registrations": 60,
            "invites": 60,
            "total_packets": 180
        },
        "per_second": {
            "registrations": 2,
            "invites": 4,
            "total_packets": 7
        }
    }
}
```

## Fetch Device's Rate Limits

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits
```

## Update Device's Rate Limits

> POST /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"per_minute": {"registrations": 60,"invites": 60,"total_packets": 180},"per_second": {"registrations": 2,"invites": 4,"total_packets": 7}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits
```

## Remove Device's Rate Limits

> DELETE /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits
```
