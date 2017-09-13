### IPs

#### About IPs

The IPs API allows users to manage the IP addresses assigned to their accounts. In the future they will be able to fully manage dedicated IPs.

The common use case is adding proxy IPs that must be used when routing calls to upstream resources. If the upstream requires traffic to come from a specific set of IPs, adding those IPs here will cause outbound calls to carriers to be routed through the IP(s) supplied.

#### Adding IPs to the system

IPs need to be configured by the system admin using the `sup kazoo_ips_maintenance add {IP} {ZONE} {HOST}` command:

```shell
sup kazoo_ips_maintenance add "1.2.3.4" "us-east" "proxy1.us-east.myswitch.com"
added IP 1.2.3.4 to available dedicated ips
```

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`{IP}` | The IP address of the proxy | `string` |   | `true`
`{ZONE}` | The Kazoo zone this proxy is assigned  | `string` |   | `true`
`{HOST}` | The Hostname associated with the IP | `string` | | `true`

Once you've added IPs to the system, you can assign those to different customer accounts to proxy their calls through using the below Crossbar APIs.

#### Schema

IP addresses assigned to the account



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`ips.[]` |   | `string()` |   | `false`
`ips` | List of IP addresses | `array(string())` |   | `false`



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips

This can take an optional query string parameter `zone` to filter the results.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "ip": "1.2.3.4",
            "zone": "us-east"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Assign IPs to the account

> POST /v2/accounts/{ACCOUNT_ID}/ips

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":["1.2.3.4"]}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "assigned_to": "{ACCOUNT_ID}",
            "host": "proxy1.us-east.myswitch.com",
            "id": "1.2.3.4",
            "ip": "1.2.3.4",
            "status": "assigned",
            "type": "dedicated_ip",
            "zone": "us-east"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove an IP assignment

> DELETE /v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "host": "proxy1.us-east.myswitch.com",
        "id": "1.2.3.4",
        "ip": "{IP_ADDRESS}",
        "status": "available",
        "type": "dedicated_ip",
        "zone": "us-east"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch details of the assignment

> GET /v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "assigned_to": "{ACCOUNT_ID}",
        "host": "proxy1.us-east.myswitch.com",
        "id": "{IP_ADDRESS}",
        "ip": "{IP_ADDRESS}",
        "status": "assigned",
        "type": "dedicated_ip",
        "zone": "us-east"
    },
    "request_id":"{REQUEST_ID}",
    "revision":"{REVISION}",
    "status":"success"
}
```

#### Assign a single IP to the account

> POST /v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP_ADDRESS}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "assigned_to": "{ACCOUNT_ID}",
        "host": "proxy1.us-east.myswitch.com",
        "id": "{IP_ADDRESS}",
        "ip": "{IP_ADDRESS}",
        "status": "assigned",
        "type": "dedicated_ip",
        "zone": "us-east"
    },
    "request_id":"{REQUEST_ID}",
    "revision":"{REVISION}",
    "status":"success"
}
```

#### Fetch hosts

> GET /v2/accounts/{ACCOUNT_ID}/ips/hosts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/hosts
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "proxy1.us-east.myswitch.com"
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch zone listings

> GET /v2/accounts/{ACCOUNT_ID}/ips/zones

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/zones
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "us-east"
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch IPs assigned to this account

> GET /v2/accounts/{ACCOUNT_ID}/ips/assigned

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/assigned
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "ip": "1.2.3.4",
            "zone": "us-east"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
