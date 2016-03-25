

The IPs API allows users to list the address assigned to their accounts.  In the future they will be able to fully manage dedicated IPs.

#### Schema

* `ip`: The IP address
* `zone`: The zone the IP currently routes to, this will be an arbitrary value

#### APIs

##### List Assigned IPs

Request:

`GET` `http://searver.com:8000/v1/accounts/{ACCOUNT_ID}/ips/assigned`

Responce:

```
{
    "data": [{
        "ip": "192.168.1.46",
        "zone": "east"
    }],
    "status": "success"
}
```

#### List Zones

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/zones`

Responce:

```
{
    "data": [
        "east",
        "west"
    ],
    "status": "success"
}
```

#### List Hosts

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/hosts`

Responce:

```
{
    "data": [
        "server.usa.com",
        "server.france.com"
    ],
    "status": "success"
}
```

#### Get IP Info

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/{IP}`

Responce:

```
{
    "data": {
        "id": "192.168.1.46",
        "ip": "192.168.1.46",
        "zone": "east",
        "host": "server.usa.com",
        "status": "assigned",
        "type": "dedicated_ip",
        "assigned_to": "{{ACCOUNT_ID}}"
    },
    "status": "success"
}
```

#### Assign IP To Account

Request:

`POST` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/{IP}`

Payload:
```
{
    "data": {}
}
```

Responce:

```
{
    "data": {
        "id": "192.168.1.46",
        "ip": "192.168.1.46",
        "zone": "east",
        "host": "server.usa.com",
        "status": "assigned",
        "type": "dedicated_ip",
        "assigned_to": "{{ACCOUNT_ID}}"
    },
    "status": "success"
}
```

#### Assign IPS To Account

Request:

`POST` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips`

Payload:
```
{
    "data": {
        "ips": ["192.168.1.45", "192.168.1.46"]
    }
}
```

Responce:

```
{
    "data": [{
        "id": "192.168.1.45",
        "ip": "192.168.1.45",
        "zone": "west",
        "host": "server.usa.com",
        "status": "assigned",
        "type": "dedicated_ip",
        "assigned_to": "{{ACCOUNT_ID}}"
    }, {
        "id": "192.168.1.46",
        "ip": "192.168.1.46",
        "zone": "east",
        "host": "server.usa.com",
        "status": "assigned",
        "type": "dedicated_ip",
        "assigned_to": "{{ACCOUNT_ID}}"
    }],
    "status": "success"
}
```

#### Release IP From Account

Request:

`DELETE` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/{IP}`

Payload:
```
{
    "data": {}
}
```

Responce:

```
{
    "data": {
        "id": "192.168.1.46",
        "ip": "192.168.1.46",
        "zone": "east",
        "host": "server.usa.com",
        "status": "available",
        "type": "dedicated_ip",
        "assigned_to": "{{ACCOUNT_ID}}"
    },
    "status": "success"
}
```
