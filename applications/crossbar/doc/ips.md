/*
Section: Crossbar
Title: IPs
Language: en-US
*/

The IPs API allows users to list the address assigned to their accounts.  In the future they will be able to fully manage dedicated IPs.

## Schema

* `ip`: The IP address
* `zone`: The zone the IP currently routes to, this will be an arbitrary value

## APIs

### List Assigned IPs

Request:

`GET` `http://searver.com:8000/v1/accounts/{ACCOUNT_ID}/ips/assigned`

Responce:

```
{
  "data":[
    {
      "ip":"192.168.1.46",
      "zone":"east"
    }
  ],
  "revision":"undefined",
  "request_id":"517dc259376b32361b79246ad055263f",
  "status":"success",
  "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```

## List Zones

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/zones`

Responce:

```
{
  "data":[
    "east",
    "west"
  ],
  "revision":"undefined",
  "request_id":"d1dbecb3a8a3153d4ad363a8231f71ae",
  "status":"success",
  "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```

## List Hosts

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/hosts`

Responce:

```
{
  "data":[
    "server.usa.com",
    "server.france.com"
  ],
  "revision":"undefined",
  "request_id":"52564a9293c620994a6b5327188a61b8",
  "status":"success",
  "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```

## Get IP Info

Request:

`GET` `http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/{IP}`

Responce:

```
{
  "data":{
    "id":"192.168.1.46",
    "ip":"192.168.1.46",
    "zone":"east",
    "host":"server.usa.com",
    "status":"assigned",
    "type":"dedicated_ip",
    "assigned_to":"1760753c8d022d650418fbbe6a1a10e0"
  },
  "revision":"undefined",
  "request_id":"6ea9bafbf5887ba220bc6906cf89b951",
  "status":"success",
  "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```

## Assign IP To Account

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
   "data":{
      "id":"192.168.1.46",
      "ip":"192.168.1.46",
      "zone":"east",
      "host":"server.usa.com",
      "status":"assigned",
      "type":"dedicated_ip",
      "assigned_to":"1760753c8d0226430418fbbe6a1a10e0"
   },
   "revision":"undefined",
   "request_id":"ca7d4eba9275a75910b1a0d2158e1316",
   "status":"success",
   "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```

## Release IP From Account

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
  "data":{
    "id":"192.168.1.46",
    "ip":"192.168.1.46",
    "zone":"east",
    "host":"server.usa.com",
    "status":"available",
    "type":"dedicated_ip",
    "assigned_to":"1760753c8d022d650418fbbe6a1a10e0"
  },
  "revision":"undefined",
  "request_id":"6ea9bafbf5887ba220bc6906cf89b951",
  "status":"success",
  "auth_token":"f4a59304f15847d883325b43103cbc88"
}
```
