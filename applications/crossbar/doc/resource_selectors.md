# Resource selectors

## About

Resource selectors is a new way to route Offnet-calls. Old way used regex rules and "flags" for select proper resources (gateways). With new "resource selectors" you have several small modules, which can be organaized in "chanin" (rules).

## Rules

Rules is array of JSON objects. Each object contain one item where key is name of the module, and value is another object, with parameters for that module.  
Example:
```JSON
{
  "filter_list": {
    "value_a": "request:Flags",
    "value_b": "resource:flags",
    "action": "keep"
  }
}
```
here we call modue `filter_list` (which filter resources comparing 2 lists).
More info about modules and their parameters can be found [here](https://github.com/2600hz/kazoo/blob/master/applications/stepswitch/doc/resource_selectors.md).

Rules can be managed via http://{{IP}}:8000/v2/resource_selectors or http://{{IP}}:8000/v2/accounts/{{ACCOUNT_ID}}/resource_selectors

Rules storred in `resource_selector_rules` file in Account database. System-wide rules is stored in Master-Account database, so http://{{IP}}:8000/v2/resource_selectors is equal to http://{{IP}}:8000/v2/accounts/{{MASTER_ACCOUNT_ID}}/resource_selectors

### Show rules

> GET /v2/resource_selectors

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors
{
  "data": {
    "rules": [
      {
        "get_resources": {}
      },
      {
        "filter_list": {
          "value_a": "request:Flags",
          "value_b": "resource:flags",
          "action": "keep"
        }
      },
      {
        "filter_regex": {
          "value_a": "number",
          "value_b": "resource:rules",
          "action": "keep",
          "mode": "empty_fail"
        }
      },
      {
        "filter_regex": {
          "value_a": "cid_number",
          "value_b": "resource:cid_rules",
          "action": "keep",
          "mode": "empty_ok"
        }
      },
      {
        "order": {
          "value": "resource:weight_cost",
          "direction": "ascend"
        }
      }
    ],
    "id": "resource_selector_rules"
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

### Update rules

> POST /v2/resource_selectors

```curl
curl -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"rules":[{"get_resources":{}},{"filter_list":{"value_a":"request:Flags","value_b":"resource:flags","action":"keep"}}]}}'
    http://{SERVER}:8000/v2/resource_selectors
{
  "data": {
    "rules": [
      {
        "get_resources": {}
      },
      {
        "filter_list": {
          "value_a": "request:Flags",
          "value_b": "resource:flags",
          "action": "keep"
        }
      },
    "id": "resource_selector_rules"
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Database selectors

Database selectors - selectors stored in special database. Name of this database `account/XX/XX/XXXXXXXXXXXXXXXXXXXXXXXXXXXX-selectors`, where `XXX...XXX` - Account ID. System-wide selectors database use Master Account ID.

Each selector is sepparate doument:
```JSON
{
   "_id": "00066509d2648ede97e30635aa5ba097",
   "_rev": "1-5c13654b7a5521778791e6657789bb56",
   "pvt_type": "resource_selector",
   "name": "prefix",
   "selector": "7495",
   "resource": "RES-4",
   "value": "0.37"
}
```
- name - all selectors with same name used for filtering/sorting resources
- selector - this value used for filtering/sorting
- resource - Resource ID
- value - additional value, which can be used for sorting


### List selectors

#### List selectors names

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/name
{
  "data": [
    {
      "lcr2": 12
    },
    {
      "lcr": 36039
    }
  ],
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

Here we see 2 selectors, `lcr` with 12 documents and `lcr2` with `36039` documents.

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/name/lcr2
{
  "data": [
    {
      "RES-4": 1
    },
    {
      "RES-3": 8
    },
    {
      "RES-2": 3
    }
  ],
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### List resources

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/resource
{
  "data": [
    {
      "RES-4": 36040
    },
    {
      "RES-3": 8
    },
    {
      "RES-2": 3
    }
  ],
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

In this exampe we see resources `RES-2` with 3 docuemnts, `RES-3` with 8 documents and `RES-4` with 1 document.

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/resource/RES-4
{
  "data": [
    {
      "lcr2": 1
    },
    {
      "lcr": 36039
    }
  ],
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Show selectors

```curl
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/resource/RES-4/name/lcr
{
  "data": [
    {
      "74956785833": "0.20"
    },
    {
      "74999055927": "0.30"
    },
    ...
    "74991234",
    "74951234",
    ...
    {
      "7495": "0.40"
    },
    {
      "7499": "0.40"
    }
  ],
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

Here we ses selectors for resource `RES-4` with selector name `lcr`. Resulted list can be simple list of strings or list of objects, its dependind if there additional `value` or not.

### Add selectors

```curl
curl -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"selectors":["123","456","789"]}}' \
    http://{SERVER}:8000/v2/resource_selectors/resource/RES-4/name/lcr
{
  "data": {
    "total": 3,
    "success": 3,
    "error": 0
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

### Delete selectors

```curl
curl -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"selectors":["123","456","789"]}}' \
    http://{SERVER}:8000/v2/resource_selectors/resource/RES-4/name/lcr
{
  "data": {
    "total": 3,
    "success": 3,
    "error": 0
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

If you want delete all selectors, you can use special word `_all`, instead explictly list each selector.

```curl
curl -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"selectors":["_all"]}}' \
    http://{SERVER}:8000/v2/resource_selectors/resource/RES-4/name/lcr
{
  "data": {
    "total": 36039,
    "success": 36039,
    "error": 0
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Import selectors from CSV-files
TODO
