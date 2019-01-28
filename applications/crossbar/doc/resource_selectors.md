# Resource Selectors

## About Resource Selectors

Resource selectors is a new way to route Offnet-calls. Old way used regex rules and "flags" for select proper resources (gateways). With new "resource selectors" you have several small modules, which can be organized in "chain" (rules).

#### Schema

Schema for resource selector document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`name` | Selector name | `string()` |   | `true` |  
`resource` | Resource ID | `string()` |   | `true` |  
`selector` | Selector data | `string()` |   | `true` |  
`start_time` | Start time (Gregorian seconds) | `integer()` |   | `false` |  
`stop_time` | Stop time (Gregorian seconds) | `integer()` |   | `false` |  
`value` | Extra selector data | `string()` |   | `false` |  



## Fetch

> GET /v2/resource_selectors/rules

```shell
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/resource_selectors/rules
```

```json
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

## Update rules

> POST /v2/resource_selectors/rules

```shell
curl -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"rules": [
        {"get_resources":{}},
        {"filter_list": {"value_a": "request:Flags", "value_b": "resource:flags", "action": "keep"}}
    ]}}'
    http://{SERVER}:8000/v2/resource_selectors/rules
```

```json
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
    ],
    "id": "resource_selector_rules"
  },
  "revision": "{REVISION_ID}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

### Database selectors

Database selectors - selectors stored in special database. Name of this database `account/XX/XX/XXXXXXXXXXXXXXXXXXXXXXXXXXXX-selectors`, where `XXX...XXX` - Account ID. System-wide selectors database use Master Account ID.

Each selector is separate document:

```json
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

## List selectors names

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name
```

```json
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

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/lcr2
```

```json
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

## List resources

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource
```

```json
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

In this example we see resources `RES-2` with 3 documents, `RES-3` with 8 documents and `RES-4` with 1 document.

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/RES-4
```

```json
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

## Show selectors

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/RES-4/name/lcr
```

```json
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

Here we see selectors for resource `RES-4` with selector name `lcr`. Resulted list can be simple list of strings or list of objects, its depending if there additional `value` or not.

### Manage selectors

Manage (import/delete) resource selectors made via kazoo tasks (CSV file).

Category `resource_selectors`, action `import` or `delete`.

CSV columns:
* `mandatory`
    * `name`
    * `selector`
    * `resource`
* `optional`
    * `stat_time`
    * `stop_time`
    * `value`
