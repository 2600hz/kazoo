### Service Planner

#### About Service Planner

Crossbar Service Planner allows you to add, edit, list and remove service plans on your reseller account.

> **Note:** This is for creating and editing service plans that you want to offer to your own sub-accounts, to actually applying and see the current applied service plans to an account use (Service Plans API)[./service_plans.md].

> **Note:** Only an user logged in with their reseller account is able to change their own service plan offerings.

#### Service Plans Schema

Describes services offered to sub-accounts

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bookkeepers` |   | [#/definitions/bookkeepers](#bookkeepers) |   | `false`
`category` | Optional category used for grouping service plans | `string()` |   | `false`
`description` | Describes the service plan offering | `string()` |   | `false`
`name` | A friendly name for the service plan | `string(1..128)` |   | `true`
`plan` | Outlines the service plan for various services | `object()` |   | `true`
`plan./^[0-9a-zA-Z_]+$/` |   | [#/definitions/service_plan](#bookkeepers) |   | `false`

##### Book Keepers Schema

The bookkeeper modules provided by Kazoo

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`braintree` |   | `object()` |   | `false`
`local` |   | `object()` |   | `false`

##### Service Plan Schema

Describes a service plan

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`.+` | Category name | [#/definitions/service_plan.category](#service_plan.category) |   | `false`

##### Service Plan Category Schema

Describes a service plan category

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`.+` | Item name | service_plan.item |   | `false`
`_all` | Applies item rules to any item in this category | `object()` |   | `false`
`_all.exceptions` | Items that are not included in this item plan | `array(string())` |   | `false`
`_all.exceptions.[]` |   | `string()` |   | `false`

##### Service Plan Item Schema

Describes a service plan item

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`activation_charge` | What to charge when activating an Item | `number()` |   | `false`
`as` | Count Item as if it was another Item | `string()` |   | `false`
`cascade` | Whether to count quantities among all sub-accounts or just the account | `boolean()` |   | `false`
`cumulative_discount` | Whether to give a discount based on quantities of the account and all sub-accounts | `boolean()` |   | `false`
`cumulative_discount_rate` | How much of a discount to apply | `number()` |   | `false`
`discounts.cumulative.maximum` | The most number of Items to apply discount to | `integer()` |   | `false`
`discounts.cumulative.rate` | The discount to apply, up to maximum Items (if applicable) | `number()` |   | `false`
`discounts.cumulative` |   | `object()` |   | `false`
`discounts` |   | `object()` |   | `false`
`minimum` | The minimum quantity to charge for, if 'quantity' is less than 'minimum' | `integer()` |   | `false`
`name` | Friendly name for this Item | `string()` |   | `false`
`quantity` | How many of the item are allowed | `integer()` |   | `false`
`rate` | How much is the item billed, per-item | `number()` |   | `false`
`rates./^[0-9]+$/` | The rate to charge when under the quantity indicated in the key | `number()` |   | `false`
`rates` | Tiers of rates based on quantities | `object()` |   | `false`
`single_discount` | Whether to give a discount to the account | `boolean()` |   | `false`
`single_discount_rate` | How much of a discount to apply, per-item | `number()` |   | `false`

#### Available Fields To Customize

> GET /v2/service_planner

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/service_planner
```

##### Response

```json
{
  "data": {
    "devices": {
      "_all": {
        "activation_charge": {},
        "as": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "exceptions": {},
        "minimum": {},
        "rate": {}
      },
      "landline": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {}
      }
      "..."
    },
    "limits": {
      "_all": {
        "activation_charge": {},
        "as": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "exceptions": {},
        "minimum": {},
        "rate": {}
        }
        "..."
    },
    "number_services": {
      "_all": {
        "activation_charge": {},
        "as": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "exceptions": {},
        "minimum": {},
        "rate": {}
      },
      "cnam": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {}
      }
      "..."
    },
    "phone_numbers": {
      "_all": {
        "activation_charge": {},
        "as": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "exceptions": {},
        "minimum": {},
        "rate": {}
        },
      "did_us": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {}
      }
      "..."
    },
    "ui_apps": {
      "_all": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {},
        "exceptions": {},
        "as": {}
      },
      "accounts": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {}
      }
      "..."
    },
    "users": {
      "_all": {
        "activation_charge": {},
        "as": {},
        "discounts": {
            "maximum": {},
            "rate": {}
        },
        "exceptions": {},
        "minimum": {},
        "rate": {}
        },
      "admin": {
        "activation_charge": {},
        "discounts": {
          "maximum": {},
          "rate": {}
        },
        "minimum": {},
        "rate": {}
      }
      "..."
    }
  },
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "{STATUS}",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Create Service Plans

> PUT /v2/accounts/{ACCOUNT_ID}/service_planner

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner
```

#### Get a Summary of Service Plans

Get a list of all your account's service plans.

> GET /v2/accounts/{ACCOUNT_ID}/service_planner

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner
```

##### Response

```json
{
  "data": [
    {
      "id": "7c88ae4fdd60263452b4a898ba00e4dd",
      "name": "Device Only",
      "description": "Device Only Description",
      "category": "SaaS Plans"
    },
    {
      "id": "7c88ae4fdd60263452b4a898ba0050af",
      "name": "Awesome Full Service",
      "description": "",
      "category": "SaaS Plans"
    },
    {
      "id": "7c88ae4fdd60263452b4a898ba003843",
      "name": "Site Sign-ups with Support",
      "description": "",
      "category": "SaaS Plans"
    }
  ],
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "{STATUS}",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Get a Service Plan Details

> GET /v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner/7c88ae4fdd60263452b4a898ba0050af
```

##### Response

```json
{
  "data": {
    "bookkeepers": {
      "braintree": {
        "devices": {
          "sip_devices": {
            "addon": "sip_device",
            "plan": "SIP_Services"
          }
        },
        "limits": {
          "inbound_trunks": {
            "addon": "inbound_trunk",
            "plan": "SIP_Services"
          },
          "twoway_trunks": {
            "addon": "twoway_trunk",
            "plan": "SIP_Services"
          }
        },
        "number_services": {},
        "phone_numbers": {
          "did_us": {
            "addon": "did_us",
            "discounts": {
              "cumulative": "discount_did_us"
            },
            "plan": "SIP_Services"
          },
          "tollfree_us": {
            "addon": "tollfree_us",
            "plan": "SIP_Services"
          }
        }
      }
    },
    "description": "",
    "name": "Awesome Full Service",
    "category": "SaaS Plans",
    "plan": {
      "devices": {
        "_all": {
          "as": "sip_devices",
          "cascade": true,
          "name": "SIP Device",
          "rates": {
            "5": 0,
            "20": 4.95,
            "50": 9.95,
            "100": 49.95
          }
        }
      },
      "limits": {
        "inbound_trunks": {
          "name": "Inbound Trunk",
          "rate": 1.99
        },
        "twoway_trunks": {
          "name": "Two-Way Trunk",
          "rate": 1.99
        }
      },
      "number_services": {},
      "phone_numbers": {
        "did_us": {
          "cascade": true,
          "discount": {
            "cumulative": {
              "rates": {
                "5": 0,
                "10": 2.5,
                "20": 5
              }
            }
          },
          "name": "US DID",
          "rate": 1
        },
        "tollfree_us": {
          "cascade": true,
          "name": "US Tollfree",
          "rate": 5
        }
      }
    },
    "id": "7c88ae4fdd60263452b4a898ba0050af"
  },
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "{STATUS}",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_planner/{PLAN_ID}
```

