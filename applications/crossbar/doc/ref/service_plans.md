# Service Plans

## About Service Plans

#### Schema

Describes services offered to sub-accounts



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bookkeepers` |   | `object()` |   | `false` |  
`category` | Optional category used for grouping service plans | `string()` |   | `false` |  
`description` | Describes the service plan offering | `string()` |   | `false` |  
`manual_recurring.[].name` | A friendly name for the item | `string()` |   | `false` |  
`manual_recurring.[].quantity` | How many of the item are allowed | `integer()` |   | `false` |  
`manual_recurring.[].rates` | Item's rate | `number()` |   | `false` |  
`manual_recurring` | Monthly recurring items | `array(object())` |   | `false` |  
`merge.priority` | The priority among the service plans with the merge strategy | `integer()` |   | `false` |  
`merge.strategy` | The merge strategy, like strategies are merged together based on their priority | `string('simple' | 'cumulative')` |   | `false` |  
`merge` | Optionally defines a strategy and priority to merge multiple service plans together | `object()` |   | `false` |  
`name` | A friendly name for the service plan | `string(1..128)` |   | `true` |  
`plan./.+/` | Category name | `object()` |   | `false` |  
`plan` | Outlines the service plan for various services | `object()` |   | `true` |  

### bookkeepers

The bookkeeper modules provided by Kazoo


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`braintree` |   | `object()` |   | `false` |  
`local` |   | `object()` |   | `false` |  

### service_plan.category

Describes a service plan category


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`.+` | Item name | `object()` |   | `false` |  
`_all.exceptions.[]` |   | `string()` |   | `false` |  
`_all.exceptions` | Items that are not included in this item plan | `array(string())` |   | `false` |  
`_all` | Applies item rules to any item in this category | `object()` |   | `false` |  

### service_plan.item

Describes a service plan item


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`activation_charge` | What to charge when activating an Item | `number()` |   | `false` |  
`as` | Count Item as if it was another Item | `string()` |   | `false` |  
`cascade` | Whether to count quantities among all sub-accounts or just the account | `boolean()` |   | `false` |  
`cumulative_discount` | Whether to give a discount based on quantities of the account and all sub-accounts | `boolean()` |   | `false` |  
`cumulative_discount_rate` | How much of a discount to apply | `number()` |   | `false` |  
`discounts.cumulative.maximum` | The most number of Items to apply discount to | `integer()` |   | `false` |  
`discounts.cumulative.rate` | The discount to apply, up to maximum Items (if applicable) | `number()` |   | `false` |  
`discounts.cumulative` |   | `object()` |   | `false` |  
`discounts` |   | `object()` |   | `false` |  
`markup_type` | How rate for this usage is calculated | `string('fixed_price' | 'percentage' | 'rate')` |   | `false` |  
`minimum` | The minimum quantity to charge for, if 'quantity' is less than 'minimum' | `integer()` |   | `false` |  
`name` | Friendly name for this Item | `string()` |   | `false` |  
`quantity` | How many of the item are allowed | `integer()` |   | `false` |  
`rate` | The rate to charge | `number()` |   | `false` |  
`rates./^[0-9]+$/` | The rate to charge when under the quantity indicated in the key | `number()` |   | `false` |  
`rates` | Tiers of rates based on quantities | `object()` |   | `false` |  
`single_discount` | Whether to give a discount to the account | `boolean()` |   | `false` |  
`single_discount_rate` | How much of a discount to apply, per-item | `number()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/editable

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/editable
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/override

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/override
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/current

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/current
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/synchronization
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}
```

