# Understanding Rating

System administrators can create multiple ratedecks and assign them to accounts or resellers as service plan items. This allows admins and resellers flexibility in how they rate calls for different accounts (and admins can have a nice opportunity to offer this functionality as a paid extra).


## High level overview

1.  Admins can create ratedecks by uploading CSVs to the [Tasks API](/applications/crossbar/doc/tasks.md) and defining the \`ratedeck\_id\`. See the [Rate Task](/applications/tasks/doc/rates.md) docs for how to upload the CSV. This will be covered further down too.
2.  Admins can then create Service Plans that incorporate the ratedeck(s) available. You can optionally charge for access to these ratedecks as part of the service plan.
3.  Now admins can assign these service plans to an account or reseller. When rating a call, the system will look at the account, then the reseller, and finally the default system ratedeck looking for what ratedeck(s) to use.

The rest of this article will demonstrate the API commands necessary to configure the ratedecks, service plans, and accounts to have this all work appropriately. Hopefully someone will be inspired to create an appropriate [MonsterUI](https://docs.2600hz.com/ui) application to manage this from the browser!


## Getting ready

For the purposes of this document, we'll refer to the admin account as `{ADMIN_ACCOUNT_ID}`, a reseller account as `{RESELLER_ACCOUNT_ID}`, and to the customer account of the reseller as `{CUSTOMER_ACCOUNT_ID}`.

We are going to use some Bash scripts to help us along the way. Feel free to try them out too or adapt the cURL requests to your programming language of choice.

Let's create our admin auth token:

```shell
export CREDENTIALS=`echo -n "{ADMIN_USERNAME}:{ADMIN_PASSWORD}" | md5sum | cut -d ' ' -f 1`
eval $(./scripts/export_auth_token.bash -a {ADMIN_ACCOUNT_NAME})
```

The account ID and auth token are now exported to the shell environment. You can check this by simply echo-ing them:

```shell
echo $ACCOUNT_ID
{ADMIN_ACCOUNT_ID}
echo $AUTH_TOKEN
{ADMIN_AUTH_TOKEN}
```

Also, be sure you've handled the [Tasks Operations](/applications/crossbar/doc/tasks.md) steps to start the tasks application and the tasks crossbar endpoint.


## Creating Ratedecks


### CSV Format

Looking at the [Rates Task](/applications/tasks/doc/rates.md) we see the following fields that can be defined:

    Name | Description | Required
    ---- | ----------- | --------
    `account_id`|reseller's account (see **Note 1** below)|
    `description`|description for rate|
    `direction`|direction of call leg ("inbound", "outbound"), if not set - rate matches both directions|
    `iso_country_code`|[ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements) code for prefix's country|
    `prefix`|prefix for match DID number| `true`
    `pvt_rate_cost`|internal rate cost, used for `weight` calculation|
    `pvt_rate_surcharge`|internal rate surcharge|
    `rate_cost`|per minute cost| `true`
    `rate_increment`|billing "steps" for rate|
    `rate_minimum`|minimum call duration|
    `rate_name`|short name for rate, if this field not set it will be generated from `prefix`, `iso_country_code` and `direction` fields|
    `rate_nocharge_time`|"free" call time, if call duration less then this value (seconds), then call not charged|
    `rate_surcharge`|charge amount on connect (answer)|
    `rate_version`|rate version|
    `ratedeck_id`| ratedeck name, assigned to account via service plan|

You can also query to the rates task to find this information out:

```shell
curl -H "X-Auth-Token: $AUTH_TOKEN" 'http://{SERVER}:8000/v2/tasks?category=rates&action=import' | python -mjson.tool
```

```json
{
  "auth_token": "{ADMIN_AUTH_TOKEN}",
  "data": {
    "tasks": {
      "rates": {
        "import": {
          "description": "Bulk-import rates to a specified ratedeck",
          "doc": "Creates rates from file",
          "expected_content": "text/csv",
          "mandatory": [
            "prefix",
            "rate_cost"
          ],
          "optional": [
            "account_id",
            "carrier",
            "description",
            "direction",
            "internal_rate_cost",
            "iso_country_code",
            "options",
            "rate_increment",
            "rate_minimum",
            "rate_name",
            "rate_nocharge_time",
            "rate_surcharge",
            "rate_version",
            "ratedeck_id",
            "routes",
            "weight"
          ]
        }
      }
    }
  },
  "status": "success",
  "timestamp": "2017-07-24T21:34:06",
  "version": "4.0.0"
}
```

So we know we need to include at least a "prefix" and a "rate\_cost" field (the "mandatory" array above).


### Sample CSV

Let's create a sample CSV file with a couple rates.

```csv
"rate_cost","description","name","prefix"
"0.1","BRONZE","BRONZE","1503"
"0.2","SILVER","SILVER","150"
"0.3","GOLD","GOLD","15"
"0.4","PLATINUM","PLATINUM","1"
```

We define four rates here - the more specific the prefix, the more preference will be given when matching a call to a rate (so dialing 15035551234 will prefer the BRONZE rate, for instance). Also note that the order of the fields is immaterial as long as the first row is the header row with at least the mandatory fields defined. Any unknown fields will be ignored.

Save this as 'simple\_rates.csv' so we can upload it.


### Uploading a CSV

1.  Create the import task

    ```shell
    curl -v -X PUT \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    -H "Content-type: text/csv" \
    --data-binary @simple_rates.csv \
    'http://{SERVER}:8000/v2/tasks?category=rates&action=import' | python -mjson.tool
    ```

    ```json
    {
        "auth_token":"{AUTH_TOKEN}",
        "data": {
            "_read_only": {
                "account_id": "{ADMIN_ACCOUNT_ID}",
                "action": "import",
                "auth_account_id": "{ADMIN_ACCOUNT_ID}",
                "category": "rates",
                "created": 63653369168,
                "id": "{IMPORT_TASK_ID}",
                "status": "pending",
                "total_count": 4
            }
        },
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }
    ```

2.  Start the import task

    ```shell
    curl -v -X PATCH \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    'http://{SERVER}:8000/v2/tasks/{IMPORT_TASK_ID}' | python -mjson.tool
    ```

    ```json
    {
        "auth_token":"{AUTH_TOKEN}",
        "data": {
            "_read_only": {
                "account_id": "{ADMIN_ACCOUNT_ID}",
                "action": "import",
                "auth_account_id": "{ADMIN_ACCOUNT_ID}",
                "category": "rates",
                "created": 63653369168,
                "id": "{IMPORT_TASK_ID}",
                "node": "lappy64@lappy64.pdx.2600hz.com",
                "start_timestamp": 63653369288,
                "status": "executing",
                "total_count": 4
            }
        },
        "page_size": 1,
        "request_id": "94e639657567700808668ad6083b32e2",
        "revision": "automatic",
        "status": "success"
    }
    ```

3.  Query the import task

    ```shell
    curl -v -X GET \
         -H "X-Auth-Token: $AUTH_TOKEN" \
         'http://{SERVER}:8000/v2/tasks/{IMPORT_TASK_ID}' | python -mjson.tool
    ```

    ```json
    {
        "auth_token": "{AUTH_TOKEN}",
        "data": {
            "_read_only": {
                "account_id": "{ADMIN_ACCOUNT_ID}",
                "action": "import",
                "auth_account_id": "{ADMIN_ACCOUNT_ID}",
                "category": "rates",
                "created": 63653369168,
                "csvs": [
                    "out.csv",
                    "in.csv"
                ],
                "end_timestamp": 63653369290,
                "failure_count": 0,
                "id": "{IMPORT_TASK_ID}",
                "node": "lappy64@lappy64.pdx.2600hz.com",
                "start_timestamp": 63653369288,
                "status": "success",
                "success_count": 4,
                "total_count": 4
            }
        },
        "page_size": 1,
        "request_id": "{REQUEST_ID}",
        "revision": "2402e64e232116695d2f54f9f5a24ed8",
        "status": "success"
    }
    ```

    Once 'status' is equal to 'success', the ratedeck is now uploaded into the system's global ratedeck.

4.  Test the new ratedeck by rating a number

    ```shell
    curl -v -X GET \
        -H "X-Auth-Token: $AUTH_TOKEN" \
        http://{SERVER}:8000/v2/rates/number/15035551234 | python -mjson.tool
    ```

    ```json
    {
        "auth_token": "{AUTH_TOKEN}",
        "data": {
            "Base-Cost": 0.1,
            "E164-Number": "+15035551234",
            "Prefix": "1503",
            "Rate": 0.1,
            "Rate-Description": "BRONZE",
            "Rate-Increment": 60,
            "Rate-Minimum": "60",
            "Rate-Name": "1503",
            "Ratedeck-ID": "ratedeck",
            "Surcharge": 0.0
        },
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }
    ```


### Uploading a second ratedeck

Now that we've populated a "default" ratedeck, let's create a new ratedeck by adding the `ratedeck_id` field to a CSV. We'll just edit "simple\_rates.csv" and re-save it as "bulk\_rates.csv":

```csv
"rate_cost","description","name","prefix","ratedeck_id"
"0.01","BRONZE","BRONZE","1503","bulk"
"0.02","SILVER","SILVER","150","bulk"
"0.03","GOLD","GOLD","15","bulk"
"0.04","PLATINUM","PLATINUM","1","bulk"
```

In our hypothetical, we're creating a ratedeck we'd give to those who push lots of minutes through us and want to give a bulk discount.

1.  Create the import task

    ```shell
    curl -v -X PUT \
         -H "X-Auth-Token: $AUTH_TOKEN" \
         -H "Content-type: text/csv" \
         --data-binary @bulk_rates.csv \
         'http://{SERVER}:8000/v2/tasks?category=rates&action=import' | python -mjson.tool
    ```

    ```json
    {
        "auth_token":"{AUTH_TOKEN}",
        "data": {
            "_read_only": {
                "account_id": "{ADMIN_ACCOUNT_ID}",
                "action": "import",
                "auth_account_id": "{ADMIN_ACCOUNT_ID}",
                "category": "rates",
                "created": 63653370672,
                "id": "{BULK_IMPORT_TASK_ID}",
                "status": "pending",
                "total_count": 4
            }
        },
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }
    ```

2.  Start the import

    ```shell
    curl -v -X PATCH \
         -H "X-Auth-Token: $AUTH_TOKEN" \
         'http://{SERVER}:8000/v2/tasks/{BULK_IMPORT_TASK_ID}' | python -mjson.tool
    ```

    ```json
    {
        "auth_token":"{AUTH_TOKEN}",
        "data": {
            "_read_only": {
                "account_id": "{ADMIN_ACCOUNT_ID}",
                "action": "import",
                "auth_account_id": "{ADMIN_ACCOUNT_ID}",
                "category": "rates",
                "created": 63653370672,
                "id": "{BULK_IMPORT_TASK_ID}",
                "node": "lappy64@lappy64.pdx.2600hz.com",
                "start_timestamp": 63653370752,
                "status": "executing",
                "total_count": 4
            }
        },
        "page_size": 1,
        "request_id": "{REQUEST_ID}",
        "revision": "automatic",
        "status": "success"
    }
    ```

3.  Test 'bulk' ratedeck lookups

    ```shell
    curl -v -X GET \
        -H "X-Auth-Token: $AUTH_TOKEN" \
        'http://{SERVER}:8000/v2/rates/number/15035551234?ratedeck_id=bulk' | python -mjson.tool
    ```

    ```json
    {
        "auth_token": "{AUTH_TOKEN}",
        "data": {
            "Base-Cost": 0.01,
            "E164-Number": "+15035551234",
            "Prefix": "1503",
            "Rate": 0.01,
            "Rate-Description": "BRONZE",
            "Rate-Increment": 60,
            "Rate-Minimum": "60",
            "Rate-Name": "1503",
            "Ratedeck-ID": "bulk",
            "Surcharge": 0.0
        },
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }
    ```


### Recap

We now have two ratedecks: the default system ratedeck and one named 'bulk'. We can now assign them to accounts via service plans and have those accounts billed differently for per-minute calls!


## Creating a service plan

Now that we have our ratedecks, let's create service plans that can be applied to accounts and affect what rates are made available to an account (or a reseller and its sub-accounts).


### Service Plan format

Take a look at the [Service Plan lifecycle](/core/kazoo_services/doc/lifecycle.md) for details on how service plans work. Basically, we have a category of `ratedeck` and an item with the `ratedeck_id` as the key. You can also optionally include surcharges, activation fees, etc.


### Sample Service plan

Since the "retail" ratedeck is the default, no service plan is necessary. Let's create a service plan that has the "bulk" ratedeck with no charges for use.

```json
{
    "_id":"plan_bulk_ratedeck",
    "pvt_type":"service_plan",
    "name":"Bulk Ratedeck Service Plan",
    "plan":{
        "ratedeck":{
            "bulk":{
            }
        }
    }
}
```


### Create the service plan

At the moment, there is no API for creating new service plans so you have to do it directly into the `{ADMIN_ACCOUNT_ID}` (get it with `sup kapps_util get_master_account_db` if unsure):

```shell
curl -v -X PUT \
     -H "content-type: application/json" \
     -d '{"pvt_type":"service_plan","name":"Bulk Ratedeck Service Plan","plan":{"ratedeck":{"bulk":{}}}}' \
     'http://{SERVER}:15984/{ADMIN_ACCOUNT_ID}/plan_bulk_ratedeck'
```

```json
{"ok":true,"id":"plan_bulk_ratedeck","rev":"{REVISION}"}
```

Now we can query service plans to see it as an available choice:

```shell
curl -v -X GET \
     -H "X-Auth-Token: $AUTH_TOKEN" \
     'http://{SERVER}:8000/v2/accounts/{ADMIN_ACCOUNT_ID}/service_plans' | python -mjson.tool
```

```json
{
    "auth_token":"{AUTH_TOKEN}",
    "data": [
        {
            "id": "plan_bulk_ratedeck",
            "name": "Bulk Ratedeck Service Plan"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


### Assign the service plan to an account

You can assign the ratedeck to a reseller account and it will apply to all sub-accounts that don't have an explicitly-assigned ratedeck. Or to individual accounts. If an account (and its reseller) doesn't have an assigned service plan, it will use the system's default ratedeck.

```shell
curl -v -X POST \
     -H "X-Auth-Token: $AUTH_TOKEN" \
     -d '{"data": {"add": ["plan_bulk_ratedeck"]}}' \
     http://{SERVER}:8000/v2/accounts/{RESELLER_ACCOUNT_ID}/service_plans | python -mjson.tool
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "plan": {
            "ratedeck": {
                "bulk": {}
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "3-69ff67e59c44670e95ac62f7807d37bf",
    "status": "success"
}
```


### Test rating

```shell
curl -v -X GET \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    "http://{SERVER}:8000/v2/accounts/$RESELLER_ACCOUNT_ID/rates/number/15035551234" | python -mjson.tool
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "Base-Cost": 0.01,
        "E164-Number": "+15035551234",
        "Prefix": "1503",
        "Rate": 0.01,
        "Rate-Description": "BRONZE",
        "Rate-Increment": 60,
        "Rate-Minimum": "60",
        "Rate-Name": "1503",
        "Ratedeck-ID": "bulk",
        "Surcharge": 0.0
    },
    "request_id": "993749de15a0db50752dfccf8c8d6dc5",
    "revision": "undefined",
    "status": "success"
}
```


### Recap

You should now be able to assign service plans to accounts to modify which ratedeck they are able to use.
