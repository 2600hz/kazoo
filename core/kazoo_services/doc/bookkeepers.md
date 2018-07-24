# Bookkeepers

Bookkeepers provide a way for external bookkeeping services to reconcile services items for an account. If you sell a device for $39.99 to your customers and they try to add one to their account, you'd like to know that you just got paid $39.99.


## A bit of history

The first [bookkeeper module](https://github.com/2600hz/kazoo/blob/master/core/kazoo_services/src/bookkeepers/kz_bookkeeper_braintree.erl) was written to interface with Braintree but this was a tailored interface based on the needs of 2600Hz; most community members weren't working with Braintree for credit card processing.


## Define your own bookkeeper

Now, it is possible to define your own bookkeeper service to interact with Kazoo. Kazoo will send your server a request via HTTP and, based on the response, update the requesting account accordingly.

Define HTTP bookkeeper by creating the following \`system\_config > services.http\_sync\` document in the DB.

```json
{
    "_id": "services.http_sync",
    "default": {
        "authorization_header": "123abc",
        "http_url": "http://my-bookeeper-url/"
    }
}
```

Assign the http bookkeeper to the master account in the \`system\_config > services\` document

```json
"default": {
    "sync_services": true,
    "support_billing_id": true,
    "should_save_master_audit_logs": false,
    "master_account_bookkeeper": "kz_bookkeeper_http",
    "scan_rate": 20000,
    "sync_buffer_period": 600
},
```

After adding a SIP device, Kazoo will contact the remote bookkeeper with the new information.

Here is some sample PHP code for testing the collection of data on the bookkeeper end.

```php
<?php

/* This is environment specific.  May not be required or may need to be something else depending on the environment. */
require ('init.php');

$auth = "123abc";
$connection_ip = "xx.xx.xx.xx";

/* Collect POST header for no particular reason */
$results = (apache_request_headers());
file_put_contents('post_header.txt', print_r($results, true));

/* Check Authorization and IP in received POST header via $_SERVER */
if ($_SERVER['HTTP_AUTHORIZATION'] != $auth || $_SERVER['REMOTE_ADDR'] != $connection_ip) {
    http_response_code(500);
    exit();
}

// Authorized.  Now collect POST data
file_put_contents('post_body.txt', file_get_contents('php://input'));

/* Do something with the data*/
...
...
...
/* Tell Kazoo that data update was successful */
http_response_code(200);

```


## Syncing

Kazoo is configured to periodically (20s by default) look for accounts which are marked **dirty**. These are accounts that have changes to billable things that have not successfully synced to the configured bookkeeper. Kazoo will take these unsynced updates, apply the service plan(s) of the account, and create a full list of service items in the account. These are then sent to the bookkeeper for reconciliation.


# The HTTP Bookkeeper

As an account grows in what's configured, a JSON object of these service items will be sent to the configured bookkeeper server, to be processed remotely. Depending on the response, Kazoo will mark the items as complete and the account as being in good standing or will either flag the account as no longer in good standing or retry the update (keep the account marked dirty) again later.

NOTE: HTTP Bookkeeper will only send a JSON object if the account has an active service plan.  Kazoo will make no attempt to contact the remote HTTP bookkeeper if there is no active service plan.


## The Request

The request payload will be composed of objects with the following fields:

| Name                       | Description                                            | Type            | Required |
|-------------------------- |------------------------------------------------------ |--------------- |-------- |
| category                   | The service category                                   | string()        | true     |
| item                       | The service item                                       | string()        | true     |
| quantity                   | The quantity of items to purchase                      | integer()       | true     |
| rate                       | The service plan cost, per-item, for the service items | float()         | true     |
| name                       | The service plan's name for the item                   | string()        | false    |
| single\_discount           | Whether a single item discount applies                 | boolean()       | false    |
| single\_discount\_rate     | The discount to apply to the item                      | float()         | false    |
| cumulative\_discount       | Whether a cumulative discount applies                  | boolean()       | false    |
| cumulative\_discount\_rate | The discount to apply to the item                      | float()         | false    |
| activation\_charge         | A one-time charge to apply                             | float()         | false    |
| minimum                    | The minimum quantity to charge                         | integer()       | false    |
| exceptions                 | Service items that are excepted from the service plan  | array(string()) | false    |


### Example:

applications.

```json
{
    "devices":{
        "sip_device":{
            "category":"devices"
            ,"item":"sip_device"
            ,"quantity": 4
            ,"rate": 29.99
        }
        ,"softphone":{
            "category":"devices"
            ,"item":"softphone"
            ,"quantity":2
            ,"rate":0
        }
    }
    ,"ui_apps": {
        "numbers": {
            "category": "ui_apps",
            "item": "numbers",
            "quantity": 1,
            "rate": 2.0,
            "activation_charge": 1.0
        }
        ,"accounts": {
            "category": "ui_apps",
            "item": "accounts",
            "quantity": 1,
            "rate": 5.0,
            "activation_charge": 4.0
        }
    }
}
```


## The Response

The HTTP response code will be used to determine how Kazoo proceeds with the update.

| Response Code | Account Standing | Update Complete |
|------------- |---------------- |--------------- |
| 200           | Good             | Yes             |
| 402           | Error            | Yes             |
| 4xx or 5xx    | Good             | No              |


### Response code: 200

A response of 200 from the bookkeeper server means Kazoo will accept the update and consider the account in good standing (synced with the bookkeeping service).


### Response code: 402

A response of 402 will accept the update (not dirty) but will mark the account as needing attention (not in good standing).


### Response codes: 4xx and 5xx

Any error response codes in the 400s or 500s (besides 402), as well as connection errors (failure of the server to accept the TCP connection), will leave the account in good standing but will not mark the account as synced (dirty). The update will be retried at a future date.


### Response body

At this time, no processing of the response body will be done. Implementing servers are free to leave it empty.


# Account standing

To move an account into or out of good standing, the admin can use the following Crossbar API to move an account's standing:


## /v2/accounts/{ACCOUNT\_ID}/services/status

-   **GET**: Fetch the current status of the account

    -   Account in good standing:

    ```shell
      $> curl -X GET http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status
      {
          "data":{
              "in_good_standing":true
          }
      }
    ```

    -   Not in good standing

    ```shell
      $> curl -X GET http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status
      {
          "data":{
              "in_good_standing":false
              ,"reason":"credit card expired"
              ,"reason_code":12345
          }
      }
    ```
-   **POST**: Move an account to/from good standing

    -   Move to good standing

    ```shell
      $> curl -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status -d '{"data":{"in_good_standing":true}}'
      {
          "data":{
              "in_good_standing":true
          }
      }
    ```

    -   Move from good standing

    ```shell
      $> curl -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status -d '{"data":{"in_good_standing":false, "reason":"custom error reason"}}'
      {
          "data":{
              "in_good_standing":false
              ,"reason":"custom error reason"
          }
      }
    ```
