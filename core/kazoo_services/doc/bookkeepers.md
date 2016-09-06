- [Bookkeepers](#orgheadline4)
  - [A bit of history](#orgheadline1)
  - [Define your own bookkeeper](#orgheadline2)
  - [Syncing](#orgheadline3)
- [The HTTP Bookkeeper](#orgheadline12)
  - [The Request](#orgheadline6)
    - [Example:](#orgheadline5)
  - [The Response](#orgheadline11)
    - [Response code: 200](#orgheadline7)
    - [Response code: 402](#orgheadline8)
    - [Response codes: 4xx and 5xx](#orgheadline9)
    - [Response body](#orgheadline10)
- [Account standing](#orgheadline14)
  - [/v2/accounts/{ACCOUNT\_ID}/services/standing](#orgheadline13)


# Bookkeepers<a id="orgheadline4"></a>

Bookkeepers provide a way for external bookkeeping services to reconcile services items for an account. If you sell a device for $39.99 to your customers and they try to add one to their account, you'd like to know that you just got paid $39.99.

## A bit of history<a id="orgheadline1"></a>

The first [bookkeeper module](https://github.com/2600hz/kazoo/blob/master/core/kazoo_services/src/bookkeepers/kz_bookkeeper_braintree.erl) was written to interface with Braintree but this was a tailored interface based on the needs of 2600Hz; most community members weren't working with Braintree for credit card processing.

## Define your own bookkeeper<a id="orgheadline2"></a>

Now, it is possible to define your own bookkeeper service to interact with Kazoo. Kazoo will send your server a request via HTTP and, based on the response, update the requesting account accordingly. Let's take a look.

## Syncing<a id="orgheadline3"></a>

Kazoo is configured to periodically (20s by default) look for accounts which are marked **dirty**. These are accounts that have changes to billable things that have not successfully synced to the configured bookkeeper. Kazoo will take these unsynced updates, apply the service plan(s) of the account, and create a full list of service items in the account. These are then sent to the bookkeeper for reconciliation.

# The HTTP Bookkeeper<a id="orgheadline12"></a>

As an account grows in what's configued, a JSON object of these service items will be sent to the configured bookkeeper server, to be processed remotely. Depending on the response, Kazoo will mark the items as complete and the account as being in good standing or will either flag the account as no longer in good standing or rety the update (keep the account marked dirty) again later.

## The Request<a id="orgheadline6"></a>

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

### Example:<a id="orgheadline5"></a>

applications.

```js2
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

## The Response<a id="orgheadline11"></a>

The HTTP response code will be used to determine how Kazoo proceeds with the update.

| Response Code | Account Standing | Update Complete |
|------------- |---------------- |--------------- |
| 200           | Good             | Yes             |
| 402           | Error            | Yes             |
| 4xx or 5xx    | Good             | No              |

### Response code: 200<a id="orgheadline7"></a>

A response of 200 from the bookkeeper server means Kazoo will accept the update and consider the account in good standing (synced with the bookkeeping service).

### Response code: 402<a id="orgheadline8"></a>

A response of 402 will accept the update (not dirty) but will mark the account as needing attention (not in good standing).

### Response codes: 4xx and 5xx<a id="orgheadline9"></a>

Any error response codes in the 400s or 500s (besides 402), as well as connection errors (failure of the server to accept the TCP connection), will leave the account in good standing but will not mark the account as synced (dirty). The update will be retried at a future date.

### Response body<a id="orgheadline10"></a>

At this time, no processing of the response body will be done. Implementing servers are free to leave it empty.

# Account standing<a id="orgheadline14"></a>

To move an account into or out of good standing, the admin can use the following Crossbar API to move an account's standing:

## /v2/accounts/{ACCOUNT\_ID}/services/standing<a id="orgheadline13"></a>

-   **GET**: Fetch the current standing of the account
-   **POST**: Move the account into good standing (if not already)
-   **DELETE**: Move the account out of good standing