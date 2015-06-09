/*
Section: Whistle Services
Title: Lifecycle of a service plan
Language: en-us
Version: 3.21
*/

# In the beginning

There was a service\_plan doc. It defined a simple service: charge $1 per sip device. And lo, it looked like this:

    {"_id":"plan_simple"
     ,"_rev":"1-revision"
     ,"name":"Super Simple Service sPlan"
     ,"plan":{
         "devices":{
             "sip_device":{
                 "rate":1
             }
         }
     }
     ,"pvt_type":"service_plan"
    }

And the admins rejoiced, for they could now bill their accounts for SIP devices.

# The Account Is Born

An account is created and assigned the SSSP service plan. As part of account creation, an initial full reconcile is run to create the initial services document for the account (found in the services database with the account's ID as the doc ID).

The initial services doc looks something like this:

    {"_id":"{ACCOUNT_ID}
     ,"quantities": {
         "users": {}
         ,"ui_apps": {}
         ,"number_services": {}
         ,"phone_numbers": {}
         ,"ips": {"dedicated": 0}
         ,"devices": {}
     }
     ,"plans": {
         "plan_simple": {
             "account_id": "{MASTER_ACCOUNT_ID}"
         }
     }
     ,"pvt_dirty":true
     ,"pvt_type":"service"
    }

We can see that the initial quantities of "service-able" things have been initialized. Since the account has nothing in it yet, "quantities" only contains the service categories (except for ips, which has a service item "dedicated" - a real overachiever, that one!).

We can also see the plans applied to this account. We could create all sorts of different plans and apply multiple to this account.

Finally, we notice the "pvt_dirty" flag is set to true, indicating this account has changes that should be reconciled with the backend bookkeeper (Braintree, for instance).

## So Dirty

When an account's service document is marked dirty (`pvt_dirty` set to `true`), the system is being told that the `quantities` in the service document are likely out of sync with the account's actual quantities or that service charges or some other billable event needs to occur.

## Reconciliation

When reconciling an account, each services category module is called to reconcile their portion of the services doc's `quantities`. So `wh_service_devices:reconcile/1` is called with the account's services record as the arg. The service module will tally up all the service item quantities and update the services record appropriately. The services record should now reflect the "real" quantities. These updates are then merged onto the existing services doc and saved back to the services database.

Once reconcilitation has finished, the services doc's `quantities` should reflect the actual quantities of the account for that service category/item.

### Periodic Reconciliation

Kazoo starts a process, `wh_service_sync` that can be configured to periodically reconcile accounts if needed. This is configured in the `system_config/services` doc, under the `sync_services` flag. It defaults to 'false' so background reconciliation will not happen by default.

If enabled, the sync process will start a timer, determined by the `scan_rate` (in milliseconds) in the `system_config/services` doc, to periodically reconcile accounts and save updates if appropriate. When the timer fires, the sync process will check the `services` database for service docs marked dirty.

The sync job will find the oldest dirty account and attempt to bump its modified time. If successful, the account will be reconciled. If not, the process will return back and start a new timer.

If the sync process "wins" the bump contest, it will start synchronizing with the bookkeeper. The first thing to check is the `billing_id` on the account. If the account ID is the billing ID, sync the services. If not, we need to follow the `billing_id`. First, mark the billing ID's services doc as dirty; if successful, mark the account's services as clean. If a services doc for the billing ID is missing, kazoo does one of two things: if the billing ID doesn't exist as an account or has been soft-deleted, the account is updated to be its own billing ID; otherwise the billing ID is reconciled.

If the billing ID was marked dirty, this iteration is done (and the billing ID will be reconciled at a future time).

If the account is its own billing ID, service items are created and synced with the bookkeeper. If all goes well, the account is put in good standing and the reseller is marked dirty for future reconciliation/sync.

# The Account Grows

The admin for the account logs in and wants to create a device. Let's see what happens!

When the `PUT /v2/accounts/{ACCOUNT_ID}/devices` comes in and is validated, it is time to actual try to create the device. However, Kazoo needs to find out if this would cause a billable event and warn the client making the request. A `dry_run` request is made to see what the impact of the change would be. In this case, `wh_service_devices` would calculate a change in quantity which `crossbar_services` would get back as a `dry_run` JSON object. It might look like this:

    {"devices":{
        "sip_device":{
            "category":"devices"
            ,"item":"sip_device"
            ,"quantity":1
            ,"rate":1.0
            ,"single_discount":true
            ,"single_discount_rate":0.0
            ,"cumulative_discount":0
            ,"cumulative_discount_rate":0.0
        }
     }
    }

Crossbar now knows that if the action is performed, the account will be charged. Crossbar then checks to see if the request has explicitly indicated it will accept the charges (by including `"accept_charges":true` on the request payload). If the request is not accepting charges, a 402 response is sent back. The client can then decide if they'd like to accept the charges and resubmit the `PUT` with `"accept_charges":true` set.

## Accepting Charges

When resubmitted, the same dry_run process is executed, the request is found to accept charges, and the `PUT` is executed (in this case, the device is saved to the database). This in turn causes the service doc for the account to be marked as dirty.

This should also cause an audit log entry to be filed in the account's reseller's MODb. This audit log can help the reseller chain know who did what to cause a billing event to occur.

## Audit Logs

The master account won't save audit logs unless configured to do so. In `system_config/services`, toggle the flag `should_save_master_audit_logs` to true. Otherwise only the reseller tree minus the master account will have audit logs saved.

## Continuing...

Now that the client has created a device and indicated they are willing to be billed, all is well...except it isn't. If one were to view the account's services doc, one would see `"devices":{}` in the `quantities` object. Reconciliation hasn't been persisted yet. The main reason is that changes tend to come in waves and clients don't want to see 5 entries for each of 5 devices they've added; they'd rather see they added 5 devices in one line item. Now, any changes made will reconcile before committing the changes to ensure the account is able to make the change, but the actual stored quantities may be out of sync with both the services doc and the underlying bookkeeper.

Two APIs exist to force reconciliation/syncing:

* `POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation`
    - This API will do a full account reconcile and persist the real quantities to the services doc.

* `POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization`
    - This API will do a full account reconcile, persist, *and* will sync with the bookkeeper (basically what `wh_service_sync` does periodically).
    - This can also be run via SUP on the backend: `sup whistle_services_maintenance sync {ACCOUNT_ID}`

# Scenarios

The following are the anticipated scenarios of service changes being made and the resulting charges.

Let's assume an account tree of:

```
M -- R1 -- R2 -- D2
      | -- D1
```

Here we have the master account *M* with a child account, *R1*, a reseller. *R1* has two child accounts, *R2* (a reseller), and *D1* (a direct client). *R2* has a direct client *D2*.

It is important to keep in mind not only which account is changing a service quantity but also *who* is making the change (an account user, reseller user, or master user).

## *M*

Since *M* is the master account, it doesn't really get a service plan applied to it and can do as it pleases. Phew, that was easy.

## *R1*

When *M* creates *R1*, it applies a service plan (or several) to the account. Let's continue using the SSSP ($1 per device). After flagging the account as a reseller (`sup whistle_services_maintenance make_reseller {R1_ACCOUNT_ID}`), *R1* now make sub-accounts for itself.

### *R1* makes changes

Now that *R1* exists and is a reseller, let's look at when the *R1* admin creates a device and see what should occur.

When attempting the first PUT to devices, Crossbar should return a 402 with a payload like:


    {"data":{
         "devices":{
             "sip_device":{
                 "category":"devices"
                 ,"item":"sip_device"
                 ,"quantity":1
                 ,"rate":1.0
                 ,"single_discount":true
                 ,"single_discount_rate":0.0
                 ,"cumulative_discount":0
                 ,"cumulative_discount_rate":0.0
             }
         }
     }
     ,"error":"402"
     ,"message":"accept charges"
     ,"status":"error"
     ,"request_id":"{REQUEST_ID}"
     ,"auth_token":"{AUTH_TOKEN"
    }

The payload shows the details of what the change would entail (in absolute terms). If the request is resubmitted with the `accept_charges` flag, and the account is in good standing, the device is created and the services for *R1* is updated.

Now, if you were to look at *R1*'s services doc in the `services` DB, the quantities for `devices` is still empty. The account won't be reconciled until the background sync process gets to it. If another device creation would occur, the 402 response would be:

    {"data":{
        "devices":{
            "sip_device":{
                "category":"devices"
                ,"item":"sip_device"
                ,"quantity":2
                ,"rate":1.0
                ,"single_discount":true
                ,"single_discount_rate":0.0
                ,"cumulative_discount":0
                ,"cumulative_discount_rate":0.0
            }
        }
    }
    ,"error":"402"
    ,"message":"accept charges"
    ,"status":"error"
    ,"request_id":"{REQUEST_ID}"
    ,"auth_token":"{AUTH_TOKEN}"
   }

You can see that the quantity is now 2 for SIP devices. Now, if we reconcile the account manually (either via the UI or on the backend with `sup whistle_services_maintenace reconcile {R1_ACCOUNT_ID}`), we see that *R1*'s services doc now reflects the `"sip_device":1` in the `quantities` for `devices`.

When *R1* adds a second device after reconciling the first on the backend, we get a 402 with a nearly identical response payload (as Kazoo reconciles the service category to generate the 402 but doesn't persist it). So, from *R1*'s perspective, all is calculated properly for service updates, whether the account has been reconciled in the `services` db.

### *M* makes changes to *R1*

Suppose *M* wants to make a change while masquerading as *R1*. What happens?

Creating device #3 we see that the request is not responded to with a 402 as before. Why? Since the auth account of the request (*M*) is a reseller, the services doc of *M* is used to process the request, and since *M* is the boss, the request is processed and the device saved.

If we manually reconcile *R1* again, we see `"sip_device":3` as expected. *R1*, when synced with the bookkeeper, will be billed for that third device, so admins of *M* need to be aware when making this service changes.
