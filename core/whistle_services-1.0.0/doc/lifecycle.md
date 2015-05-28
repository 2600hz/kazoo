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
             "sip_devices":{
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
