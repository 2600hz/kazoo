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
    }

We can see that the initial quantities of "service-able" things have been initialized. Since the account has nothing in it yet, "quantities" only contains the service categories (except for ips, which has a service item "dedicated" - a real overachiever, that one!).

We can also see the plans applied to this account. We could create all sorts of different plans and apply multiple to this account.
