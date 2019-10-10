
# Rating and Limits

Topic Goal: Describe how Jonny5 and Hotornot work. This implies also teaching people how authz and authorization work (a flowchart would be AMAZING). Things like how dry run works, etc would be extra-amazing.

-----

## Let's talk billing

People like to talk a lot about "billing".

What **IS** billing?

The request usually comes in the form "I just need something simple." That's usually a gross understatement of the actual desire.

-----

## It's complicated

The truth is, billing is a complicated topic. It helps to break it into pieces. You generally have:

* Recurring Subscriptions (monthly charges)
* Pro-rated monthly subscriptions
* Activation charges
* Per-minute / per-use charges
* Limits on spend (daily, monthly, pre-pay/credit base)
* Allocations of usage (400 minutes included - daily, monthly, etc.)
* Bursting / overage charges
* Rating of calls
* Pre-pay vs. post-pay
* Auto top up
* Deposit tracking
* Accounts Payable / Accounts Receivable
* Strategies for warning customers
  * low balances
  * overdue
  * expiring credit card
* Discounts
  * one time
  * continuous
* Rate decks (global + account based)
* Actual cost analysis

-----

## Rating & Routing in Kazoo

Today, we're NOT going to cover all these things. It's too much. We will give you a general overview of what exists today, how to configure it, how to monitor it, and how to parse the logs a bit to see what's going on.

Kazoo's rating and routing functions are actually quite robust.

But they are not well documented so it can be easy to get lost in them.

# Rating
## What a rate document looks like

```
{"prefix":"1"
 ,"iso_country_code":"US"
 ,"description":"US default rate"
 ,"direction":"both"
 ,"rate_name":"US-1"
 ,"routes":["^\\+1\\d+$"]
 ,"options":[]
 ,"weight":10
 ,"rate_increment":60
 ,"rate_minimum":60
 ,"rate_surcharge":1.00
 ,"rate_cost":0.01
}
```

-----

## How Kazoo rates

Rating is done by Hotornot:

1. Searches for rate in global rate deck matching the normalized dialed number
   * Breaks number into all possible prefixes
   * +14158867900 => [1, 14, 141, 1415, 14158, ...]
   * Matches all rate docs with prefixes in the above list
2. Match rates found against various criteria
   * Call Direction (inbound, outbound, both)
   * Options - match route options+flags against rate options list
     * each route option/flag must exist in rate options list
   * Routes - match rate regex to dialed number
3. Sort matched rates
   * Larger prefix first (1415 matches before 141)
   * Weight parameter if prefix lengths are the same
4. Set various parameters on the call for tracking per-minute costs, if any

-----

## How to test your rates

Test what rate is selected for a given dialed number:

```
sup hotornot_maintenance rates_for_did 4158867900
Candidates:
    RATE NAME |      COST | INCREMENT |   MINIMUM | SURCHARGE |    WEIGHT |          PREFIX |
 US-1-INBOUND |    0.0049 |        60 |        60 |      0.00 |         2 |               1 |
US-1-OUTBOUND |    0.0089 |        60 |        60 |      0.00 |         2 |               1 |
Matching:
      RATE NAME |      COST | INCREMENT |   MINIMUM | SURCHARGE |    WEIGHT |          PREFIX |
* US-1-OUTBOUND |    0.0089 |        60 |        60 |      0.00 |         2 |               1 |
   US-1-INBOUND |    0.0049 |        60 |        60 |      0.00 |         2 |               1 |
```

# Limits
## How Kazoo does limits

Limits are a concept of limiting how many flat-rate (included) calls are available which WON'T actually result in a charge.

You can limit based on:

* Inbound
  * Limit the number of simultaneous inbound calls that can be received
  * Limit the number of simultaneous inbound calls that can be received per DID number (regex based)
* Outbound
  * Limit the number of simultaneous outbound calls that can be made
* Two-way
  * Limit the number of simultaneous calls that can be made
* Resource consuming
  * Any endpoint the system operators likely pay (upstream carriers generally)
  * Limit the number of calls that can consume resources (internal calls unaffected)
* Burst
  * Allows account to consume more trunks than the base number allotted, typically for short intervals
  * Good for seasonal, customer support, radio shows, call centers, schools, etc
* Bundled Trunks
  * Inbound, Outbound, Twoway as well
  * Limit determined by the number of users or devices (configurable)
  * `"twoway_bundled":"user"`, for example
* Prepay
  * Pay up front, deduct until 0
  * No simultaneous call limit
* Postpay
  * Basically prepay that can go negative
* Allotments
  * Buckets of minutes per time-period
    * Monthly, Weekly, Daily, Hourly, Minutely (seriously)

Emergency calls are immediately authorized, as are outbound calls to tollfree numbers.

Each CDR is augmented with two fields showing the trunk usage for the account and the reseller at the time of the call. The format is `{INBOUND}/{OUTBOUND}/{TWOWAY}/{BURST}`.

-----

## How Limits are checked

Both the account making the call and the reseller of the account **must** authz the call.

1. Check `pvt_enabled` in account
   * if false, permit call
2. Check classification of call
   * if `emergency` or `tollfree_us` (and call direction is `outbound`), permit call
3. Check call limits
4. Check resource-consuming call limits
5. Check allotments
6. Check flat rate trunks
   * Check dialed number against white/black lists
   * Check inbound/outbound trunks available
   * Check two-way trunks available
   * Check account burst trunks
7. Check per-minute funds
   * Check pre-pay credit
   * Check post-pay credit

-----

## How we log and track balances

Track temporal data in temporal databases

* Affectionately called MODBs (month-only databases)
* Stores temporal data for a given month (account_id-yyyymm)
* Keeps account database small and fast
* Monthly rollovers for transactions and other ledger-based work
* Views with map/reduce maintain the month's balances
* Once out of scope, MODBs can be archived and deleted

-----

## How we log and track balances

Jonny5 also tracks active channels in an in-memory cache.

SUP commands later will help you inspect that cache.

-----

## Why our approach is unique and different

* Rate real-time, in parallel with authorization
* Helps with fraud
* Scales by accounts, so technically infinitely
* Tracks funny money
  * Allows external billing systems
  * Admins can easily apply credits to accounts

-----

## How to set authz up

* Enable authorization on calls
   `sup kapps_config set_default ecallmgr authz_enabled true`
   `sup kapps_config flush ecallmgr`
   `sup -n ecallmgr kapps_config flush ecallmgr`
* Authorize local resource usage
   `sup kapps_config set_default ecallmgr authz_local_resources true`
   `sup kapps_config flush ecallmgr`
   `sup -n ecallmgr kapps_config flush ecallmgr`
* Dry Run authz attempts (useful when testing authz)
   `sup kapps_config set_default ecallmgr authz_dry_run true`
   Still allows a call that would have been denied
* Required a rate to continue call
  `sup kapps_config set_default ecallmgr {DIRECTION}_rate_required true`
  If enabled, ensures a rate is found for the leg; otherwise kills the channel
* Default Authz action (if authz request fails)
  `sup kapps_config set_default ecallmgr authz_default_action deny`
  Alternative is `allow`

-----

## How to set account limits up

Add limits to an account:

```
POST /v2/accounts/{ACCOUNT_ID}/limits
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "allow_prepay": true,
        "outbound_trunks": 5
    }
}
```

Check the [limits schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/limits.json) for various limits to be set here and read more about the [limits API](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/limits.md).

## How to set manual limits up

System admins can manually restrict an account's limits:

Prefix any of the limit doc's keys with `pvt_` will restrict the account's ability to set that limit. So if `inbound_trunks` is set to 11 as in the payload above, and `pvt_inbound_trunks` exists and is set to 5, the account is really limited to 5 trunks.

Admins can also disable limits entirely for the account (useful on the top-level account). Setting '"pvt_enabled":false`, on the account's limit doc results in all calls being authorized by that account (reseller authz still applies if not top level account).

## How to check that limits are operating

```
sup jonny5_maintenance authz_summary [{ACCOUNT_ID}]
+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+
| Account ID                       | Calls | Resource Calls | Allotments | Inbound Trunks | Outbound Trunks | Per Minute |
+==================================+=======+================+============+================+=================+============+
|{ACCOUNT_ID_1}                    | 1     | 1              | 0          | 1              | 0               | 0          |
+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+
| {ACCOUNT_ID_2}                   | 1     | 1              | 0          | 1              | 0               | 0          |
+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+
| {ACCOUNT_ID_3}                   | 1     | 1              | 0          | 1              | 0               | 0          |
+----------------------------------+-------+----------------+------------+----------------+-----------------+------------+
```

```
sup jonny5_maintenance limits_summary [{ACCOUNT_ID}]
+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+
| Account ID                       | Calls | Resource Calls | Allotments |           Trunks         | Per Minute | Max Postpay |
|                                  |       |                |            |  In | Out | Both | Burst |            |             |
+==================================+=======+================+============+==========================+============+=============+
| {ACCOUNT_ID_1} | -1    | -1             | 0          | 3   | 0   | 3    | 0     | 24.6685    | disabled    |
+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+
| {ACCOUNT_ID_2} | -1    | -1             | 0          | 20  | 0   | 20   | 0     | 4814.201   | disabled    |
+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+
| {ACCOUNT_ID_3} | -1    | -1             | 0          | 0   | 0   | 0    | 0     | 2693.6755  | -5000.0     |
+----------------------------------+-------+----------------+------------+--------------------------+------------+-------------+
```

## How to credit/debit an account

Credit in Kazoo is not tied to a billing system. Administrators can add or remove funds from an account as they need:

* `sup kazoo_services_maintenance credit {ACCOUNT_ID} 5.0`
* `sup kazoo_services_maintenance debit {ACCOUNT_ID} 5.0`

Each command above will add/remove 5 dollars to/from the account.

-----

## How to read the logs

```
|{CALL_ID}|j5_request:186 (<0.15795.437>) account {ACCOUNT_ID} authorized channel: per_minute
|{CALL_ID}|j5_request:177 (<0.15795.437>) reseller {RESELLER_ID} authorized channel: per_minute
```

This call has be authorized as per_minute by both the account and reseller

-----

## How to read the logs

```
|{CALL_ID}|j5_request:186 (<0.29272.139>) account {ACCOUNT_ID} authorized channel: flat_rate
|{CALL_ID}|j5_request:177 (<0.29272.139>) reseller {RESELLER_ID} authorized channel: flat_rate
```

This call is consuming a flat rate trunk

-----

## How to read the logs

```
|{CALL_ID}|j5_authz_req:153 (<0.14181.140>) allowing outbound tollfree call
|{CALL_ID}|j5_request:186 (<0.14181.140>) account {ACCOUNT_ID} authorized channel: limits_disabled
|{CALL_ID}|j5_request:177 (<0.14181.140>) reseller {RESELLER_ID} authorized channel: limits_disabled
```

This call was authorized because it is an outbound tollfree call

-----

## How to set up inbound calls per DID limit

Ensure authz is enabled including the authz_local_resources.
Add "inbound_channels_per_did_rules" flag to account's limits doc:

```
   "pvt_inbound_channels_per_did_rules": {
       "456": 3,
       "^876512.": 5,
       "^\\+?78122404700$": 1
   }
```

In the case where the DID number matches one of provided regular expressions in the rule keys, the number of simultaneous calls will be limited to the configured amount specified in matching rule's value.
In the case that no rules match the DID, no limitation will be applied to the number of simultaneous calls for that DID.

-----
