/*
Section: Reference
Title: Routing and Rating
Language: en-US
Version: 3.22
*/

# Rating and Routing

Topic Goal: Describe how Jonny5 and HotOrNot work. This implies also teaching people how authz and authorization work (a flowchart would be AMAZING). Things like how dry run works, etc would be extra-amazing.

-----

## Let's talk billing

People like to talk a lot about "billing".

What **IS** billing?

The request usually comes in the form "I just need something simple." That's usually a gross understatement of the actual desire.

-----

## It's complicated

Billing is a complicated topic. It helps to break it into pieces. You generally have:

* Recurring Subscriptions (monthly charges)
* Pro-rated monthly subscriptions
* Activation charges
* Per-minute / per-use charges
* Limits on spend (daily, monthly, pre-pay/credit base)
* Allocations of usage (400 minutes included - daily, monthly, etc.)
* Bursting / overage charges
* Rating of calls
* Pre-pay vs. post-pay
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

Today, we're NOT going to cover all these things. It's too much. We're going to cover only one section of this - rating and routing.

-----

## Rating & Routing in Kazoo

Kazoo's rating and routing functions are actually quite robust.

But they are not well documented so it can be easy to get lost in them.

Today, we're going to attempt to document and explain many of them.

## How we rate

Rating is done by HotOrNot:

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

## How we do limits

Limits are a concept of limiting how many flat-rate (included) calls are available which WON'T actually result in a charge.

You can limit based on:

* Inbound
  * Limit the number of simulataneous inbound calls that can be received
* Outbound
  * Limt the number of simulataneous outbound calls that can be made
* Resource consuming
  * Any endpoint the system operators likely pay (upstream carriers generally)
  * Limit the number of calls that can consume resources (internal calls unaffected)
* Burst
  * Allows account to consume more trunks than the base number allotted, typically for short intervals
  * Good for seasonal, customer support, radio shows, call centers, schools, etc
* Prepay
  * Pay up front, deduct until 0
  * No simulataneous call limit
* Postpay
  * Basically prepay that can go negative
* Allotments
  * Buckets of minutes per time-period
    * Monthly, Weekly, Daily, Hourly, Minutely (seriously)
* Hard/Soft Limits
  * Hard limits are unbreachable
  * Soft limits are temporarily breachable

Emergency calls are immediately authorized, as are outbound calls to tollfree numbers.

-----

## Limit Flows Up

1. Check account limits
2. If exhausted, check reseller limits

If a limit is reached or not found, then we fail back to use credit based on the
rates (if enabled).

-----

## How we log and track balances

Talk about the MODB database

* MODB databases
* Monthly rollovers
* Strengths (this is basically like writing a text file)
* Weaknesses (not great for searching)

-----

## Why our approach is unique and different

* We rate real-time
* Helps with fraud
* Scales by accounts, so technically infinitely

-----

## How it works

Flowchart?

-----

## How to set it up

IF TIME, show off relevant system_config options

-----

## How to check that it's operating

Sup commands?

-----

## How to read the logs

Why did a call get rated but not billed?
Or how do you know if you're over limits?
Etc.

-----

## What to do when it doesn't work

Helpful sup commands
Performance impact
