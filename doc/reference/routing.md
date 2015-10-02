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

* Normalizes your number dialed? Yes? (how?)
* Checks it against a database?
* Selects a rate based on (weight? Longest match?)
* Adds the rate to the call as a variable (doesn't necessarily mean they're charged)

-----

## FLOWCHART!

-----

## How we do limits

Limits are a concept of limiting how many flat-rate (included) calls are available which WON'T actually result in a charge.

You can limit based on:

* Inbound
* Outbound
* Resource consuming (define what this is)
* Burst
* Prepay
* Postpay
* Allotments
* Hard/Soft Limits

__Clarify each__

-----

## Limit Flows Up

1. Account limits
2. Reseller limits

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
