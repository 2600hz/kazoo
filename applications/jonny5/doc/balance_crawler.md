/*
Section: Jonny5
Title: Balance crawler
Language: en-US
*/

# Balance crawler
Disconnects active per_minute calls when account balance drops below zero.

## Prerequisites
Enable `system_config/ecallmgr/broadcast_route_win_to_queues` - add `jonny5` to list

## Crawler configuration options
Document - `system_config/jonny5`
Options:
- `balance_crawler_enabled` - boolean, default `false`. Enable crawler.
- `balance_crawler_cycle_ms` - integer, milliseconds, default 60000 (1 minute). How often crawler check accounts balance.
- `balance_crawler_interaccount_delay_ms` - integer, miliseconds, default 10. Deley before process next account.
- `balance_crawler_delayed_hangup` - boolean, default `true`. When disconnect call, delay hangup for `Rate-Increment`.

## Per-account settings
You can set boolean option `pvt_disconnect_active_calls` in per-account `limits` document.
If this options not set, then used options `default_disconnect_active_calls` from system-wide document `system_config/jonny5`.
