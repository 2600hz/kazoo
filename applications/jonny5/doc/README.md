# Jonny5

Based on the lovable robot from the Short Circuit franchise, jonny5 tracks active calls for accounts and applies limits based on the account's credit and trunking limits.

## Messages processed

* `j5_listener` (shared queue):
 authz_req -> j5_channels:authorized -> inserts authz_resp to j5_channels table
              broadcast authz resp

 channel_destroy -> updates ledgers for per-minute

* `j5_channels` (solo queue):
 broadcasted authz resp -> inserts broadcasted authz_resp to j5_channels table

 hook register (solo queue) -> channel_create => insert_new from call event
                               channel_destroy => update if destroyed=false

## PropEr testing

See `test/j5_channels_pqc.erl`. Creates model to test that no stuck channels occur with different interleavings of broadcasted `authz_resp` payloads and `CHANNEL_CREATE/DESTROY` events.
