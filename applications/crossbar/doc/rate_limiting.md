

API abuse can occur either maliciously or on accident; either way, the API server needs to protect itself from clients consuming too many server resources simultaneously.

#### Token Buckets

Crossbar (and Kazoo in general) has a [token bucket facility](https://en.wikipedia.org/wiki/Token_bucket) for tracking how many tokens a particular bucket has left. A bucket will be created for each client based on the IP of the client and the account ID being manipulated. As a client makes requests against the APIs, tokens will be deducted from the bucket until the bucket is exhausted, at which point API requests will return an [HTTP 429 error](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#4xx_Client_Error).

If you receive a 429, that means you're accessing the APIs too quickly and your bucket hasn't been replenished enough to permit the request to be processed.

#### Configuring API Costs

By default, every request costs 1 token (typically a bucket starts with 100 tokens which refill at 10 tokens per second). Administrators can optionally increase the cost of certain APIs (or methods on an API) to discourage heavy access patterns.

In the `system_config/crossbar` document, administrators can create a `token_costs` object to set the costs associated with a particular endpoint. Crossbar will check a series of keys, in order, to find the first one that returns a non-negative cost.

* If the account is defined in the URI:
    1. Check `{ACCOUNT_ID}.{ENDPOINT}.{HTTP_METHOD}`
    2. Check `{ACCOUNT_ID}.{ENDPOINT}`
    3. Check `{ACCOUNT_ID}`
    4. Check `{ENDPOINT}.{HTTP_METHOD}`
    5. Check `{ENDPOINT}`
* If no account id is on the URI, just check steps 4 and 5 above.

Concretely, if the request is `GET /v2/accounts/{ACCOUNT_ID}/callflows`, Crossbar will check:

1. Check `{ACCOUNT_ID}."callflows"."GET"`
2. Check `{ACCOUNT_ID}."callflows"`
3. Check `{ACCOUNT_ID}`
4. Check `"callflows"."GET"`
5. Check `"callflows"`

Note: `{HTTP_METHOD}` is always in all-caps for the key.

##### Sample configuration

Using the request above, you can configure the costs in the `crossbar` config in a variety of ways:

1. All requests to `callflows` cost the same

        {"_id":"crossbar"
         ,"default":{
             "token_costs":{
                 "callflows":2
             }
         }
        }

2. `callflows` vary in cost depending on method:

        {"_id":"crossbar"
         ,"default":{
             "token_costs":{
                 "callflows":{
                     "GET":1
                     ,"PUT":5
                     ,"POST":5
                     ,"DELETE":1
                 }
             }
         }
        }

3. Deduct flat amount for an account:

        {"_id":"crossbar"
         ,"default":{
             "token_costs":{
                 "{ACCOUNT_ID}":2
             }
         }
        }

4. Deduct flat amount for an account for a specific endpoint:

        {"_id":"crossbar"
         ,"default":{
             "token_costs":{
                 "{ACCOUNT_ID}":{
                     "callflows":10
                 }
             }
         }
        }

#### Disable token costs

If an administrator prefers to not use rate-limiting, or wants to set a flat-rate for all requests, configure the `token_costs` with just a number (0 to disable):

1. Disable token buckets

        {"_id":"crossbar"
         ,"default":{
             "token_costs":0
         }
        }

2. Flat cost across APIs - any integer > 0. This is how the default Crossbar operates.

        {"_id":"crossbar"
         ,"default":{
             "token_costs":1
         }
        }

#### Configuring Token Bucket start parameters

To configure the token buckets themselves, look in the `system_config/token_buckets` document.

* `max_bucket_tokens`: How many tokens to fill a bucket up to, defaulting to 100.
* `tokens_fill_rate`: How many tokens to add to a non-full bucket, defaulting to 10.
* `tokens_fill_time`: What time period to wait before adding `token_fill_rate` tokens, defaults to "second". Could also be "minute", "hour", or "day".

So the default bucket will have a maximum of 100 tokens, refilling at 10 tokens per second.

##### Per-Application configuration

An administrator can change the above parameters on a per-application basis. This would allow larger token limits for Crossbar-related buckets, and smaller limits for Callflow-related (for instance). Configure these per-application settings in the `token_buckets` document by creating an object with the application name as the key, and the parameters above as sub keys.

    {"_id":"token_buckets"
     ,"default":{
         "crossbar":{
             "max_bucket_tokens":250
             ,"tokens_fill_rate":10
             ,"tokens_fill_time":"minute"
         }
         ,"callflow":{
             "max_bucket_tokens":50
             ,"tokens_fill_rate":5
             ,"tokens_fill_time":"hour"
         }
     }
    }

#### Special Cases

There are some APIs that have extra rate limiting options for administrators to tweak.

##### Quickcall

Administrators can increase the cost of the quickcall API to keep call volume low from this endpoint.

Given a `GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{NUMBER}`

1. Check `{ACCOUNT_ID}."devices"."GET"."quickcall"`
2. Check `{ACCOUNT_ID}."devices"."quickcall"`
3. Check `{ACCOUNT_ID}."quickcall"`
4. Check `"devices"."GET"."quickcall"`
5. Check `"devices"."quickcall"`

So a configuration to make all quickcall requests cost 20 tokens would look like:

        {"_id":"crossbar"
         ,"default":{
             "token_costs":{
                 "devices":{
                     "quickcall":20
                 }
             }
         }
        }
