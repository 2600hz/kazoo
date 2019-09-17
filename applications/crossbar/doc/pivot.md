# Pivot API

## About Pivot

The Pivot Crossbar resource allows the client to query and inspect data related to the [Pivot](../../pivot/doc/README.md) application (real-time call control).

## Enabling in Crossbar

The Pivot endpoint is not loaded on start in a default Kazoo installation.

* To enable at runtime:
    * `sup crossbar_maintenance start_module cb_pivot`
* To autostart on Crossbar boot:
    * Navigate to `http://localhost:15984/_utils/document.html?system_config/crossbar`
    * Edit the `autoload_modules` list to include 'cb_pivot'
    * Click the green check box to the right of the input box
    * Click 'Save Document' in top left of the screen

!!! note
    Adding `cb_pivot` to the crossbar `system_config` doc will not start the endpoint; only on restarting Crossbar will `cb_pivot` be loaded. Use the `sup` command above to start the endpoint at runtime.

## Callflow Schema

#### Schema

Validator for the Pivot callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`cdr_url` | Optional URL to send the CDR to at the end of the call | `string()` |   | `false` |
`debug` | Store debug logs related to processing this Pivot call | `boolean()` | `false` | `false` |
`method` | What HTTP verb to send the request(s) with | `string('get' | 'post' | 'GET' | 'POST')` | `get` | `false` |
`req_body_format` | What format should the request body have | `string('form' | 'json')` | `form` | `false` |
`req_format` | What format of Pivot will the your server respond with | `string('kazoo' | 'twiml')` | `kazoo` | `false` |
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |
`voice_url` | What URL to request the initial Pivot callflow | `string()` |   | `true` |



!!! note
    `cdr_url` is only applicable when using the XML (TwiML) format. When using the kazoo format, control is handed off to the Callflows app, with the Pivot process ending (and nothing waiting for the CDR). Instead, please use [webhooks](./webhooks.md) (specifically the CHANNEL_DESTROY event) to receive CDRs.

## Debugging pivot attempts

You will need to edit the `data` object in the `pivot` callflow element to include a `debug` flag:

```json
{
    "flow": {
      "data": {
        "method": "GET",
        "req_format": "kazoo",
        "voice_url": "http://your.pivot.server/path/to/callflow.php",
        "debug": true
        },
      "module": "pivot",
      "children": {
      }
    }
}
```

All calls to this callflow will now store debug logs to the account's current MODb database.


## Fetch a List of Debug UUIDs

> GET /v2/accounts/{ACCOUNT_ID}/pivot/debug

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/pivot/debug
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "call_id": "{UUID_1}",
            "created": 63635231906,
            "iteration": 1,
            "node": "{PIVOT_SERVER}",
            "status_code": "404",
            "has_schema_errors": false,
            "uri": "http://127.0.0.1/pivot/kazoo_4786.php?Language=en-us&Caller-ID-Number=user_suyt9r93ng&Caller-ID-Name=user_suyt9r93ng&Direction=inbound&Api-Version=2015-03-01&To-Realm={SIP_REALM}&To=4786&From-Realm={SIP_REALM}&From=user_suyt9r93ng&Account-ID={ACCOUNT_ID}&Call-ID={UUID_1}"
        },
        {
            "call_id": "{UUID_2}",
            "created": 63635230409,
            "iteration": 1,
            "node": "{PIVOT_SERVER}",
            "has_schema_errors": true
        }
      ],
     "page_size": 3,
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success",
}
```

## Fetch Debug Logs for a UUID

> GET /v2/accounts/{ACCOUNT_ID}/pivot/debug/{UUID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/pivot/debug/{UUID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}"
     ,"data": [{"call_id": "829597750@10.26.0.158"
                ,"id": "b791e38c9641652a69e297dc9c3a8d66"
                ,"method": "get"
                ,"req_body": ""
                ,"req_headers": {}
                ,"uri": "http://{PIVOT_SERVER}/path/to/callflow.php?CallerNumber={CALLER_ID_NUMBER}&CallerName={CALLER_ID_NAME}&Direction=inbound&ApiVersion=2010-04-01&ToRealm={TO_SIP_REALM}&To={DIALED_NUMBER}&FromRealm={FROM_SIP_REALM}&From={SIP_FROM_USER}&AccountSid={ACCOUNT_ID}&CallSid=829597750%4010.26.0.158"
               }
               ,{"call_id": "829597750@10.26.0.158"
                 ,"id": "f071ae42d9bcebd158f263258e73b001"
                 ,"resp_headers": {
                   "content-length": "303"
                   ,"content-type": "text/html"
                   ,"date": "fri, 30 may 2014 20:42:53 gmt"
                   ,"server": "apache/2.4.7 (ubuntu)"
                 }
                 ,"resp_status_code": "404"
               }
               ,{"call_id": "829597750@10.26.0.158"
                 ,"id": "79604993e4dbe962872a71fe6cbc9717"
                 ,"resp_body": "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>404 Not Found</title>\n</head><body>\n<h1>Not Found</h1>\n<p>The requested URL /path/to/callflow.php was not found on this server.</p>\n<hr>\n<address>Apache/2.4.7 (Ubuntu) Server at {PIVOT_SERVER} Port 80</address>\n</body></html>\n"
                 }
               ]
      ,"request_id": "{REQUEST_ID}"
      ,"revision": "{REVISION}"
      ,"status": "success"
     }
```

!!! note
    You must URL-encode the call-id in the URL. Typically this would just mean converting `@` to `%40`, but you'll need to take care depending on how your call-ids are constructed.
