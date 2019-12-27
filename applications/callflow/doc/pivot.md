# Pivot

## About Pivot

Execute an HTTP request to a web server about the call, expecting more callflow instructions in the response.

#### Schema

Validator for the Pivot callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`cdr_url` | Optional URL to send the CDR to at the end of the call | `string()` |   | `false` |  
`debug` | Store debug logs related to processing this Pivot call | `boolean()` | `false` | `false` |  
`method` | What HTTP verb to send the request(s) with | `string('get' | 'post' | 'GET' | 'POST')` | `get` | `false` |  
`req_body_format` | What format should the request body have when using POST | `string('form' | 'json')` | `form` | `false` |  
`req_format` | What format of Pivot will the your server respond with | `string('kazoo' | 'twiml')` | `kazoo` | `false` |  
`req_timeout_ms` | How long, in milliseconds, to wait for a Pivot response from the HTTP server | `integer()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`voice_url` | What URL to request the initial Pivot callflow | `string()` |   | `true` |  






##### TwiML

TwiML support is limited at the moment; KAZOO JSON is highly encouraged.

!!! note
    `cdr_url` is only applicable when using the XML (TwiML) format. When using the kazoo format, control is handed off to the Callflows app, with the Pivot process ending (and nothing waiting for the CDR). Instead, please use [webhooks](./webhooks.md) (specifically the CHANNEL_DESTROY event) to receive CDRs.

## Handling failures

The Pivot request can fail for a number of reasons:

* DNS resolution of the web server fails
* Connection (TLS or clear) to the web server fails
* Response is not of a known `content-type`
* Response is not valid callflow JSON

The pivot callflow action waits until either
  1. The Pivot response is processed successfully, in which case the pivot callflow action exits quietly
  2. The Pivot response fails (for whatever reason), in which case the pivot callflow action goes to the default `_` child branch (if any).

In the example below, if an error occurs when getting a response from `{SERVER_URL}`, the caller will hear the media at `{MEDIA_ID}` played and the call will end.

```json
"flow": {
    "data": {
        "method": "GET",
        "req_timeout": "5",
        "req_format": "kazoo",
        "voice_url": "{SERVER_URL}"
    },
    "module": "pivot",
    "children": {
        "_": {
            "module": "play",
            "data": {
                "id": "{MEDIA_ID}"
            },
            "children": {}
        }
    }
}
```
