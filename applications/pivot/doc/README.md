# Pivot Intro

## Overview

The Pivot gives developers greater control over callflows than what comes natively in Kazoo. Pivot attempts to corral the salient data and send it, via HTTP to the developer's web server. Pivot expects a response with appropriate XML or JSON, and will execute the callflow returned on behalf of the developer.

### Example Callflow

The most basic callflow for Pivot:

```json
{
 "flow":{
     "module":"pivot"
     ,"data":{
         "voice_url":"http://your.pivot.server/path/to/script.php"
         ,"req_format":"kazoo"
         ,"method":"get"
         ,"debug":false
     }
 }
}
```

## Response Formats

* [Kazoo JSON](./kazoo/README.md)
* [TwiML](./twiml/README.md)

## Debugging

You can set the `debug` flag to "true" to log the requests and responses Pivot receives from your Pivot Callflows.

### Summary

Get a list of recent Pivot attempts:

```shell
$ curl -H "X-Auth-Token: {AUTH_TOKEN} \
     -H "Content-Type: application/json" \
     'http://your.crossbar.server/v2/accounts/{ACCOUNT_ID}/pivot/debug'
```

Response:

```json
{"data":["{CALL_ID}"],"revision":"undefined","request_id":"{REQUEST_ID}","status":"success","auth_token":"{AUTH_TOKEN}"}
```

## Specific Call

Get details of a specific Pivot attempt:

```shell
$curl -H "X-Auth-Token: {AUTH_TOKEN} \
    -H "Content-Type: application/json" \
    'http://your.crossbar.server/v2/accounts/{ACCOUNT_ID}/pivot/debug/{CALL_ID}'
```

Response:

```json
{
    "auth_token": "{AUTH_TOKEN}",
        "data": [
        {
            "call_id": "{CALL_ID}",
            "id": "{DEBUG_ID}",
            "method": "get",
            "req_body": "",
            "req_headers": {},
            "uri": "http://your.pivot.server/script.php?Caller-ID-Number=XXXXXXXXXX&Caller-ID-Name=JoeFromIT&Direction=inbound&ApiVersion=2013-05-01&ToRealm=your.pivot.server&To=3004&FromRealm=your.pivot.server&From=user_sov2kt&Account-ID={ACCOUNT_ID}&Call-ID={CALL_ID}"
        },
        {
            "call_id": "{CALL_ID}",
            "id": "{DEBUG_ID}",
            "resp_body": "\n    {\"module\":\"say\"\n     ,\"data\":{\"text\":\"Please leave your message after the beep\"}\n     ,\"children\":{\n         \"_\":{\n           \"module\":\"record_caller\"\n           ,\"data\":{\n               \"format\":\"mp3\"\n               ,\"url\":\"http://your.pivot.server/recordings\"\n               ,\"time_limit\":360\n           }\n         }\n        }\n    }\n"
        },
        {
        "call_id": "{CALL_ID}",
        "id": "{DEBUG_ID}",
        "resp_headers": {
            "content-length": "342",
            "content-type": "application/json",
            "date": "wed, 01 oct 2014 23:30:24 gmt",
            "server": "apache/2.4.7 (ubuntu)",
            "x-powered-by": "php/5.5.9-1ubuntu4.4"
        },
        "resp_status_code": "200"
    }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
    }
```

Remember to URL-encode the `{CALL_ID}` before sending the request.

## Failback

You can add a children to your pivot callflow in case your server is unreachable or send back an error.

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
