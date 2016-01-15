### Pivot

The Pivot Crossbar resource allows the client to query and inspect data related to the [Pivot](/applications/pivot) application (real-time call control).

#### Enabling in Crossbar

The Pivot endpoint is not loaded on start in a default Kazoo installation.

* To enable at runtime:
    * `sup crossbar_maintenance start_module cb_pivot`
* To autostart on Crossbar boot:
    * Navigate to `http://localhost:15984/_utils/document.html?system_config/crossbar`
    * Edit the `autoload_modules` list to include 'cb_pivot'
    * Click the green check box to the right of the input box
    * Click 'Save Document' in top left of the screen

Note: adding `cb_pivot` to the crossbar `system_config` doc will not start the endpoint; only on restarting Crossbar will `cb_pivot` be loaded. Use the [sup](./sup.md) command above to start the endpoint at runtime.

#### Usage

You will need to edit the "data" object in the "pivot" callflow element to include a "debug" flag:

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

All calls to this callflow will now store debug logs to the account's current MODb database.

##### Query available debugged calls

    curl -v -H "X-Auth-Token: {AUTH_TOKEN}" 'http://{CROSSBAR_SERVER}:8000/v2/accounts/{ACCOUNT_ID}/pivot/debug'
    [REQUEST HEADERS]
    [RESPONSE HEADERS]
    {"auth_token": "{AUTH_TOKEN}"
     ,"data": [
         "829597750@10.26.0.158"
     ]
     ,"request_id": "{REQUEST_ID}"
     ,"revision": "undefined"
     ,"status": "success"
    }

The `data` key will contain a list of recently debugged calls.

##### Query debugged call

Take a call-id from the list to query for the debugging details:

    curl -v -H "X-Auth-Token: {AUTH_TOKEN}" 'http://{CROSSBAR_SERVER}:8000/v2/accounts/{ACCOUNT_ID}/pivot/debug/829597750%4010.26.0.158'
    {"auth_token": "{AUTH_TOKEN}"
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
      ,"request_id": "fb455599cf9f1390c5efa1d948d41d2b"
      ,"revision": "f231fd438e5ba812cac542bff00e636d"
      ,"status": "success"
     }

Note: You must URL-encode the call-id in the URL. Typically this would just mean converting `@` to `%40', but you'll need to take care depending on how your call-ids are constructed.

The resulting `data` will be a list of any requests made to your server, and the response headers/body received.
