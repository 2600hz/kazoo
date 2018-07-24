### Call Inspector

#### About Call Inspector

The Call Inspector Crossbar resource allows the client to query and inspect data related to the Call Inspector application.

[More info on Call Inspector](https://github.com/2600hz/kazoo/blob/master/applications/call_inspector/doc/README.md).


The Call Inspector endpoint is not loaded on start in a default Kazoo installation.

* To enable at runtime:
    * `sup crossbar_maintenance start_module cb_call_inspector`
* To autostart on Crossbar boot:
    * Navigate to `http://localhost:15984/_utils/document.html?system_config/crossbar`
    * Edit the `autoload_modules` list to include 'cb_call_inspector'
    * Click the green check box to the right of the input box
    * Click 'Save Document' in top left of the screen

Note: adding cb_call_inspector to the crossbar system_config doc will not start the endpoint;
only on restarting Crossbar will cb_call_inspector be loaded.
Use the *sup* command above to start the endpoint at runtime.


#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/call_inspector

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/call_inspector
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {CALL_ID1},
        {CALL_ID2}
    ]
    "status": "success"
}
```

#### Read a call's SIP dialogue

> GET /v2/accounts/{ACCOUNT_ID}/call_inspector/{CALL_ID}

* `{CALL_ID}` is the unique string identifying a call. Call has to be under the authority of `{ACCOUNT_ID}`.
* `{ACCOUNT_ID}` has to be a reseller's account id.

Note: `{CHUNKS}` is an array of JSON-formatted chunks.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/call_inspector/{CALL_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "analysis": [],
        "messages": {CHUNKS}
    }
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
