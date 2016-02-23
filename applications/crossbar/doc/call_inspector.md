/*
Section: Crossbar
Title: Call Inspector
Language: en-US
*/

# The Call Inspector Crossbar resource

The Call Inspector Crossbar resource allows the client to query and inspect data related to the Call Inspector application.

[More info on Call Inspector](https://github.com/2600hz/kazoo/blob/master/applications/call_inspector/doc/index.md).

## Enabling in Crossbar

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

## Usage

GET a call's dialogue by **callid**:

    curl -X GET -H 'Content-Type: application/json' -H 'X-Auth-Token: {AUTH_TOKEN}' http://{CROSSBAR_SERVER}:8000/v2/accounts/{ACCOUNT_ID}/call_inspector/{CALL_ID} | python -mjson.tool

* `{AUTH_TOKEN}`, `{CROSSBAR_SERVER}` should be straightforward.
* `{CALL_ID}` is the unique string identifying a call. Call has to be under the authority of `{ACCOUNT_ID}`.
* `{ACCOUNT_ID}` has to be a reseller's account id.

Example:

    $ curl -H 'x-auth-token: 077dceaa5420167953530ee604fdb1ca' http://localhost:8000/v2/accounts/4b8c6fec4b2597882c0390202d195419/call_inspector/1f0444b2-514176ff-ec0c0a38@10.26.0.114 -s | python -mjson.tool
    {
      "auth_token": "077dceaa5420167953530ee604fdb1ca",
      "data": {
        "analysis": [],
        "messages": {CHUNKS}
      }
      "request_id": "6a10fe5e916c1982d8b910e59bed7915",
      "revision": "undefined",
      "status": "success"
    }

Where `{CHUNKS}` is an array of JSON-formated chunks.
