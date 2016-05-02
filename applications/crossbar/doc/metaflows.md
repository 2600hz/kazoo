

Metaflows allow functionality to be executed on an in-progress call, triggered by DTMFs from the caller/callee. For instance, a callee could setup a metaflow on their user doc such that when they receive a call, they can press "*9" to initiate a recording of the call.

#### Metaflow structure

Any document can be amended with a "metaflows" top-level key; however, at the moment Kazoo only processes a "metaflows" key on: the account doc, a callflow doc, a user doc, or a device doc.

##### Let's take a peek

Inside the "metaflows" object should be a familiar couple of keys, plus a couple metaflows-specific options:

* numbers: An object with keys that correspond to collected DTMF sequence after the binding_digit is pressed
* patterns: An object with keys of regexes to match against the collected DTMF sequence
* binding_digit: DTMF to trigger a metaflow; defaults to '*'
* digit_timeout_ms: how long to wait for another DTMF before processing the collected DTMFs
* listen_on: restrict which leg of the call to listen on for DTMF
    * "self": listen for DTMF on the leg of the device/user with the metaflow
    * "peer": listen on the opposite leg of the device/user
    * "both": listen to both legs of the call for DTMF

###### Numbers

The keys in the _numbers_ object represent the DTMF sequence to match, minus the binding_digit. If the caller presses '*234', the numbers object will be searched for a key of '234'.

The value of each key is the metaflow object to run on a match. It mirrors the callflow's action format:

* module: Which metaflow module to execute
* data: An object with module-specific data for execution
* children: An object of children metaflow actions (optional)

        "numbers":{
            "234":{
                "module":"play"
                ,"data":{"id":"media_id"}
            }
            ,"82824646":{
                "module":"tts"
                ,"data":{"text":"hello world"}
            }
        }

###### Patterns

The keys in the _patterns_ object represent regular expressions to be matched against the collected DTMF sequence (minus the binding\_digit as well). The collected DTMFs and the captures (if any) will be included in the _data_ payload.

First, an example _patterns_ object:

    "patterns":{
        "^1([0-9]{4})$":{
            "module":"callflow"
            ,"data":{}
        }
    }

This regex will match 1 and any four digits (let's say 2001). The _data_ object will go from empty to:

    "data":{
        "collected":"12001"
        ,"captured":["2001"]
    }

The callflow metaflow module, in this case, would look at the "captured" list and take the first element. Using that, it would look up a callflow and, if found, ask a callflow app to execute that extension's callflow against the call (why, I'm not sure yet, but it is there).

###### Binding Digit

What DTMF triggers a metaflow collection? Typically this would be '*' or '#', but could ostensibly be any DTMF.

    "binding_digit":"*"

###### Digit Timeout

How long to wait, in milliseconds, for the next DTMF. Once this timeout expires, the available metaflows will be searched, first numbers then patterns, for one that matches.

    "digit_timeout_ms":800

###### Listen On

Most of the time, a metaflow will only be concerned with receiving the DTMF from the call leg of the user/device configured with a metaflow. This is the "self" option (and the default if left unspecified). A metaflow can alternatively listen only to the other leg of the call using "peer", or to both sides of the call using "both".

    "listen_on":"self"

##### Putting it together

Remember, this "metaflows" object can be put on any account, callflow, user, or device doc.

    "metaflows":{
      "numbers":{
        "234":{
          "module":"play"
          ,"data":{"id":"media_id"}
        }
        ,"82824646":{
          "module":"tts"
          ,"data":{"text":"hello world"}
        }
      }
      ,"patterns":{
        "^1([0-9]{4})$":{
          "module":"callflow"
          ,"data":{}
        }
      }
      ,"binding_digit":"*"
      ,"digit_timeout_ms":800
      ,"listen_on":"self"
    }

#### Crossbar

Using Crossbar to modify metaflows is very simple. There are only three actions:

* GET - Gets the current metaflows on the document
* POST - Updates the metaflows on the document
* DELETE - Removes the metaflows object from the document

There are two URIs used to manipulate metaflows

##### Account Metaflow URI

`/v1/accounts/{ACCOUNT_ID}/metaflows`

This URI is used to manipulate the metaflows available to anyone in the account.

###### _GET_ - Fetch account metaflows:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/metaflows

###### _POST_ - Update account metaflows:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/metaflows -d '{"data":{"numbers":{"2":{"module":"tts","data":{"text":"2 pressed"}}},"binding_digit":"*","patterns": {"^1(\\d+)$": {"module": "callflow"}}}}'

###### _DELETE_ - Remove account metaflows:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/metaflows

##### Callflow/User/Device/etc Metaflow URI

`/v1/accounts/{ACCOUNT_ID}/{THINGS}/{THING_ID}/metaflows`

Here, `{THINGS}` would be "callflows", "users", "devices", etc, and `{THING_ID}` would be a callflow, user, device, or whatever id. Let's look at adding metaflows to a device.

###### _GET_ - Fetch device metaflows:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/metaflows

###### _POST_ - Update device metaflows:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/metaflows -d '{"data":{"numbers":{"2":{"module":"tts","data":{"text":"2 pressed"}}},"binding_digit":"*"}}'

###### _DELETE_ - Remove device metaflows:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/metaflows

#### Configuration

Metaflows can be configured to be started without needing to attach them to a device, user, etc. You can configure default metaflows on an account or across the system.

To start a metaflow handler for an account: `sup kapps_account_config set {ACCOUNT_ID} metaflows default_metaflow true`

To start a metaflow for all accounts: `sup kapps_config set_default metaflows default_metaflow true`

Now, this in and of itself isn't too useful
