### Formatters

#### About formatters

It is all about control!

With [numbers](https://github.com/2600hz/kazoo/blob/master/doc/internationalization/numbers.md) controlling how Kazoo processes phone numbers into and out of the system, it only made sense to add functionality to control the format of other fields that are commonly found.

Enter *formatters*!

#### Carriers

When a call enters Kazoo from a carrier, Stepswitch receives the route request first (since no account information exists to identify the context of the call). Stepswitch will process this request to figure out what account the request is destined for and will replay the route request with the associated information.

If the call came from a known resource, you can optionally define formatters to take the route request headers and format the values before they are replayed to the call processing applications.

On the resource document, define a key `Formatters` which is a JSON object of route request headers as keys and their value being an object (or list of objects) of regexps, prefixes, and suffixes.

    {"_id":"some_resource_id"
     ,...
     ,"formatters":{
         "request":[{...}]
         ,"from":[{...},{...}]
         ,"outbound_caller_id_number":{...}
         ...
     }

In the above partial example, the resource has defined formatters for the `request` and `caller-id-number` fields, and a list of two formatters for the `from` field.

For outbound (to the carrier) formatters, you can see the fields available by looking at the [kapi_offnet_resource request schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/kapi.offnet_resource.req.json).

Inbound (from the carrier) fields are found in the [kapi_route request schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/kapi_route.req.json).

In both of the above cases the request, to and from fields are tracked outside of the mentioned schemas. 

#### Devices, users, accounts

It can be desirable to control the format of fields going to registered devices. You can place the `formatters` object on a device, user, or account, to have it be used when processing calls to endpoints. The `formatter` object(s) will be merged before being applied to the endpoint.

Inbound fields are found in the [kapi_route request schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/kapi_route.req.json); outbound fields are typically found when [bridging via kapi_dialplan](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/kapi.dialplan.bridge.json).

## Formatter format

Okay, so what goes in the `{...}` portions of the example above? The generic formatter looks like this:

    {"regex":"^som(e_r)egex$"
     ,"prefix":"some_prefix"
     ,"suffix":"some_suffix"
     ,"strip":boolean()
     ,"match_invite_format":boolean()
     ,"direction":direction()
     ,"value":"static_value"
    }

#### Schema

Schema for formatters

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`^[[:alnum:]_]+$` |   | `object` |   | `false`
`^[[:alnum:]_]+$.value` | Replaces the current value with the static value defined | `string` |   | `false`
`^[[:alnum:]_]+$.suffix` | Appends value against the result of a successful regex match | `string` |   | `false`
`^[[:alnum:]_]+$.strip` | If set to true, the field will be stripped from the payload | `boolean` |   | `false`
`^[[:alnum:]_]+$.regex` | Matches against the value, with optional capture group | `string` |   | `false`
`^[[:alnum:]_]+$.prefix` | Prepends value against the result of a successful regex match | `string` |   | `false`
`^[[:alnum:]_]+$.match_invite_format` | Applicable on fields with SIP URIs. Will format the username portion to match the invite format of the outbound request. | `boolean` |   | `false`
`^[[:alnum:]_]+$.direction` | Only apply the formatter on the relevant request direction | `string('inbound', 'outbound', 'both')` |   | `false`



#### Running Formatters

The regex will be run against the value of the header and if it matches, will take the capture group (or the whole value if no capture group is defined) and prepend the prefix and append the suffix (if applicable).

There is one caveat to that: when processing a regex for the `request`, `to`, or `from` fields, the value applied to the regex is the username portion, not the full `user@hostname` value. The prefix and suffix will be applied to the captured portion of the username, and the `@hostname` is appended as the last step.

A more full example:

    {"_id":"resource_id"
     ,"formatters":{
         "from":[
             {"regex":"^\\+?1?(\\d{10})$"
              ,"prefix":"+1"
              ,"direction":"inbound"
             }
             ,{"regex":"\\+?1?(\\d{10})$"
               ,"direction":"outbound"
              }
          ]
          ,"diversion":[{
              "match_invite_format":true
              ,"direction":"outbound"
          }]
          ,"outbound_caller_id_name":[{
              "value":"Kazoo"
          }]
      }
      ,...
    }

This will:

1. Format the 'From' DID as E164 before republishing the inbound request
2. Format the From as NPAN on an offnet(outbound) request
3. Format the Diversion username to match the invite format of the request on an outbound request
4. Set the Outbound Caller ID Name to "Kazoo" on outbound calls

### SIP Headers

The formatting process will also walk any SIP Headers defined in the request. You need only include the actual header in the `formatters` object.

### Priority

The formatter object is processed in the following order:

1. Check whether to `strip` the key/value pair
2. Check whether to replace the value with the static text
3. Check whether to adjust the request to match a certain invite format
4. Check whether the value matches the regex
  a. if so, replace value with `prefix` + `matched` + `suffix`

The first step to process successfully ends that formatter's processing.

## The From URI

There are a number of parameters available to manipulate the SIP From header.

By default the From header will be set to the outbound caller id as the username and the FreeSWITCH sofia SIP interface IP as the realm. For example:
```
sip:+14158867900@10.26.0.38
```

There are three levels that the From header can be changed, in order of precedence (most preferred first):

* Gateway reformatting
* Resource reformatting
* System reformatting

These preferences are applied per-gateway.  For example, if an outbound request utilized three resources all three could modify the From realm utilizing each level of configuration.

### Gateway Reformatting

The parameters described in [resource reformatting](#resource-reformatting) can be set per-gateway as well.  If they are set at the resource level they apply to all gateways unless they are also set at the gateway level, in which case the gateway level parameter is used.

For a list of the parameters used in reformatting the From header see below.

The username component of the From header will utilize the outbound caller id number, after any resource formatters are applied.

```
sip:4158867900@my.carrier.com
```

### Resource Reformatting

When the resource is configured at the root level with `format_from_uri` set as `true`, the From header of the INVITE will be manipulated.  The realm will be determined based on the following parameters, in order of precedence (most preferred first):

* The string property `from_uri_realm` indicates that the value should be used directly as the domain in the From header.  For example,
```
sip:4158867900@my.static.realm
```
* The boolean property `from_account_realm` when `true` indicates that the accounts SIP realm (as reported, and possibly manipulated in another application on a per-account basis) should be used
```
sip:4158867900@kazoo.account.realm
```
* The gateway `realm` property will be used when neither `from_uri_realm` or `from_account_realm` but `format_from_uri` is `true`

The username component of the From header will utilize the outbound caller id number, after any resource formatters are applied.  If no formatters are present on the resource then the E.164 format of the caller id number will be used.

### System Reformatting

The system configuration document `stepswitch` contains the property `format_from_uri` which when set to `true` will set the the From header to the E.164 number and accounts SIP realm.

```
sip:+14158867900@kazoo.account.realm
```

### Caller-ID reformatting

The caller ID presented to the device can be altered by creating a formatter object for `caller_id_number`. If you just want to capture the 10-digit number (in the US) for instance, the value could be:

```json
"formatters":{
    "caller_id_number":{
        "regex":"(\\d{10})$"
    }
}
```

This would match caller id numbers that end with 10 digits and capture them as the value to be used.
