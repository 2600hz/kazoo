/*
Section: Stepswitch
Title: Formatters
Language: en-US
Version: 3.18
*/

# Formatting fields

When a call enters Kazoo from a carrier, Stepswitch receives the route request first (since no account information exists to identify the context of the call). Stepswitch will process this request to figure out what account the request is destined for and will replay the route request with the associated information.

If the call came from a known resource, you can optionally define formatters to take the route request headers and format the values before they are replayed to the call processing applications.

On the resource document, define a key `Formatters` which is a JSON object of route request headers as keys and their value being an object (or list of objects) of regexes, prefixes, and suffixes.

    {"_id":"some_resource_id"
     ,...
     ,"formatters":{
         "request":{...}
         ,"from":[{...},{...}]
         ,"caller-id-number":{...}
         ...
     }

In the above partial example, the resource has defined formatters for the `request` and `caller-id-number` fields, and a list of two formatters for the `from` field.

## Formatter format

Okay, so what goes in the `{...}` portions of the example above? The generic formatter looks like this:

    {"regex":"^som(e_r)egex$"
     ,"prefix":"some_prefix"
     ,"suffix":"some_suffix"
     ,"strip":boolean()
     ,"match_invite_format":boolean()
     ,"direction":direction()
    }

### Properties

* `direction`: Can be "inbound", "outbound", or unset. If unset, the formatter will be used on both inbound and outbound requests; otherwise the formatter will only be applied on the relevant request direction
* `strip`: If set to true, the field will be stripped from the request. Typically used to strip headers from a request before sending to the carrier (for instance, if the carrier doesn't support the Diversion header).
* `match_invite_format`: Applicable on fields with SIP URIs. Will format the username portion to match the invite format of the outbound request.
* `regex`: Matches against the value, with optional capture group
* `prefix`: Prepends value against the result of a successful regex match
* `suffix`: Appends value against the result of a successful regex match

So the regex will be run against the value of the header and if it matches, will take the capture group (or the whole value if no capture group is defined) and prepend the prefix and append the suffix (if applicable).

There is one caveat to that: when processing a regex for the `request`, `to`, or `from` fields, the value applied to the regex is the username portion, not the full `user@hostname` value. The prefix and suffix will be applied to the captured portion of the username, and the `@hostname` is appended as the last step.

A more full example:

    {"_id":"resource_id"
     ,"formatters":{
         "from":[
             {"regex":"^\\+?\\1?(\\d{10})$"
              ,"prefix":"+1"
              ,"direction":"inbound"
             }
             ,{"regex":"\\+?\\1(\\d{10})$"
               ,"direction":"outbound"
              }
          ]
          ,"diversion":[{
              "match_invite_format":true
              ,"direction":"outbound"
          }]
      }
      ,...
    }

This will format the 'From' DID as E164 before republishing the inbound request, formats the From as NPAN on an offnet(outbound) request, and format the Diversion username to match the invite format of the request on an outbound request.

### Custom Channel Vars and SIP Headers

The formatting process will also walk any Custom Channel Variables or SIP Headers defined in the request. You need only include the actual header in the `formatters` object.
