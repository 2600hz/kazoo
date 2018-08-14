
# Rules fields

Example resource document:

    {
       "_id": "some_resource_id",
       "_rev": "2-9cde8aaf05b398fa8eb463fc5a943ca7",
       "name": "Resource 1",
       "enabled": true,
       "flags": [
       ],
       "weight_cost": 30,
       "rules": [
           "^\\+7(\\d{10})$"
       ],
       "cid_rules": [
           "^(\\+749[59]\\d{7})$"
       ],
       "gateways": [
           {
                ...
           }
       ],
       "pvt_type": "resource"
    }

## Dialed number rules ("rules" field)
A list of regular expressions for matching E.164-formatted (+12223334444) DIDs. A sample regex to match all Russian E.164 numbers:

    "^\\+7(\\d{10})$"

You can obviously add regexps for specific area codes, toll-free, E911, and international numbers. The first capture group is what is used to pass in the bridge URI (in the example, the 10-digit number will be passed to the gateways).

## CallerID number rules ("cid_rules" field)
Like "rules" field, but capture groups don't modify outgoing CallerID number. If you want modify CallerID number - use "formatters".

