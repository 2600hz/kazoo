# International Phone Numbers

By default, Kazoo includes appropriate configurations for running the system in the United States. Nothing, however, stops folks from re-configuring the system to support other country's numbering system.

2600Hz encourages you to consider sticking with the [E.164](https://en.wikipedia.org/wiki/E.164) format for globally rout-able numbers.

## Determine if a number is "global"

The first thing to configure is how to tell when a number is "globally rout able" versus an internal extension. This is managed in the `system_config/number_manager` configuration document, under the `reconcile_regex` key.

```
"reconcile_regex": "^\\+?1?\\d{10}$|^\\+[2-9]\\d{7,}$|^011\\d{5,}$|^00\\d{5,}$"
```

Here is the default, which if reading regexps isn't second nature, optionally matches a `+` and a `1` (the country code for the US), followed by any 10 digits, or matches 8-or-more digit numbers (prefixed by a `+`), or the international dialing codes for the US.

This regex must be able to match number formats your carrier(s) will send you. In the US, it is normal to see the 10-digit number (NPA-NXX-XXXX), optionally with a `1` prepended (NPANXXXXXX), or the full E.164 version (+1NPANXXXXXX). The default `reconcile_regex` matches all of those. Internal extensions, like 100, 2504, or `*97`, will obviously fail to be matched with the `reconcile_regex` and thus be rout-able only by authorized devices within an account.

## Country samples

### [France +33](https://en.wikipedia.org/wiki/%2B33)

Calls within France are 10-digit numbers with a leading 0; from outside of France, only the last 9 digits (omitting the 0) are dialed after the `+33` country code. Armed with this knowledge, a regex might look like:

    "reconcile_regex":"^(?:\\+33\\d{9})|(?:0\\d{9})$"

Note: `(?:)` is a non-capturing regex group

This should match calls dialed within France (using the 0 followed by a 9 digit number) as well as calls coming from outside of France (+33 followed by a 9 digit number).

## Converters

This a set of normalization regular expressions used on every number that Kazoo processes. The job of these expressions is to format numbers the same way regardless of where they originated. For example, most US carriers send numbers in E164 format (+14158867900) yet users do not dial +1. One use case is to ensure any US number begins with +1.

```json
{
    "e164_converters":{
        "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$":{
            "prefix":"+1"
        },
        "^011(\\d{5,})$|^00(\\d{5,})$":{
            "prefix":"+"
        },
        "^[2-9]\\d{7,}$": {
            "prefix": "+"
        }
    }
}
```

The first regex, `"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"` will capture the 10 digit number (ignoring a + or a 1 if present on the front of the dialed number), and adds a "+1" prefix to the captured number. So a dialed number of `4158867900`, `14158867900`, or `+14158867900` will all result in `+14158867900`. This would cover the main ways users and carriers will send numbers to Kazoo.

The second regex, `"^011(\\d{5,})$|^00(\\d{5,})$"`, matches how US customers would dial international numbers. `\\d{5,}` indicates there must be at least 5 digits following 011 or 00 (to allow people who want 001, 002, etc as extensions within accounts). The result is the number captured being prefixed by '+'. So if `01133123456789` was dialed, the second regex would match it, resulting in `+33123456789` being the number used for Kazoo's internal routing.

The third regex matches international numbers added to the system and prefixes them with a '+'. This can be further delineated (or removed) if you're not adding numbers to the system from multiple countries.

The final version of converted numbers becomes the format for the numbers databases (which controls how globally rout-able numbers are assigned to accounts).

!!! warning
    Change these carefully if you have an active system; when numbers are added to the data store they are first normalized. If you change the these settings such that a number that used to be normalized in one way now results in a different format it will fail to route until it is re saved (causing it to be duplicated in the data store in the new format).

## Country Samples

### [France +33](https://en.wikipedia.org/wiki/%2B33)

Since within France one needs only dial the 10-digit number (0 + 9 digit subscriber number), the converter regex will look similar to the `reconcile_regex`:

```json
{
    "^0(\\d{9})$":{
        "prefix":"+33"
    },
    "^+33(\\d{9})$":{
        "prefix":"+33"
    }
}
```

Only capturing the 9-digit subscriber number, "+33" is prepended to form the E164-formatted version of the number. This checks either internally-dialed French numbers (the first regex) or externally-dialed French numbers (the second regex).

### Examples

See [some examples](examples/number_manager/european_union.md) for user-contributed samples (and create pull requests of your own!).

## Classifiers

This is a set of regexps to group numbers by type and are not used for routing. Classifiers are used to create groups of numbers that can be restricted, pretty print numbers in emails (like voicemail to email) and provide user friendly names in the UI.

```json
{
    "classifiers":{
        "tollfree_us":{
            "regex":"^\\+1((?:800|888|877|866|855)\\d{7})$",
            "friendly_name":"US TollFree"
        },
        "toll_us":{
            "regex":"^\\+1(900\\d{7})$",
            "friendly_name":"US Toll"
        },
        "emergency":{
            "regex":"^(911)$",
            "friendly_name":"Emergency Dispatcher"
        },
        "caribbean":{
            "regex":"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})$",
            "friendly_name":"Caribbean"
        },
        "did_us":{
            "regex":"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$",
            "friendly_name":"US DID",
            "pretty_print":"SS(###) ##### - ####"
        },
        "international":{
            "regex":"^(011\\d*)$|^(00\\d*)$",
            "friendly_name":"International",
            "pretty_print":"SSS011*"
        },
        "unknown":{
            "regex":"^(.*)$",
            "friendly_name":"Unknown"
        }
    }
}
```

The key is the name of the group of numbers and is arbitrary. Within that sub-object, define a regex pattern that would classify a dialed number as a member of that group (Groups are evaluated in order, so the first group to match a number is the group associated).

Optionally define `friendly_name` which could be used for display purposes in a UI.

Optionally define `pretty_print`, allowing the dialed number to be formatted in a more "readable" fashion.

The following characters can be used in a pretty print string to manipulate the number:

* `###` - Pound signs will be replaced by the number at the same position
* `S` - A capital 'S' will skip a number at the same position
* `*` - An asterisk will add any remaining numbers from that position to the end of the number

If you want a literal '#', 'S', or '*', prefix it with a '\' (so '\#', '\S', and '\*')

`SS(###) ##### - *` : this sample will convert numbers in the format of +14158867900 to (415) 886 - 7900

### Per-Account dial plans

Users can dial local numbers, just as they do with the PSTN, by providing Kazoo with `dial_plan` regular expressions. These regexps will be used on the dialed numbers to correct them to properly routable numbers.

It is possible to set these regexps on an account, user, or device basis. All that needs doing is adding a `dial_plan` key at the root level of the account, user, or device document. Kazoo will then apply the regexps in order, preferring the calling device's, then user's (if the calling device has an `owner_id` set), and finally the account's dialplan. Failing any of those, the system `e164_converters` will be employed.

!!! warning
    It is possible that these `dial_plan` rules will interfere with extension dialing within an account. Please take common extension length into consideration when creating these `dial_plan` rules.

See [the examples](./examples/dialplan/IE_Local-National-Mobile.md) for user-contributed samples (and create pull requests of your own!).

#### Example `dial_plan` object

```json
{
    "dial_plan" : {
        "^(\\d{9})$": {
            "description": "Portugal",
            "prefix": "+351"
        }
        ,"^(\\d{10})$": {
            "description": "USA",
            "prefix": "+1"
        }
        ,"^(\\d{7})$":{
            "description": "USA/CA/SF",
            "prefix": "+1415"
        },
        "^0(\\d{9,})$": {
            "description": "UK",
            "prefix": "+44"
        }
    }
}
```

The `dial_plan` key is a regex to match against the dialed number, with `prefix` and `suffix` rules to prepend and append to the capture group in the regex. Regexps are evaluated in order and the first regex to match is the one used.

## Scenarios

### One locale for all devices in an account

If all of the users/devices in an account are located in the same city, it would be most convenient to place a `dial_plan` at the account level, allowing them to dial as they are used to and converting it for Kazoo processing. For instance, we can poach the "USA/CA/SF" regex from above for an account who's users are all in San Francisco. Then, when a user dials a 7-digit number, it is prepended with the 415 area code (as well as +1).

### Globally distributed users

Users within an account may be located anywhere in the world. An account-level `dial_plan` may not make sense for them. Instead, place `dial_plan` objects on the users' documents to ensure their local dialing preferences are honored.

### Adding `dial_plan` example

Using the PATCH HTTP verb, you can add the `dial_plan` object to an existing document:

    curl -X PATCH -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID} -d '{"data":{"dial_plan":{"^(\\d7)$":{"prefix":"+1415","description":"USA/CA/SF"}}}}'

You can, of course, POST the full document with the added `dial_plan` object.

### System dial plans

It is possible to add dial plans to system config. Account/user/device `dial_plan` can refer to it adding array of system dial plan names at key `system`.

### Adding system `dialplan` example

Create dialplans doc in case it is still absent in system_config db:

````
curl -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/system_configs -d '{"data":{"id":"dialplans"}}'
````

Then create your dialplan:

````
curl -X POST -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/system_configs/dialplans -d '{"data":{"^(2\\d{6})$":{"prefix":"+7383","name":"Novosibirsk"}}}'
````

or dialplans:

````
curl -X POST -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/system_configs/dialplans -d '{"data":{"^(\\d{7})$":[{"prefix":"+7495","name":"Moscow"},{"prefix":"+7812","name":"Saint Petersburg"}]}}'
````

### Using system `dialplan` example

```shell
curl -X PATCH \
   -H "Content-Type: application/json" \
   -H "X-Auth-Token: {AUTH_TOKEN}" \
   http://server.com:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID} -d '{"data":{"dial_plan":{"system":["Novosibirsk"]}}}'
```

## Available system dial plans

All users can view available system dial plans.

```shell
    curl -X GET \
       -H "Content-Type: application/json" \
       -H "X-Auth-Token": {AUTH_TOKEN}" \
       http://server.com:8000/v2/dialplans
```

## Caches to flush

Changes made via Crossbar *should* flush the appropriate caches automatically. If you make changes to the database directly, or aren't seeing your changes via Crossbar reflected, the following `sup` commands should flush the appropriate caches.

Execute on VMs running:

* Crossbar
    * `sup kazoo_couch_maintenance flush [{ACCOUNT_ID} [{DOCUMENT_ID}]]`
* Callflow
    * `sup callflow_maintenance flush`

If you make a change to `system_config`, execute `sup kapps_config flush [{CONFIG_DOC}]`
