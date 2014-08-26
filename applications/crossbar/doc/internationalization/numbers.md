/*
Section: Internationalization
Title: Numbers
Language: en-US
*/

By default, Kazoo includes appropriate configurations for running the system in the United States. Nothing, however, stops folks from reconfiguring the system to support other country's numbering system.

2600Hz encourages you to consider sticking with the [E.164](https://en.wikipedia.org/wiki/E.164) format for globally routable numbers.

#Kazoo can also have dialplans on a per account/user/device basis now! More on that near the bottom



## Determine if a number is "global"

The first thing to configure is how to tell when a number is "globally routable" versus an internal extension. This is managed in the `system_config/number_manager` configuration document, under the `reconcile_regex` key.

    "reconcile_regex": "^\\+?1?\\d{10}$|^\\+[2-9]\\d{7,}$|^011\\d{5,}$|^00\\d{5,}$"

Here is the default, which if reading regexes isn't second nature, optionally matches a '+' and a '1' (the country code for the US), followed by any 10 digits, or matches 8-or-more digit numbers (prefixed by a '+'), or the international dialing codes for the US.

This regex must be able to match number formats your carrier(s) will send you. In the US, it is normal to see the 10-digit number (NPA-NXX-XXXX), optionally with a '1' prepended (NPANXXXXXX), or the full E.164 version (+1NPANXXXXXX). The default `reconcile_regex` matches all of those. Internal extensions, like 100, 2504, or *97, will obviously fail to be matched with the `reconcile_regex` and thus be routable only by authorized devices within an account.

### Country samples

#### [France +33](https://en.wikipedia.org/wiki/%2B33)

Calls within France are 10-digit numbers with a leading 0; from outside of France, only the last 9 digits (omitting the 0) are dialed after the '+33' country code. Armed with this knowledge, a regex might look like:

    "reconcile_regex":"^(?:\\+33\\d{9})|(?:0\\d{9})$"

Note: `(?:)` is a non-capturing regex group

This should match calls dialed within France (using the 0 followed by a 9 digit number) as well as calls coming from outside of France (+33 followed by a 9 digit number).

## Convertors

This a set of normalization regular expresions used on every number that Kazoo processes. The job of these expressions is to format numbers the same way regardless of where they originated. For example, most US carriers send numbers in E164 format (+14158867900) yet users do not dial +1. One use case is to ensure any US number begins with +1.

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

The first regex, `"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"` will capture the 10 digit number (ignoring a + or a 1 if present on the front of the dialed number), and adds a "+1" prefix to the captured number. So a dialed number of `4158867900`, `14158867900`, or `+14158867900` will all result in `+14158867900`. This would cover the main ways users and carriers will send numbers to Kazoo.

The second regex, `"^011(\\d{5,})$|^00(\\d{5,})$"`, matches how US customers would dial international numbers. `\\d{5,}` indicates there must be at least 5 digits following 011 or 00 (to allow people who want 001, 002, etc as extensions within accounts). The result is the number captured being prefixed by '+'. So if `01133123456789` was dialed, the second regex would match it, resulting in `+33123456789` being the number used for Kazoo's internal routing.

The third regex matches international numbers added to the system and prefixes them with a '+'. This can be further delineated (or removed) if you're not adding numbers to the system from multiple countries.

The final version of converted numbers becomes the format for the numbers databases (which controls how globally-routable numbers are assigned to accounts).

### Warning!!!

Change these carefully if you have an active system; when numbers are added to the datastore they are first normalized. If you change the these settings such that a number that used to be normalized in one way now results in a different format it will fail to route until it is resaved (causing it to be duplicated in the datastore in the new format).

### Country Samples

#### [France +33](https://en.wikipedia.org/wiki/%2B33)

Since within France one needs only dial the 10-digit number (0 + 9 digit subscriber number), the convertor regex will look simiarl to the `reconcile_regex`:

    "^0(\\d{9})$":{
        "prefix":"+33"
    },
    "^+33(\\d{9})$":{
        "prefix":"+33"
    }

Only capturing the 9-digit subscriber number, "+33" is prepended to form the E164-formatted version of the number. This checks either internally-dialed French numbers (the first regex) or externally-dialed French numbers (the second regex).

## Classifiers

This is a set of regexes to group numbers by type and are not used for routing. Classifiers are used to create groups of numbers that can be restricted, pretty print numbers in emails (like voicemail to email) and provide user friendly names in the UI.

    "classifiers":{
        "tollfree_us":{
            "regex":"^\\+1(800|888|877|866|855)\\d{7}$",
            "friendly_name":"US TollFree"
        },
        "toll_us":{
            "regex":"^\\+1900\\d{7}$",
            "friendly_name":"US Toll"
        },
        "emergency":{
            "regex":"^911$",
            "friendly_name":"Emergency Dispatcher"
        },
        "caribbean":{
            "regex":"^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7}$",
            "friendly_name":"Caribbean"
        },
        "did_us":{
            "regex":"^\\+?1?[2-9][0-9]{2}[2-9][0-9]{6}$",
            "friendly_name":"US DID",
            "pretty_print":"SS(###) ### - ####"
        },
        "international":{
            "regex":"^011\\d*$|^00\\d*$",
            "friendly_name":"International",
            "pretty_print":"SSS011*"
        },
        "unknown":{
            "regex":"^.*$",
            "friendly_name":"Unknown"
        }
    }

The key is the name of the group of numbers and is arbitrary. Within that sub-object, define a regex pattern that would classify a dialed number as a member of that group (Groups are evaluated in order, so the first group to match a number is the group associated).

Optionally define "friendly_name" which could be used for display purposes in a UI.

Optionally define "pretty_print", allowing the dialed number to be formatted in a more "readable" fashion.

The following characters can be used in a pretty print string to manipulate the number:

* # - Pound signs will be replaced by the number at the same position
* S - A capital 'S' will skip a number at the same position
* \* - An asterisk will add any remaining numbers from that position to the end of the number

If you want a literal '#', 'S', or '*', prefix it with a '\' (so '\#', '\S', and '\*')

`SS(###) ### - *` : this sample will convert numbers in the format of +14158867900 to (415) 886 - 7900



Dialplans on a per account/user/device basis

Since version (version here) Kazoo has better support for Internationalisation.
In perspective to this document we are speaking about the ability to dial as if u are local and conected to the PSTN.

Kazoo can be used with clients acounts from all over the globe, so we needed a way to deal with that.
We created dial patterns, so that u can set some rules on how outbound dialed numbers are manipulated so that it will work.
One can also set those kind of rules on many Devices such as SIP phones, we think it should be done on the platform.

The format is the same as the settings as above, but now on a account/user/device level, neat huh?





One can set dial paterns on
 Account level
 User level
 Device level
 
in the database of your install: accounts>users>device
Futon interface> accounts > list by username
or list by device
or list by whatever to find what u are looking for.

 
The order in which we handle them is Device -> User -> Account.
----------------------------------------------------------------

Example
----------------------------------------------------------------

"dial_plan" : {
   "^(\\d{9})$": {
       "description": "Portugal",
       "prefix": "+351"
   },
   "^(\\d{10})$": {
       "description": "USA",
       "prefix": "+1"
   },
   "^0(\\d{9,})$": {
       "description": "UK",
       "prefix": "+44"
   }
}
----------------------------------------------------------------

As said u can set this on three levels: Device -> User -> Account
This order is also the order in which we deal with this function.
Please note that its one or the other 	(is this a correct assumption????)

So... if u have an account with people from the same village that require the same set of dialing rules, u could set them on the acount level.
If u have an account that has people from all over the country, u would need to set them on the user or Device level, i recommend to use the User level.

If u want to specify it on a per Device basis, thats where u set it.

At this time its unclear to me what happens if u set them in all documents, someone might edit this post to fix that though.
What happens if u set different rules in the three docs is also unknown to me, i guess that the last one (account) would be used.

One might be able to create really great regexes that would catch all for a region, country or even better!
If u did, please share them!!! U can fork this repo via git and commit the changes, 
if thats too much u can email them to info@yumminova.eu and ill put them here with a big thumbs up.

Please dont forget to issue 
sup callflow_maintenance flush
sup crossbar_maintenance flush
sup whistle_couch_maintenance flush

command when done 

Curtains close

//ToDo
Explain where to find those database documents. Its very unclear as an acount has so many (ugly named) documents, its hard to tell what is what
Rumour has it that this is something being worked on.


