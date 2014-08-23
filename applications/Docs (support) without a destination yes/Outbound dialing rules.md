/*
Section: Unknown
Title: Outbound dialing rules
Language: en-US
*/
// Please note that My English is rather crappy and i dont use a spell checker, feel free to adjust, but dont comment on any errors ))

Since version (version here) Kazoo has better support for Internationalisation.
In perspective to this document we are speaking about the ability to dial as if u are local.
Kazoo can be used with clients acounts from all over the globe, so we needed a way to deal with that.
We created dial patterns, so that u can set some rules on how outbound dialed numbers are manipulated so that it will work.
One can also set those kind of rules on many Devices such as SIP phones, we think it should be done on the platform.

One can set dial paterns on
 Account level
 User level
 Device level
 
The order in which we handle them is Device -> User -> Account.
----------------------------------------------------------------

Example
----------------------------------------------------------------

dial_plan : {
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

Curtains close



