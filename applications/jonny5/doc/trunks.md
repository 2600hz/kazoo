/*
Section: Trunks
Title: Trunks AKA flat rate or channels
Language: en-US
*/

# Overview

Trunks are derived from the PSTN idea of a physical phone line. You can only
have one call active for each trunk (phone line) you own.
There are three main concepts to a trunk.
1. Only one call can be active on any one trunk at a time.
2. Customer is charged a flat fee for the trunk no matter how much of the day
   that trunk is used.
3. A trunk is defined to only serve a defined set of destinations. Like only
   USA destinations.

# Trunk Types

There are three basic types of trunks. Inbound only, outbound only,
and two way. The reason for having different types is for billing purposes.
Inbound calls are generally less expensive. This provides flexibility to better
meet a customers needs yet keep things similar to what they are used to.

# History

Previously the trunk destinations were defined by a pair of regular expressions
named flat_rate_whitelist and flat_rate_blacklist in the jonny5 doc in the
system_config DB. They are still used by default for backward compatibility but
now there is more flexibility in defining what calls are eligible to use the
trunks. The old lists had several shortcomings.
1. Inbound and Outbound use the same white/black lists. This creates a problem
   for excluding toll free from inbound calls so they get charged on a per
   minute basis while outbound
2. All customers can only use the one system defined trunk white/black lists.
   Works fine as long as all your customers are in the same basic area and have
   the same basic needs. Not so good if you have customers in US, Europe, and
   Asia.
3. A customer can not have trunks defined in multiple area. (not yet addressed)

# Trunk Definition

Jonny5 now supports multiple trunk definitions that one can be selected by
setting "pvt_trunk_region_id" in the limits doc of the customers account DB.
The trunk definition it self has several was to define destinations that apply.
1.  There are separate white/black lists for inbound and outbound.
2.  Support for white and black classification matching lists.
3.  Support to include other trunk definitions. This makes it easy for combining
    regions you already defined into a more general trunk.

The basic idea with this functionality is to keep the regex from getting super
complex and make the functionality relatively simple to implement with a UI.

As mentioned in history about shortcomings, inbound and outbound do not share
definitions except the list of other definitions to include.

# Classification Matching

The called number was classified prior to the start of the trunk process and is
part of the request record passed in. The classification white/black list if
present is an array of classification IDs defined in system_config/numbers.
Keep in mind that classification matching is done in order of appearance in the
numbers doc and first one matched is what the call is classified as. Same holds
true for restrictions and DID billing. Take that into consideration here as well.
Restrictions differ from the black list in that they completely prevent an
outbound call irregardless of available trunks, allotments or per minute options.

# Including Other Trunk Definitions

With customers around the globe users have different local calling areas. Some
may want their trunks to also include other areas that they commonly call. It
is much cleaner to define a trunk around a single area like a country the deals
with it's special circumstances for things like mobile, toll free, etc. Then
just include them. For example you might want an outbound trunk for Western
Europe excluding higher cost mobile and toll free (since a free call should not
consume a long distance trunk). Each country has it's own way of defining mobile
and toll free numbers. Writing one white and one black regex that can handle all
would be daunting. Instead for each conutry and type in write separate
definitions. One that defines the country code, one that defines mobile,
one that defines toll free and one that ties it all together like
"germany_landlines": {
    "include_defs" : [
        "germany"
    ],
    "exclude_defs" : [
        "germany_mobile",
        "germany_toll_free"
    ],
    "name": "Germany Land Lines",
    "description": "Germany numbers excluding mobile and toll free."
}
Then tie the countries together in the same way like
"western_euro_landlines": {
    "include_defs": [
        "germany_landlines",
        "france_landlines",
        "spain_landlines",
        ...
    ],
    "name": "Western Europe Land Lines",
    ...
}
Note: That is not the most simple way to define it but it demonstrates the
flexibility.
The more simple way is to use classification match lists. Since the request was
classified before this process and classifying runs through in order picking the
first match it finds, if mobile and toll free classes are defined and smartly
are before the general country prefix the same thing is accomplished in the
spain_landlines as the entire german_landline grouping def shown in the example
section below.

# Billing

Currently billing only sees the three basic types of trunks as billable items.
So for now you will need to use a different service plan for different regions
or put overrides in the customers service doc.
Logically the next step is to put the trunk definition id as part of the trunk
type.
Example: western_euro_landlines_twoway_trunks

# Order of Processing

For backward compatibility...
If pvt_trunk_region_id is not defined in the customer's limits doc, the old
system defined white/black lists are used for both inbound and outbound. They
are defined in flat_rate_blacklist and flat_rate_whitelist of
system_config/jonny5. It does also support separate white/black lists with:
flat_rate_inbound_whitelist, flat_rate_inbound_blacklist,
flat_rate_outbound_whitelist, flat_rate_outbound_blacklist

New method. For simplicity of the setting names we will assume an outbound call.
1.  If we have recursed to deep, reject current trunk definition.
    Use dialed_region_max_recurse_depth to override the default of 20. There is
    no limit to the width of the inclusion tree.
2.  The trunk definition is loaded.
Check if we should reject. Any reject will end the process returning false.
3.  If a loop is detected a loop warning is logged and that definition is
    skipped/rejected.
4.  If outbound_black_num_classifications exists and the request classification
    matches an entry in it, that definition is rejected.
5.  If exclude_defs exists, they will be recursed and if the result from it is
    true, then that definition is rejected returning false.
Check if we should accept. If we were not already rejected :-), any accept will
end the process returning true.
6.  If outbound_white_num_classifications exists and the request classification
    matches an entry in it, that definition is accepted returning true.
7.  If outbound_whitelist does not exist, is empty, or it's regex matches then it
    the outbound_blacklist will be checked. If the black list does not exist, is
    empty, or it's regex does not match the white/black list test will accept
    returning true.
8.  If include_defs exists, they will be recursed and if the result from it is
    true, then that definition is accepted returning true.
9.  If there are other trunks that were defined after the current definition in
    a list of definitions to include/exclude, try the next one in the list.
It recurses the include/exclude tree in deep then wide method.
A final result of true means the call is eligible to use a trunk if they are not
already all in use.
10. If the call is ineligible for a trunk the process for using a trunk is ended
    returning the request record NOT adding indication it is using a trunk.
11. If not all the purchased trunks are used up, it will claim one and indicate
    that in the request record which it will then return.

WARNING: With exclude_def, blacklists and black classifications it can be easy
to set your self up with a double negative. TEST!!!

# Example system_config/jonny5 Doc

Unlike classifying a number order of values does not matter.
It includes partial examples of a Western Europe trunk definition and one that
was used to test functionality like loop detection.

```
{
   "_id": "jonny5",
   "_rev": "13-2c47af0d441f453b836c0422e6479ca4",
   "default": {
       "default_enabled": true, // is jonny5 enabled
       "inbound_user_field": "Request", // Which part of the SIP INVITE to use for the dialed number, the Request URI or the To header URI.
       "assume_inbound_e164": false, // Whether to run e164 conversion on the inbound number.
       "default_allow_prepay": true, // Allow calls to use system local account balance to pay for per minute calls.
       "default_allow_postpay": false, // Allow the system local account balance to go negative.
       "default_max_postpay_amount": 0, // How much postpay credit is allowed.
       "default_reserve_amount": 0.5, // How much credit must be available to allow a per minute call to begin
       "default_twoway_trunks": -1, // Number of trunks that are allowed to handle inbound and/or outbound calls.
       "default_inbound_trunks": -1, // Number of trunks available only for inbound calls. If there any available it will be used before twoway trunks.
       "default_outbound_trunks": 0, // Number of trunks available only for outbound calls. If there any available it will be used before twoway trunks.
       "default_resource_consuming_calls": -1, // Max number of calls that can consume one or more carriers/resources (IE: inbound to outbound forwarded number is one resource consuming call)
       "default_calls": -1, // Total number of simultaneous calls including extension to extension and checking voicemail.
       "default_burst_trunks": 0,
       "default_soft_limit_inbound": false, // Do NOT enforce inbound limits
       "default_soft_limit_outbound": false, // Do NOT enforce outbound limits
       "flat_rate_whitelist": "^\\+?1\\d{10}$", // Old white list, used if no newer definitions are set.
       "flat_rate_blacklist": "^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900)\\d{7}$", // old black list, used if no new definitions are set.
       "flat_rate_inbound_whitelist": ".*", // Separate inbound from outbound white list. Not used if pvt_trunk_region_id is set in account's limits doc.
       "flat_rate_inbound_blacklist": "^\\+?(1(800|888|877|866|855|844)|(611|3531)(800)|9721801)", // Separate inbound from outbound black list. Not used if pvt_trunk_region_id is set in account's limits doc.
       "flat_rate_outbound_whitelist": "^\\+?1\\d{10}$", // Overrides/replaces/interchangeable with flat_rate_whitelist. Optional for consistency with new inbound type.
       "flat_rate_outbound_blacklist": "^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900)\\d{7}$", // Overrides/replaces/interchangeable with flat_rate_blacklist. Optional for consistency with new inbound type.
       "default_trunk_eligible_on_regex_error": false, // allow a trunk to be used even if a regex is bad. Dangerous, use a regex checker!!
       "dialed_region_max_recurse_depth": 20, // to help keep you from accidentally writing very deep recursive includes
       "dialed_region_definitions": { // flexible trunk definitions
           "default": { // "default" is defined by super duper admin. Defs available to all limits docs. Future support for reseller definitions.
               "western_euro_landlines": { // id of a trunk definition, what would be set in the account's limits doc like "pvt_trunk_region_id": "western_euro_landlines"
                   "include_defs": [ // trunk defs that extend the definition of this trunk def.
                       "germany_landlines",
                       "france_landlines",
                       "spain_landlines"
                   ],
                   "name": "Western Europe Land Lines" // short name for future UI option display
               },
               "germany_landlines": {
                   "include_defs": [
                       "germany"
                   ],
                   "exclude_defs": [
                       "germany_mobile",
                       "germany_toll_free"
                   ],
                   "name": "Germany Land Lines",
                   "description": "Germany numbers excluding mobile and toll free." // Description for future UI hover text.
               },
               "germany": {
                   "name": "All German Numbers",
                   "description": "All Germany numbers including mobile and toll free.",
                   "outbound_whitelist": "^49"
               },
               "germany_mobile": {
                   "name": "German Mobile Numbers",
                   "outbound_whitelist": "^4901[567]"
               },
               "germany_toll_free": {
                   "name": "German Toll Free",
                   "outbound_whitelist": "^490800",
                   "inbound_blacklist": "^490800"
               },
               "spain_landlines": {
                   "name": "Spain Land Lines",
                   "outbound_white_num_classifications": [ // trunk eligible if the request's classification is the same as any in this list.
                       "did_es"
                   ]
               },
               "france_landlines": {
                   "name": "French Land Lines",
                   "outbound_white_num_classifications": [
                       "did_fr"
                   ]
               },
               "test_trunk_def": {
                   "name": "Test 1",
                   "description": "Bla bla",
                   "include_defs": "test_2_trunk_def",
                   "inbound_black_num_classifications": [
                       "tollfree_ve"
                   ],
                   "inbound_white_num_classifications": [
                       "toll_us"
                   ],
                   "inbound_whitelist": "",
                   "inbound_blacklist": "^\\+?(1(800|888|877|866|855|844)|(611|3531)(800)|9721801)",
                   "outbound_whitelist": "^(44|33|82)"
               },
               "test_2_trunk_def": {
                   "name": "Test 2",
                   "description": "Bla bla bla",
                   "include_defs": [
                       "test_trunk_def",
                       "test_3_trunk_def"
                   ],
                   "inbound_black_num_classifications": [
                       "tollfree_ve",
                       "tollfree_us"
                   ],
                   "inbound_white_num_classifications": [
                       "toll_us"
                   ],
                   "outbound_whitelist": "^(44|33|82)",
                   "outbound_blacklist": "^(44207|1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|707)\\d{7}$)"
               },
               "test_3_trunk_def": {
                   "name": "Test 3",
                   "description": "Bla bla bla bla",
                   "outbound_white_num_classifications": [
                       "toll_us",
                       "did_us"
                   ],
                   "inbound_whitelist": "",
                   "inbound_blacklist": "^\\+?(1(800|888|877|866|855|844)|(611|3531)(800)|9721801)",
                   "outbound_whitelist": "",
                   "outbound_blacklist": "^(44207|1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340|900|707)\\d{7}$)"
               }
           }
       }
   }
}
```
