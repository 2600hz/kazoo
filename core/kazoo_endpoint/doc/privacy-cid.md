# Caller ID Privacy

## Overview

It's possible to restrict the presentation some or full part of Caller ID on calls. If a call (either inbound or outbound or local) has privacy flags, "anonymous" and "0000000000" would be shown as Caller ID Name and Caller ID Number respectively.

Kazoo has a default Feature Code `"*67"` which can be use for making a call with privacy flags set, please read documentation for `cf_privacy` to learn how to use and configure this feature.

The system config parameter `system_config/privacy/privacy_mode` can be use to configure which parts of Caller ID should be anonymize if privacy flags are set for a Call. This is especially is important for Outbound Calls, if your carrier is expecting known Caller IDs and they honor privacy flags when they're routing calls to third party carriers or final destination.

> WARNING: allowing to pass Caller IDs as is should only applied to carriers which you *TRUST* since it will expose caller's identity.

## Configuring Privacy System wide and/or in Account level

> Note: system config `privacy_mode` property is different from Callflow Privacy module's `privacy_mode`!

Possible `privacy_mode` values:

*`kazoo`*
:   anonymize both CIDName and CIDNumber

*`hide_name`*
:   anonymize CIDName only

*`hide_number`*
:   anonymize CIDNumber only

*`sip`*
:   not anonymize Caller ID, they would be passed as is

## Configuring Privacy on Resource

`privacy_mode` can be defined on resource root document and/or per gateway. Values are same as system wide config.

## Optionally check for anonymity in Caller ID Name and Caller ID Number

Some carrier systems, when using Caller ID Blocking, present the call using a non-standard format, instead of using flags to indicate privacy, some carriers set specific strings in the the Caller ID Name and Caller ID Number values. To ensure these calls are able to be blocked, the `anonymous_cid_names` and `anonymous_cid_numbers` configuration parameters allow for matching and blocking of these non-standard patterns.

This setting only impact how the `anonymous call rejection` feature is applied to inbound calls. The list of numbers or names in the `anonymous_cid_names` and `anonymous_cid_numbers` lists are compared to the `Caller-Id-Name` and `Caller-Id-Number` fields of the kazoo call. This enables the rejection of calls which use non-standard patterns, but must be used carefully as any call which matches these rules will be blocked.

`check_additional_anonymous_cid_names` enables enforcement of the configured `anonymous_cid_names` in anonymous call rejection.

`check_additional_anonymous_cid_numbers` enables enforcement of the configured `anonymous_cid_numbers` in anonymous call rejection.




