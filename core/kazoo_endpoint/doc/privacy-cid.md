### Caller ID Privacy

#### Overview

It's possible to restrict the presentation some or full part of Caller ID on calls. If a call (either inbound or outbound or local) has privacy flags, "anonymous" and "0000000000" would be shown as Caller ID Name and Caller ID Number respectively.

Kazoo has a default Feature Code `"*67"` which can be use for making a call with privacy flags set, please read documentation for `cf_privacy` to learn how to use and configure this feature.

The system config parameter `system_config/privacy/privacy_mode` can be use to configure which parts of Caller ID should be anonymize if privacy flags are set for a Call. This is especially is important for Outbound Calls, if your carrier is expecting known Caller IDs and they honor privacy flags when they're routing calls to third party carriers or final destination.

> WARNING: allowing to pass Caller IDs as is should only applied to carriers which you *TRUST* since it will expose caller's identity.

#### Configuring Privacy System wide and/or in Account level

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

#### Configuring Privacy on Resource

`privacy_mode` can be defined on resource root document and/or per gateway. Values are same as system wide config.

#### Custom patterns for Caller ID Name and Caller ID Number

*`anonymous_cids`*
:   Some carriers when using Caller ID Blocking present the call using non-standard format for the Caller ID Name and Caller ID Number values. To ensure these are able to be blocked, the `anonymous_cids` configuration parameter was added to allow matching and blocking of these non-standard patterns. This setting only impact the `anonymous call rejection` feature applied to inbound calls, settings defined in this list extend the default anonymous call rejection feature to trigger on a list of numbers and names instead of merely `anonymous` Caller ID  Name and `0000000000` for Caller ID Number. Default values are `anonymous`, `0000000000`, `Anonymous`, `Restrcted`, and `restricted`, which are the most commonly used patterns for indicating a call with Caller ID blocking configured. This list can be extended to include more patterns as required. All the patterns in this list are compared to both the Caller ID Name and the Caller ID Number when determining if a call is anonymous.
