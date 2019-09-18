# Callflow Dynamic CID

## About Dynamic CID

The Dynamic CID callflow enables you to dynamically change the Caller ID (CID). There are different methods for doing that:

* **manual:** dial the new Caller ID on the keypad when prompted, this way you can set the Caller ID number only. This works with and without a capture group
* **list:** setting Caller ID name and number based on the configuration on the specified document in the database. This requires a capture group
* **lists:** almost the same as list mode but you can use account's lists feature to configure entries
* **static** set the caller id to a value in a `caller_id.name` and `caller_id.number` property in the data object. This works with and without a capture group

## Callflow fields

#### Schema

Validator for the dynamic_cid callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform | `string('list' | 'lists' | 'manual' | 'static')` |   | `false` |  
`caller_id.name` | Caller ID Name | `string()` |   | `false` |  
`caller_id.number` | Caller ID Number | `string()` |   | `false` |  
`caller_id` | Static Caller ID settings | `object()` |   | `false` |  
`enforce_call_restriction` | Check classification restrictions against endpoint | `boolean()` | `true` | `false` |  
`id` | List ID for caller IDs when 'action' is 'list' | `string()` |   | `false` |  
`idx_name` | Named capture group to use | `string()` |   | `false` |  
`interdigit_timeout` | How long, in seconds, to wait for keypresses | `integer()` |   | `false` |  
`max_digits` | Max number of digits allowed when collecting Caller ID Number | `integer()` |   | `false` |  
`media_id` | Prompt to play to caller to enter Caller ID Number | `string()` |   | `false` |  
`min_digits` | Minimum number of digits that must match the regex to collected DTMF | `integer()` |   | `false` |  
`permit_custom_callflow` | Permit a custom callflow to be accepted instead of restricting to no_match only when looking up callflows | `boolean()` | `false` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`whitelist_regex` | Regex to match collected Caller ID Number | `string()` |   | `false` |  






## Manual action mode

In this method user is prompted to dial a new Caller ID number. The length of the dialed new Caller ID is checked to be in the default(or configured) boundary and it is also matched against the default(or configured) regex, if these checks are passed the call will proceed.

> **Notes** You can only set the Caller ID number with this method.

### Example Scenario

User dial `*212223332222` (assuming you set the feature code to `*2` by setting callflow patterns to `"^\\*2([0-9]{2,})$"`) and
prompted to dial the new Caller ID number, then the call will send onto real destination which in this case is `12223332222`.

### Manual callflow settings

You may want to customize `manual` behavior in `system_configs/callflow.dynamic_cid`.

#### Schema

Schema for callflow.dynamic_cid system_config



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`accept_prompt` | callflow.dynamic_cid accept prompt | `string()` | `tone_stream://%(250,50,440)` | `false` |
`default_prompt` | callflow.dynamic_cid default prompt | `string()` | `dynamic-cid-enter_cid` | `false` |
`max_digits` | callflow.dynamic_cid max digits | `integer()` | `10` | `false` |
`min_digits` | callflow.dynamic_cid min digits | `integer()` | `10` | `false` |
`reject_prompt` | callflow.dynamic_cid reject prompt | `string()` | `dynamic-cid-invalid_using_default` | `false` |
`whitelist_regex` | callflow.dynamic_cid whitelist regex | `string()` | `\d+` | `false` |



## List action mode

In this method you have flexibility to define multiple Caller ID, each assign to a specific number using account's list. You can set both the Caller ID name and
number in a document in database. This callflow's module use that information to set new Caller ID. You can create your list and list's entries using [List Crossbar API](../../crossbar/doc/lists.md).

This done by first getting group capture which populate by callflow automatically (feature code is removed from the beginning) then get a specific length of numbers starting from the beginning of the dialed number (by default 2 numbers). This number is it the key (or index) in a list of Caller ID entry list. For and example how this works, see the example section.

This behavior is exactly same as `lists` action, the only difference is that you can define the index of the matching number (entry index) in the callflow data in `idx_name`.

> **Notes** You can set  both the Caller ID number and name with this method.

### List Entry document fields

#### Schema

Schema for a match list entries



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`capture_group_key` | The number which is used to select this entry, length should be same as `capture_group_length` | `string()` |   | `false` |
`capture_group_length` | Length of the numbers in the list entries for which it used to select desire Caller ID | `integer()` |   | `false` |
`displayname` | Display name | `string(1..128)` |   | `false` |
`firstname` | A friendly firstname | `string(1..128)` |   | `false` |
`lastname` | A friendly lastname | `string(1..128)` |   | `false` |
`list_id` | List id | `string()` |   | `true` |
`number` | Phone number | `string()` |   | `false` |
`pattern` | Match pattern | `string()` |   | `false` |
`profile` | Profile data | `object()` | `{}` | `false` |
`type` |   | `string(1..128)` |   | `false` |

#### profile

Defines user extended properties


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`addresses.[].address` | To specify the address | `string()` |   | `false` |
`addresses.[].types` | To specify types of the address | `array()` |   | `false` |
`addresses` | To specify the components of the addresses | `array(object())` |   | `false` |
`assistant` | To specify the user's assistant | `string()` |   | `false` |
`birthday` | To specify the birth date of the user | `string()` |   | `false` |
`nicknames.[]` |   | `string()` |   | `false` |
`nicknames` | To specify the text corresponding to the nickname of the user | `array(string())` |   | `false` |
`note` | To specify supplemental information or a comment that is associated with the user | `string()` |   | `false` |
`role` | To specify the function or part played in a particular situation by the user | `string()` |   | `false` |
`sort-string` | To specify the family name or given name text to be used for national-language-specific sorting of the FN and N types | `string()` |   | `false` |
`title` | To specify the position or job of the user | `string()` |   | `false` |



### Example 1: Two digits as CID selector

Consider that the user wants to call number `5149072508` using the caller id `16139999999`. We assume that callflow patterns set as follow:

```json
    "patterns": [
        "^\\*2([0-9]{2,})$"
    ]
```

and the list entry document is:

```json
{
    "capture_group_length" : 2,
    "capture_group_key": "01",
    "number": "19058888888",
    "name": "Awesome Inc."
}
```

These are the steps will happen if a user dial `*2015149072508` in handset:

1. This pattern above means that `*2` is the "feature code". Callflow will use this module to handle the call
2. `01` will be used to select the Caller ID with number "19058888888" and name "Awesome Inc.". Consider that it's **length** is 2 digits as specified in `capture_group_length` field
3. Selected CID will be set for the call
3. `5149072508` becomes `+15149072508` and gets dialed as such

### Example 2: One digits as CID selector

Another example which uses number with length of 1 to select the Caller ID:

```json
{
    "capture_group_length" : 1,
    "capture_group_key": "0",
    "number": "16139999999",
    "name": "Awesome Co."
}
```

Dialing number `*205149072508` will cause number "5149072508" gets dialed with CID number set to "16139999999" CID name to "Awesome Co.".

## Lists action mode

Almost the same as [list mode](#list-action-mode) using account's lists feature to configure entries.

## Static action mode

In this method the new Caller ID number and name would be set to `caller_id` value sets in the callflow Data object.

### Example Callflow Data

```json
{
    "data": {
        "action": "static",
        "caller_id": {
            "name": "CALL_ME",
            "number": "5555555555"
        }
    },
    "module": "dynamic_cid"
}
```

## Permit Custom Callflow

The config parameter `permit_custom_callflow` is used in the `lists` action type case. When attempting to fetch the callflow, the default behavior is to reject any callflow returned which is not the `no_match` callflow. If a callflow is defined which matches the resultant destination number, this setting allows that callflow to be used, bypassing the `no_match` restriction.
