# Callflow Dynamic CID

## About Dynamic CID

The Dynamic CID callflow enables you to dynamically change the Caller ID (CID). There are different methods for doing that:

* **manual:** dial the new Caller ID on the keypad when prompted, this way you can set the Caller ID number only. This works with and without a capture group
* **list:** setting Caller ID name and number based on the configuration on the specified document in the database. This requires a capture group
* **lists:** almost the same as list mode but you can use account's lists feature to configure entries
* **static** set the caller id to a value in a `caller_id.name` and `caller_id.number` property in the data object. This works with and without a capture group

## Callflow fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Method to use for changing the Caller ID. Valid value are `manual` and `list` | `string` | `"manual"` | `false`
`caller_id` | caller id for `static` action | `object` | | `false`
`caller_id.name` | caller id name for `static` action | `string` | | `false`
`caller_id.number` | caller id number for `static` action | `string` | | `false`
`enforce_call_restriction` | in `list`  and `lists` action mode, should call be restricted by number classification | `boolean` | `true` | `false`
`id` | The id of document in database that holds the new Caller ID when action is set to `list` or `lists` | `string` | | `false`
`interdigit_timeout` | The amount of time (in milliseconds) to wait for the caller to press the next digit after pressing a digit | `integer` | 2000 | `false`
`max_digits` | maximum digits length to collect for `manual` action | `integer` | 10 | `false`
`media_id` | The id of the media prompt to play when collecting Caller ID from user if action is set to `manual` | `string` | `"dynamic-cid-enter_cid"` | `false`
`min_digits` | minimum digits length to collect for `manual` action | `integer` | 10 | `false`
`whitelist_regex` | the regex that will run over collected digits for number verification | `string` | `"\\d+"` | `false`

## Manual action mode

In this method user is prompted to dial a new Caller ID number. The length of the dialed new Caller ID is checked to be in the default(or configured) boundary and it is also matched against the default(or configured) regex, if these checks are passed the call will proceed.

> **Notes** You can only set the Caller ID number with this method.

### Example Scenario

User dial `*212223332222` (assuming you set the feature code to `*2` by setting callflow patterns to `"^\\*2([0-9]{2,})$"`) and
prompted to dial the new Caller ID number, then the call will send onto real destination which in this case is `12223332222`.

### Manual callflow settings

You may want to customize `manual` behavior in `system_configs/callflow.dynamic_cid`.

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`accept_prompt` | The media that would be played when the new caller id is accepted | `string` | `"tone_stream://%(250,50,440)"` | `false`
`reject_prompt` | The media that would be played when the new caller id is rejected | `string` | `"dynamic-cid-invalid_using_default"` | `false`
`default_prompt` | The media that would be played to user to dial the new caller id | `string` | `dynamic-cid-enter_cid` | `false`
`max_digits` | Maximum length of the new caller id number | `integer` | 10 | `false`
`min_digits` | Minimum length of the new caller id number | `integer` | 10 | `false`
`whitelist_regex` | The regex to use for number to be matched against | `string` | `"\\d+"` | `false`

## List action mode

In this method you have flexibility to define multiple Caller ID, each assign to a specific number using account's list. You can set both the Caller ID name and
number in a document in database. This callflow's module use that information to set new Caller ID. You can create your list and list's entries using [List Crossbar API](../../crossbar/doc/lists.md).

This done by first getting group capture which populate by callflow automatically (feature code is removed from the beginning) then get a specific length of numbers starting from the beginning of the dialed number (by default 2 numbers). This number is it the key (or index) in a list of Caller ID entry list. For and example how this works, see the example section.

This behavior is exactly same as `lists` action, the only difference is that you can define the index of the matching number (entry index) in the callflow data in `idx_name`.

> **Notes** You can set  both the Caller ID number and name with this method.

### List Entry document fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`capture_group_length` | Length of the numbers in the list entries for which it used to select desire Caller ID | `integer` | 2 | `false`
`capture_group_key` | The number which is used to select this entry, length should be same as `capture_group_length` | `string` | | `true`
`number` | Caller ID number | `string` | | `true`
`name` | Caller ID name | `string` | | `true`

### Example 1: Two digits as CID selector

Consider that the user wants to call number `5149072508` using the caller id `16139999999`. We assume that callflow patterns set as follow:

```json
    "patterns": [
        "^\\*2([0-9]{2,})$"
    ]
```

and the content list document is:

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
