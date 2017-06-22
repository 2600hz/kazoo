### Callflow Dynamic CID

#### About Dynamic CID

The Dynamic CID callflow enables you to dynamically change the Caller ID (CID). There are different methods for doing that:

* **manual:** dial the new Caller ID on the keypad when prompted, this way you can set the Caller ID number only. This works with and without a capture group
* **list:** setting Caller ID name and number based on the configurtion on the specified document in the database. This requires a capture group
* **lists:** almost the same as list mode but you can use account's lists feature to configure entries
* **static** set the caller id to a value in a `caller_id.name` and `caller_id.number` property in the data object. This works with and without a capture group

##### Callflow fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Method to use for changing the Caller ID. Valid value are `manual` and `list` | `string` | `"manual"` | `false`
`caller_id` | caller id for `static` action | `object` | | `false`
`caller_id.name` | caller id name for `static` action | `string` | | `false`
`caller_id.number` | caller id number for `static` action | `string` | | `false`
`enforce_call_restriction` | in `list`  and `lists` action mode, should call be restricted by number classification | `boolean` | `true` | `false`
`id` | The id of document in database that holds the new Caller ID when action is set to `list` | `string` | | `false`
`interdigit_timeout` | The amount of time (in milliseconds) to wait for the caller to press the next digit after pressing a digit | `integer` | 2000 | `false`
`max_digits` | maximum digits length to ollect for `manual` action | `integer` | 10 | `false`
`media_id` | The id of the media prompt to play when collecting Caller ID from user if action is set to `manual` | `string` | `"dynamic-cid-enter_cid"` | `false`
`min_digits` | minumum digits length to collect for `manual` action | `integer` | 10 | `false`
`whitelist_regex` | the regex that will run over collected digits for number verification | `string` | `"\\d+"` | `false`

#### Manual action mode

In this method user is prompted to dial a new Caller ID number. The lenght of the dialed new Caller ID is checked to be
in the default(or configured) boundry and it is also matched against the default(or configured) regex, if these checks are passed the call will proceed.

> **Notes** You can only set the Caller ID number with this method.

###### Example Scenario

User dial `*212223332222` (assuming you set the feature code to `*2` by setting callflow patterns to `"^\\*2([0-9]{2,})$"`) and
prompted to dial the new Caller ID number, then the call will send onto real destination which in this case is `12223332222`.

##### Manual callflow settings

You may want to customize `manual` behavior in `system_configs/callflow.dynamic_cid`.

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`accept_prompt` | The media that would be played when the new caller id is accepted | `string` | `"tone_stream://%(250,50,440)"` | `false`
`reject_prompt` | The media that would be played when the new caller id is rejected | `string` | `"dynamic-cid-invalid_using_default"` | `false`
`default_prompt` | The media that would be played to user to dial the new caller id | `string` | `dynamic-cid-enter_cid` | `false`
`max_digits` | Maximum lenght of the new caller id number | `integer` | 10 | `false`
`min_digits` | Minimum lenght of the new caller id number | `integer` | 10 | `false`
`whitelist_regex` | The regex to use for number to be matched against | `string` | `"\\d+"` | `false`

#### List action mode

In this method you have flexibility to define multiple Caller ID, each assgin to a specific number. You can set both the Caller ID name and
number in a document in database. This callflow's module use that information to set new Caller ID.

> **Notes** You can set  both the Caller ID number and name with this method.

##### List document fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`length` | Lenght of the numbers in the list entries for which it used to select desire Caller ID | `integer` | | `true`
`entries` | Collection of maps of numbers to Caller ID | `object` | | `true`
`entries.{NUMBER}` | Object that contains the desire Caller ID, it's key (number) is used to select this entry | `object` | | `true`
`entries.{NUMBER}.number` | Caller ID number | `string` | | `true`
`entries.{NUMBER}.name` | Caller ID name | `string` | | `true`

###### Example 1: Two digits as CID selector

Consider that the user wants to call number `5149072508` using the caller id `16139999999`. We assume that callflow patterns set as follow:

```json
    "patterns": [
        "^\\*2([0-9]{2,})$"
    ]
```

and the content list document is:

```json
{
    "_id": "cidlist",
    "_rev": "5-FyFaandfumIsmellthebloudofanEnglishman",
    "length" : 2,
    "entries": {
        "00": {
            "number": "16139999999",
            "name": "Awesome Co."
        },
        "01": {
            "number": "19058888888",
            "name": "Awesome Inc."
        }
    }
}
```

These are the steps will happen if a user dial `*2015149072508` in handset:

1. This pattern above means that `*2` is the "feature code" for this feature. callflow will use this module to handle the call
2. `01` will use to select the Caller ID with number "19058888888" and name "Awesome Inc.". Consider that it's **length** is 2 digits as specified in `lenght` field
3. Selected CID will be set for the call
3. `5149072508` becomes `+15149072508` and gets dialed as such

###### Example 1: One digits as CID selector

Another example which uses number with lenght of 1 to select the Caller ID:

```json
{
    "_id": "cidlist",
    "_rev": "5-FyFaandfumIsmellthebloudofanEnglishman",
    "length" : 1,
    "entries": {
        "0": {
            "number": "16139999999",
            "name": "Awesome Co."
        },
        "1": {
            "number": "19058888888",
            "name": "Awesome Inc."
        }
    }
}
```

Dialing number `*205149072508` will cause number "5149072508" gets dialed with CID number set to "16139999999" CID name to "Awesome Co.".

#### Lists action mode

Almost the same as list mode but you can use account's lists feature to configure entries:

> **Notes** CID key length is always 2 in this mode.

- Create a list

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"name":"Dynamic CID","description":"List of new callerid name and number entries ","list_type":"dynamic_cid"}}' \
    http://{SERVER}/v2/accounts/{ACCOUNT_ID}/lists
```

> `list_type` field is optional here and used just for easier filtering `dynamic_cid` list among the others

- Create entries

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"cid_key":"01","cid_name":"My Office CID","cid_number":"+78124906700"}}' \
    http://{SERVER}/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```

- Create callflow dynamic_cid feature code

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"flow":{"children":{},"data":{"action":"lists","id":"{LIST_ID}"},"module":"dynamic_cid"},"numbers":[],"patterns":["^\\*69([0-9]{2,})$"],"featurecode":{"number":"69","name":"dynamic_cid"}}}'
    http://{SERVER}/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Static action mode

In this method the new Caller ID number and name would be set to `caller_id` value sets in the callflow Data object.

###### Example Callflow Data

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
