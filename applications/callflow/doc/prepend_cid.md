
# Prepend CID

The `cf_prepend_cid` module can be used to prepend static values to the Caller ID Name and Number for a call in callflows.

## Configuration Values

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | If the action parameter is "reset" the caller-id-name and caller-id-number will be restored to their original values, if the value is "prepend" then the prepending will happen | `string` | "prepend" | `no`
`caller_id_name_prefix` | The prefix that should be applied to the caller-id-name | `string` | "" | `no`
`caller_id_number_prefix` | The prefix that should be applied to the caller-in-number | `string` | "" | `no`
`apply_to` | Either "original" or "current", this specifies that the prefix's should be applied to the current caller-id's or the oginial values | `string` | "original" | `no`

## Notes

* After the callflow action is run, the original caller-id values will be stored in the `original_cid_name` and `original_cid_number` fields of the call.
* When using "current" for the `apply_to` you can get into a situation where it will append the value multiple times if the callflow is included in a loop.

## Example Configuration

```json
"flow": {
    "data": {
        "action": "prepend",
        "caller_id_name_prefix": {PREFIX_NAME},
        "caller_id_number_prefix": {PREFIX_NUMBER},
        "apply_to": "original"
    },
    "module": "prepend_cid",
        "children": {
    }
}
```
