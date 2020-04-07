## Check Cid

### About Check Cid

Handles inspection of incoming caller id and branching to a child callflow node accordingly.

#### Schema

Validator for the check_cid callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`caller_id.external.name` | Update external Caller ID Name | `string()` |   | `false` |  
`caller_id.external.number` | Update external Caller ID Number | `string()` |   | `false` |  
`caller_id.external` |   | `object()` |   | `false` |  
`caller_id` |   | `object()` |   | `false` |  
`regex` | Determine match/nomatch when use_absolute_mode is false | `string()` | `.*` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`use_absolute_mode` | If true, direct call down a branch that matches the caller ID | `boolean()` | `false` | `false` |  
`user_id` | kazoo User ID to use as owner_id instead of detected owner_id | `string()` |   | `false` |  






## Example

### Absolute mode

When `use_absolute_mode` is `true`, a child branch is attempted if it matches the caller ID number exactly; otherwise the default child `"_"` is taken.

```json
{"module":"check_cid"
 ,"data":{
   "use_absolute_mode":true
   ,"regex":"\\+?1?(\\d{10})"
 }
 ,"children":{
   "_":{...no matching branches for the caller ID...}
   "+14158867900":{...If caller ID matches +14158867900...}
 }
}
```

### Match / No Match

When `use_absolute_mode` is `false`, two child branches can be taken, "match" and "nomatch", based on whether the regex matched the caller ID.

```json
{"module":"check_cid"
 ,"data":{
   "use_absolute_mode":false
   ,"regex":"\\+?1?(\\d{10})"
 }
 ,"children":{
    "match": { // callflow node to branch to when absolute mode is false and regex matches },
    "nomatch": { // callflow node to branch to when regex does not match or no child node defined for incoming caller id },

 }
}
```
