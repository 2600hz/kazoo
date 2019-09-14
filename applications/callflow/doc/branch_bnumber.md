## Branch Bnumber

### About Branch Bnumber

Try to branch to the capture group of the feature code

#### Schema

Validator for the branch_bnumber callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`hunt` | Should the capture group be used to hunt for a new callflow | `boolean()` | `false` | `false` |  
`hunt_allow` | A regexp used to match against the capture group to whitelist allowed numbers to hunt | `string()` |   | `false` |  
`hunt_deny` | A regexp used to match against the capture group to blacklist denied numbers to hunt | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  






### Branching vs Hunting

#### Branching

With the `hunt` flag set to false, the capture group will be used to lookup a child branch that matches:

```json
{"patterns":["*55(\\d+)"]
 ,"flow":{
   "module":"branch_bnumber"
   ,"data":{}
   ,"children":{
     "_": {...default branch...}
    ,"{CAPTURE_GROUP}": {...exact match on capture group...}
   }
 }
}
```

So if the caller dials `*551000` a child key `1000` would need to exist and define the rest of the callflow.

#### Hunting

This isn't particularly useful if you want a more dynamic experience. By setting `hunt` to `true`, the capture group can point to another callflow and branch to its `flow`. This could be used to create specific flows based on feature code that branch to the "normal" flow for the capture group.

For example, enabling per-call call recording could be implemented as:

```json
{"patterns":["*55(\\d+)"]
 ,"flow":{
   "module":"record_call"
   ,"data":{...}
   ,"children":{
     "_":{
        "module":"branch_bnumber"
        ,"data":{"hunt":true}
     }
   }
 }
}
```

Now, when `*551000` is dialed, call recording is started and then a hunt for `1000` is performed as if the caller had just dialed that to start. If found, the normal callflow for `1000` runs, just with call recording enabled.
