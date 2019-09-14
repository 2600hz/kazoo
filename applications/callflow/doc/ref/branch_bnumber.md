## Branch Bnumber

### About Branch Bnumber

#### Schema

Validator for the branch_bnumber callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`hunt` | Should the capture group be used to hunt for a new callflow | `boolean()` | `false` | `false` |  
`hunt_allow` | A regexp used to match against the capture group to whitelist allowed numbers to hunt | `string()` |   | `false` |  
`hunt_deny` | A regexp used to match against the capture group to blacklist denied numbers to hunt | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



