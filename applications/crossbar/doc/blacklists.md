### Blacklists

#### About Blacklists

A blacklist is a map of caller id numbers that can be then apply to the account to block these callers to call the system.

#### Schema

Schema for a blacklists



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true` |  
`numbers` | Map of caller id number to block | `object()` | `{}` | `false` |  
`should_block_anonymous` | Should block Anonymous call | `boolean()` |   | `false` |  



#### Fetch

