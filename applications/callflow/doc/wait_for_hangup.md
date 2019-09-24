## Wait For Hangup

### About Wait For Hangup

Blocks callflow execution until the call is hungup (`CHANNEL_HANGUP` event received).

#### Schema

Validator for the wait_for_hangup callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



