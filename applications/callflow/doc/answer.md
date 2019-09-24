## Answer

### About Answer

Attempts to answer the call; will block callflow execution until the call is answered (`CHANNEL_ANSWER` received).

#### Schema

Validator for the answer callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



