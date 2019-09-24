## Noop

### About Noop

A NO-op (no operator) will send a `noop` into the call's command queue. When executed, it will generate a no-op event. This callflow action will block until the noop has been received (effectively telling you when the command queue is empty).

#### Schema

Validator for the noop callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



