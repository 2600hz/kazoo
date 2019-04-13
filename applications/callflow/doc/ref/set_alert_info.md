## Set Alert Info

### About Set Alert Info

#### Schema

Validator for the set_alert_info callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`alert_info` | Set `Alert-Info` header inside b-leg channel. Used for distinctive ring. | `string()` |   | `true` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



