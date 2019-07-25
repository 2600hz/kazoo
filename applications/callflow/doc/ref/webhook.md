## Webhook

### About Webhook

#### Schema

Validator for the webhook callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_data` | JSON object of custom data included on the HTTP request | `object()` |   | `false` |  
`http_verb` | What HTTP verb to use when sending the request | `string('post' | 'get')` |   | `false` |  
`retries` | How many times to retry the request if the host isn't available | `integer()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`uri` | The HTTP URI to send the request | `string()` |   | `false` |  



