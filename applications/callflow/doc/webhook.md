## Webhook

### About Webhook

Send a custom webhook to your web server during the callflow.

#### Schema

Validator for the webhook callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_data` | JSON object of custom data included on the HTTP request | `object()` |   | `false` |  
`http_verb` | What HTTP verb to use when sending the request | `string('post' | 'get')` |   | `false` |  
`retries` | How many times to retry the request if the host isn't available | `integer()` |   | `false` |  
`uri` | The HTTP URI to send the request | `string()` |   | `false` |  



