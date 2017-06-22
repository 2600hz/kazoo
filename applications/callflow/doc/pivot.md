## Pivot

### About Pivot

Execute an HTTP request to a web server about the call, expecting more callflow instructions in the response.

#### Schema

Validator for the Pivot callflow element



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cdr_url` | Optional URL to send the CDR to at the end of the call | `string()` |   | `false`
`debug` | Store debug logs related to processing this Pivot call | `boolean()` | `false` | `false`
`method` | What HTTP verb to send the request(s) with | `string('get' | 'post' | 'GET' | 'POST')` | `get` | `false`
`req_format` | What format of Pivot will the your server respond with | `string('kazoo' | 'twiml')` | `kazoo` | `false`
`voice_url` | What URL to request the initial Pivot callflow | `string()` |   | `true`



