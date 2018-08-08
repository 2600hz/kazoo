## Customer Defined Notification

### About Customer Defined Notification

#### Schema

Validator for the customer_defined_notification callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on sepecified type | `string() | array()` |   | `true` |  
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user' | 'email')` |   | `true` |  
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object())` | `[]` | `true` |  
`send_at` | Defines when send customer defined notification. For `callflow_exec` value notifications is send during calllfow execution. For `channel_destroy` value notification is send after channel(bridge) is destroed | `string('callflow_exec' | 'channel_destroy')` | `channel_destroy` | `false` |  
`template_id` | Template ID of account defined notification | `string()` |   | `false` |  



