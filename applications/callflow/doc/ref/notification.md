## Notification

### About Notification

#### Schema

Validator for the 'notification' callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`comments` | Text message that need to include into notification | `string()` |   | `false` |  
`notification_media.[]` |   | `string('email' | 'sms')` |   | `false` |  
`notification_media` | What is media need to use for notification | `array(string('email' | 'sms'))` |   | `false` |  
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on specified type | `string() | array(string())` |   | `true` |  
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user' | 'email')` |   | `true` |  
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object())` | `[]` | `true` |  
`send_at` | Defines when send customer defined notification. For `callflow_exec` value notifications is send during callflow execution. For `channel_destroy` value notification is send after channel(bridge) is destroyed | `string('callflow_exec' | 'channel_destroy')` | `channel_destroy` | `false` |  
`template_id` | Template ID of account defined notification | `string()` |   | `false` |  



