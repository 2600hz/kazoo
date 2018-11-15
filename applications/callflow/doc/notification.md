### Custom Callflow Notifications

#### Overview

The `notification` callflow allows accounts to send customer-defined notifications to predetermined recipients.

#### Schema

Validator for the 'notification' callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`comments` | Text message that need to include into notification | `string()` |   | `false` |  
`notification_media.[]` |   | `string('email' | 'sms')` |   | `false` |  
`notification_media` | What is media need to use for notification | `array(string('email' | 'sms'))` |   | `false` |  
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on specified type | `string() | array()` |   | `true` |  
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user' | 'email')` |   | `true` |  
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object())` | `[]` | `true` |  
`send_at` | Defines when send customer defined notification. For `callflow_exec` value notifications is send during callflow execution. For `channel_destroy` value notification is send after channel(bridge) is destroyed | `string('callflow_exec' | 'channel_destroy')` | `channel_destroy` | `false` |  
`template_id` | Template ID of account defined notification | `string()` |   | `false` |  






#### Description

This callflow allow send custom defined notification and then send custom account teletype template from callflow.

Notification may be send during callflow execution or after call is destroyed.

#### Configuring Recipients

The data object of this module can define one or more specific email addresses, Kazoo user ids or a combination of both. If a Kazoo user is provided, then the currently configured user email address is used. If no recipients are provided, then it will be assumed to be configured on the email template.

#### Example Callflow
```json
{
    "flow":{
        "data":{
           "send_at": "callflow_exec",
           "template_id": "my_custom_template",
            "recipients":[
                {
                    "type":"user",
                    "id":"bdc3a4b7e3a5405a9aa8a0f9c6b76997"
                },
                {
                    "type":"email",
                    "id":[
                        "support@test.com",
                        "john@doe.com"
                    ]
                }
            ]
        },
        "module":"notification",
        "children":{
            "_":{
                "data":{
                    "id":"3e3f1217bee3ba0aa7496ca61fb4ae8f",
                    "can_call_self": false,
                    "timeout": 20,
                    "delay": 0,
                    "strategy": "simultaneous"
                },
                "module":"user",
                "children":{
                    "_":{
                        "data":{
                            "id":"f3f32d9692652565e155e931a54ab0a8",
                            "action":"compose",
                            "callerid_match_login":false,
                            "interdigit_timeout":2000,
                            "max_message_length":500,
                            "single_mailbox_login":false
                        },
                        "module":"voicemail",
                        "children":{}
                    }
                }
            }
        }
    },
    "numbers":[
        "+15554441111"
    ]
}
```
