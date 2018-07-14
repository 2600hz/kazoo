### Missed Call Alert

#### Overview

The `customer_defined_notification` callflow allows send customer defined notification.

#### Data Schema

Validator for the customer defined notification callflow's data object

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on sepecified type | `string() | array()` |   | `true` |  
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user' | 'email')` |   | `true` |  
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object())` | `[]` | `true` |  
`send_on` | Defines when send customer defined notification. For `callflow_exec` value notifications is send during calllfow execution. For `channel_destroy` value notification is send after channel(bridge) is destroed | `string('callflow_exec' | 'channel_destroy')` | `channel_destroy` | `false` |  
`template_id` | Template ID of account defined notification | `string()` |   | `false` |

#### Description

This callflow allow send custom defined notification and then send custom account teletype template from callflow.

Notification may be send during callflow execution or after call is destroed.

#### Configuring Recipients

The data object of this module can define one or more specific email addresses, Kazoo user ids or a combination of both. If a Kazoo user is provided, then the currently configured user email address is used. If no recipients are provided, then it will be assumed to be configured on the email template.

#### Example Callflow
```json
{
    "flow":{
        "data":{
           "send_on": "callflow_exec",
           "template_id": "my_custome_template",
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
        "module":"customer_defined_notification",
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
