### Missed Call Alert

#### Overview

The `missed_call_alert` callflow allows users to receive an email notification for unanswered inbound calls that do not result in a voicemail message.

An inbound call in the scope of this module is defined as one that originates external to the Kazoo system, as opposed to calls between users/devices in the Kazoo account.

It should be noted that if a call is forwarded to an external device, such as a cell phone, that has a voicemail system, Kazoo will not be able to determine if a voicemail message is left.

Moreover, if the call forwarding is configured without requiring the user to press a digit to accept forwarded calls then Kazoo will consider the call answered regardless of whether answered by the user or the external voicemail system.

#### Schema

Validator for the missed_call_alert callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on specified type | `string() | array(string())` |   | `true` |  
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user' | 'email')` |   | `true` |  
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object())` |   | `true` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  






#### Description

The Kazoo callflow executor allows modules to register functions that should be executed when the call is terminated (on `CHANNEL_DESTROY` or `CHANNEL_DISCONNECTED` events). After modules registered themselves, each registered module will be executed serially.

As a call is processed by the callflow executor, it will observe the event stream for the channel and set a flag on the Kazoo call object if a successful bridge is performed. The callflow voicemail module will also set a flag on the Kazoo call object when a voicemail message is successfully left.

When `missed_call_alert` executed as part of a callflow or preflow, will determine if the call originated as an inbound call to the account. If the call was inbound, then it will register itself as a termination function with the callflow executor.

When this function is invoked from the callflow executor it will inspect the Kazoo call object, specifically the two said above flags. If neither are set to `true` this function will then publish a new AMQP message to the notification exchange. This message will be received by the Kazoo teletype which will process a email template for missed calls, and send the email.

#### Configuring Recipients

The data object of this module can define one or more specific email addresses, Kazoo user ids or a combination of both. If a Kazoo user is provided, then the currently configured user email address is used. If no recipients are provided, then it will be assumed to be configured on the email template.

#### Example Callflow
```json
{
    "flow":{
        "data":{
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
        "module":"missed_call_alert",
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
