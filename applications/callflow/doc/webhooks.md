- [The Webhook Callflow Action](#org6301609)
  - [The Webhook action](#org712b632)
  - [Using the webhook action in a callflow](#org3ed7369)



<a id="org6301609"></a>

# The Webhook Callflow Action

Webhooks can be triggered from a callflow without needing them to be predefined by an API call. They are useful in tracking the state of a caller in a phone tree, triggering actions on the receiver's end, or whatever imagination can come up with.


<a id="org712b632"></a>

## The Webhook action

See the [schema](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/couchdb/schemas/callflows.webhook.json) for details of what can go in the action's data object.

| Field        | Description                                                    | Type                | Default | Required |
| uri          | The HTTP URI to send the webhook data to                       | string()            |         | true     |
| http\_verb   | The HTTP verb to use                                           | enum("post", "get") | "post"  | false    |
| retries      | How many times to retry the server                             | integer(0..4)       | 2       | false    |
| custom\_data | A JSON object of custom data to include on the webhook payload | object()            | {}      | false    |

An example JSON action

```js2
{"module":"webhook"
 ,"data":{
     "uri":"http://my.ser.ver/path/to/webhook/destination"
     ,"http_verb":"post"
     ,"custom_data":{
         "some_id":"123abc"
         ,"app":"my_cool_app"
     }
 }
}
```


<a id="org3ed7369"></a>

## Using the webhook action in a callflow

To receive a webhook anytime the main company number goes to voicemail instead of being answered, for instannce:

```js2
{"numbers":["+10005559999"]
 ,"flow":{
     "module":"device"
     ,"data":{"id":"front_desk_device_id"}
     ,"children":{
         "_":{
             "module":"webhook"
             ,"data":{
                 "uri":"http://my.ser.ver/frontdesk/missed_call"
             }
             ,"children":{
                 "_":{
                     "module":"voicemail"
                     ,"data":{"id":"front_desk_voicemail_box_id"}
                 }
             }
         }
     }
 }
}
```

Now your webserver will receive a webhook payload anytime the caller is sent to voicemail instead of talking to the front desk.