# Webhook

Send a custom webhook to your web server during the callflow.


## The Webhook Callflow Action

Webhooks can be triggered from a callflow without needing them to be predefined by an API call. They are useful in tracking the state of a caller in a phone tree, triggering actions on the receiver's end, or whatever imagination can come up with.


#### Schema

Validator for the webhook callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_data` | JSON object of custom data included on the HTTP request | `object()` |   | `false` |  
`http_verb` | What HTTP verb to use when sending the request | `string('post' | 'get')` |   | `false` |  
`retries` | How many times to retry the request if the host isn't available | `integer()` |   | `false` |  
`uri` | The HTTP URI to send the request | `string()` |   | `false` |  






##### An example JSON action

```json
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


## Using the webhook action in a callflow

To receive a webhook anytime the main company number goes to voicemail instead of being answered, for instance:

```json
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

Now your web server will receive a webhook payload anytime the caller is sent to voicemail instead of talking to the front desk.
