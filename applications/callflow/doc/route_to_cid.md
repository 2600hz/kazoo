## Route To Cid

### About Route To Cid

This module could be used to route calls without creating static callflows for each and every endpoint. 
This could be especially useful for Asterisk based newbies to eliminate shock of absence of explicit binding between devices and extensions  :)  


```
{
   "_id": "34396ff84198c03df70b6f5073f04a38",
   "flow": {
       "children": {
       },
       "data": {
           "cid_types": [
               "internal"
           ],
           "endpoint_types": [
               "device",
               "user"
           ],
           "timeout": 60,
           "can_call_self": true,
           "delay": 0
       },
       "module": "route_to_cid"
   },
   "numbers": [
   ],
   "patterns": [
       "^([0-9]{3,4})$"
   ],
   "metadata": {
   },
   "pvt_type": "callflow"
}
```

#### Schema

Endpoints lookup by cid number



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`can_call_self` | Toggle whether devices of the same owner can call each other | `boolean()` |   | `false` |  
`can_text_self` | Toggle whether devices of the same owner can text each other | `boolean()` |   | `false` |  
`cid_types.[]` |   | `string()` |   | `false` |  
`cid_types` | CID types to perform search: internal, external, custom | `array(string())` | `["internal"]` | `false` |  
`delay` | How long to delay ringing the device, in seconds | `integer()` | `0` | `false` |  
`endpoint_types` | Endpoint types to perform search: user, device | `array()` | `[]` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`static_invite` | Override the SIP Username | `string()` |   | `false` |  
`suppress_clid` | Suppress sending caller ID | `boolean()` |   | `false` |  
`timeout` | Time, in seconds, to wait for device to bridge | `integer()` | `0` | `false` |  



