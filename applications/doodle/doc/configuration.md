
# Configure inbound listeners

To create multiple inbound listeners, edit the document `system_config/doodle`
and add a connections object.

```
 "connections": {
       "first": {
           "exchange": "17b153b68387ea418a2966d86d16573c",
           "type": "topic",
           "broker": "amqp://user:password@server.com:5672",
           "queue": "smsc_inbound_queue_17b153b68387ea418a2966d86d16573c",
           "options": {
               "passive": true
           }
       },
       "second": {
           "exchange": "1c3e3cf908142fb0a8ad924049dbc50e",
           "type": "topic",
           "broker": "amqp://user:password@otherserver.com:5672",
           "queue": "smsc_inbound_queue_1c3e3cf908142fb0a8ad924049dbc50e",
           "options": {
               "passive": true
           }
       }
   }
```
