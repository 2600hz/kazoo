### Features

#### Annoucements

audio file played to caller to annouce position in the queue and estimated wait time.

add this object to the queue document
```
        "announcements": {
            "wait_time_announcements_enabled": true,
            "position_announcements_enabled": true,
            "interval": 30
        },
```

#### Queue Strategies

`round_robin`
`ring_all`
`most_idle`
`sbrr`

#### Agent Priorities

1 to 128 : Higher priority agents will be called 1st

#### Call Priorities

Higher priority calls can jump the queue
1) callflow can set "priority" in data to an integer value, higher the number the higher the priority
2) and/or the incoming call can have a custom variable <<"Call-Priority">> again an integer


#### Member Callback

Member can have the option of being called back when they reach the head of the queue rather than waiting in line


#### Agent Pause 

An agent can be on a break and pause for set time either via the API or via a feature code


#### Early wrapup

An Agent can make himself available before the wrapup period via the API


#### Agent Availability check callflow

Example callflow:

```
    "flow": {
      "module": "acdc_agent_availability",
      "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
      "children": {
       	 "available": {
             "data": {
                        "id": "9a218da18b8104c888f0d47d946ffac0"
                    },
                    "module": "acdc_member",
                    "children": {}
                },                
          "unavailable": {
              "data": {
			"id": "/system_media/en-us%2Fqueue-no_agents_available"
		      },
   			"module": "play",
              		"children": {}
               }
            }
    }
```

#### Average wait time check callflow

Example callflow:

```
  "module": "acdc_wait_time",
      "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
      "children": {
            "_": {
                   "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
                   "module": "acdc_member",
                   "children": {}
                 },
            "60": {
                   "data": {"id": "/system_media/en-us%2Fqueue-no_agents_available"},
                   "module": "play",
                   "children": {}
                  }
      }
```

