# Call Parking

## About Call Parking

Parking will place a call in a numbered "slot" where it will remain until it is retrieved or the caller hangs up.  It can be configured so that if the device parking the call is known then the parked call will periodically ring the parker back.  Optionally, if the parker does not answer a ringback the callflow can be configured to advance to a child and execute further callflow actions.

#### Schema

Validator for the park callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | Action to take for the caller | `string('direct_park' | 'park' | 'retrieve' | 'auto')` | `park` | `false` |  
`custom_presence_id` | use configured presence_id and fallback to request | `boolean()` | `false` | `false` |  
`default_callback_timeout` | How long, in seconds, to wait before calling back the parker | `integer()` |   | `false` |  
`default_presence_type` | Type of presence to update | `string('early' | 'terminated' | 'confirmed')` |   | `false` |  
`default_ringback_timeout` | How long, in milliseconds, before ringing back | `integer()` |   | `false` |  
`max_slot_number` | Continue past this module if the selected slot number exceeds this number. Used to restrict the max number of auto-generated slot numbers | `integer()` |   | `false` |  
`presence_id` | use this presence_id | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`slot` | Static slot number to use | `string()` |   | `false` |  
`slots` | Statically define slots and their configuration | `object()` | `{}` | `false` |  






#### Additonal Information

Some call parking parameters can be configured in a number of ways:

* The parameters `default_ringback_timeout`, `default_callback_timeout` and `park_presence_type` can be set for the entire system in `system_config/callflow.park`.
* The parameter `parked_presence_type` can be set for an entire account in `<account-db>/configs_callflow.park`.
* The parameters `default_ringback_timeout`, `default_callback_timeout` and `default_presence_type` can be set for all slots accessed by a callflow in the flow data.
* The parameters `ringback_timeout`, `callback_timeout` and `presence_type` can be set per-slot as accessed by a callflow in the flow data `slots` object.

The presence type parameter has some inconsistent naming for backwards compatibility but `park_presence_type` and `default_presence_type` are modifying the same behaviour.

If the action's data object defines the fields, they will override system- or account-specific configs.

##### `slots` object

You can override the defaults for the entire system or individually configure slot behaviour based on the callflow used to access it.  The data parameters are:

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`ringback_timeout` | When a call is parked and the device parking the call is known then parked call will ring the parker on this period. | `integer` |   | `false`
`callback_timeout` | When a parked call has remained parked for the `ringback_timeout` duration the parker will be called for this time. | `integer` |   | `false`
`presence_id` | This parameter overrides the published presence_id for blf | `string` |   | `false`
`presence_type` | This parameter overrides the dialog state used for occupied slots. | `string('early', 'terminated', 'confirmed')` |   | `false`

##### Example of `data` object

```
   "data":{
      "action":"auto",
      "default_ringback_timeout":5000,
      "default_callback_timeout":26000,
      "default_presence_type":"early",
      "slots":{
         "100":{
            "ringback_timeout":5000,
            "callback_timeout":26000,
            "presence_type":"confirmed"
         }
      }
   }
```

#### Callflow parking children

When a ringback fails the park module will look for a child key which matches the parking slot number.  If one is found then the callflow is branched to that child, otherwise the call is re-parked.

##### Example Flow

```
{
   ...
   "flow":{
      "data":{
         "action":"auto",
         "default_ringback_timeout":120000,
         "default_callback_timeout":30000,
         "slots":{
            "100":{
               "ringback_timeout":10000
            },
            "101":{
               "ringback_timeout":25000,
               "presence_type":"confirmed"
            }
         }
      },
      "module":"park",
      "children":{
         "100":{
            "children":{

            },
            "data":{
               "id":"8ca0e4c50aa1e2901d749307e19b2e4b"
            },
            "module":"voicemail"
         }
      }
   }
   ...
}
```
