Parking will place a call in a numbered "slot" where it will reamain until it is retrieved or the caller hangs up.

## Example of `data` payload

    "data": {
        "action": "{ACTION}"
        ,"default_ringback_timeout": {RINGBACK}
        ,"default_callback_timeout": {CALLBACK}
        ,"default_presence_type": {PRESENCE_TYPE}
        ,"slots": {}
    }

## Mandatory fields
**action** - action (see below)

## Optional fields

* **{ACTION}**: default is `park`
    * `auto` - If the provided slot has an active call then retrieve it, if not then the calling channel is parked.
    * `park` - Attempt to place the calling channel into the indicated slot.  If the slot is already occupied then the attempt is rejected.
    * `retrieve` - Attempt to retrieve a call from the indicated slot.  If the slot does not have an active call then the attempt is rejected.
* **{RINGBACK}** - When a call is parked and the device parking the call is known then parked call will ring the parker on this period.  The default is set in the `system_config` database on the `callflow.park` document.
* **{CALLBACK}** - When a parked call has remained parked for the {RINGBACK} duration the parker will be called for this time.  The default is set in the `system_config` database on the `callflow.park` document.
* **{PRESENCE_TYPE}** - This parameter overrides the dialog state used for occupied slots.  The default can be set in the account db as an account config on `configs_callflow.park` or the `system_config` database on the `callflow.park` document.
    * `early` - Indicate that the call state is 'ringing', generally this causes the phones to attempt to pick up a ringing call as well as blink.
    * `confirmed` - Indicate that the call state is 'answered'.  
* **{SLOTS}** - This object is used to override the options on a per-slot bases where the object key is the slot number.  When setting parameters per-slot the leading 'default_' should be removed.

### Slots format
#### Example

    "100": {
        "ringback_timeout": "{RINGBACK}"
        ,"callback_timeout": "{CALLBACK}"
        ,"presence_type": "{PRESENCE_TYPE}"
    }

## Children Options

When a ringback fails the park module will look for a child key which matches the parking slot number.  If one is found then the callflow is branched to that child, otherwise the call is re-parked.

## Example Flow

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
