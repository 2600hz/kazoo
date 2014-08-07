/*                                                                                                                                                                                                                                                                                                 
Section: Callflow
Title: Preflow
Language: en-US
*/

# Preflow

Preflow is an option to execute a callflow before any other callflow. It is set on the account level and will apply on any call using callflow for that account.

```Javascript
"preflow": {
	"ringback" : "DYNAMIC_RINGBACK_MODULE"
    "always": "CALLFLOW_ID"
}
```
The `DYNAMIC_RINGBACK_MODULE` corresponds to the module you want to call to get dynamic ringback media. This module MUST expose get_ringback(whapps_call:call()) -> {custom ringback media_id binary,opaque binary}. Opaque will be added to the preflow.always callflow arguments as "opaque" property.
The `CALLFLOW_ID` corresponds to the callflow you want to call before any other.
