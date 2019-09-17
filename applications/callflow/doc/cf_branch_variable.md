# Callflow Branch on a Variable's Value

## Overview

The `branch_variable` callflow enables you to branch based on value of some field inside one of the a call CCVs, user's document, device's document or an account's document.

### Schema

Validator for the branch_variable callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`scope` | specifies where the variable is defined | `string('account' | 'custom_channel_vars' | 'device' | 'merged' | 'user')` | `custom_channel_vars` | `false` |
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |
`variable` | specifies the name of variable/property that should be looked up | `string()` | "" | `true` |


### Description

The purpose of this callflow is to branch on value of some property so it may have children named after possible values of the property, e.g. branch on boolean value of a property with the name `"call_forward"`. For this, there should be three children on the callflow: `"true"`, `"false"` (and `"_"` for the case when property is not defined anywhere and it's value is unknown).

#### Scope

The place that the variable can be defined is configurable by `scope`. By default, if the scope is not defined or if it is set to `"custom_channel_vars"`, a call's CCVs would be looking for the variable's value.

Supported places for defining the variable:

* `"accounts"`: in the account's document
* `"custom_channel_vars"`: in the Call's Custom Channel Variables
* `"device"`: in the device's documents
* `"merged"`: in the endpoint object (the merge of device, user and account)
* `"user"`: in the user's documents

#### Variable

Variable is the name of the variable or property that should be look for in the specified scope. It must be a valid JSON key, e.g. a single string or a list of string which is a path to deep nested JSON objects.

For example, if you set `scope` to `merged`, this module tries to use endpoint which has the merged value of user, device, and account document. The endpoint has `record_call` attribute which is JSON object:

```json
...
{
	"record_call": {
		"action": "start",
		"record_call": true
	}
}
...
```

If you want to record the calls based on the merged value inside the endpoint then the variable should be set to `["record_call", "record_call"]`. This will result the value `'true'`' of the inner attribute of the JSON object been fetched.

#### Example

If you want to conditionally record outbound calls in `no_match` callflow depending on a property sets for users inside their documents:

```json
...
{
	"data": {
    	"scope": "user",
    	"variable": "x_user_record_outbound"
 	},
 	"children":{
		"true": { },
    	"false": { },
		"_" : { }
 	}
}
...
```

The name of the property that is been looked is set on the `"variable"`. Property name of each child ("children") is the value of the variable that has been found.

`"true"`
:  If the variable is set to the `true`

`"false"`
:  If the variable is set to `false`

`"_"`
:  If the variable is not defined or has a different value
