### Callflow Branch on a Variable's Value

#### Overview

The `branch_variable` callflow enables you to branch based on value of some field inside one of the a call CCVs, user's document, device's documten or an account's document.

##### Callflow fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`variable` | Name of variable/property that we are looking for | `string` or a json path | | `true`
`scope` | the place which the variable should be looked for (`custome_channel_vars`, `device`, `user`, `account`) | `string` | `"cunstom_channel_vars"` | `false`

#### Description

The purpose of this callflow is to branch on value of some property so it may have children named after possible values of the property, e.g. branch on boolean value of a property with the name `"call_forward"`. For this, there should be three children on the callflow: `"true"`, `"false"` (and `"_"` for the case when property is not defined anywhere and it's value is unknown).

The place that this variable would be looked for is configurable by `scope`. By default if the scope is not defined or if it sets to `"cunstom_channel_vars"`, it would look in the call's CCVs for getting the variable's value.

Other possible locations for which the variable would be looked up are: user's document, device's document and account's document. If you set scope to other values than these, the endpoint would be fetched and since the endpoint has merged value of some of the important attributes (like `call_forwad`, `call_restriction`, `record_call`, etc...) from account, user, device, it would be used to look up the desire variable's value.

#### Variable

Variable is the name of the variable or property that should be look for in the specified scope. It must be a valid JSON key, e.g. a single string or a list of string which is a path to deep nested JSON objects.

For example, if you set `scope` to `merged`, this module tries to use endpoint which has the merged value of user, device and account document. The endpoint has `record_call` attribute which is JSON object:

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

If you want to conditinally record outbound calls in `no_match` callflow depending on a property sets for users inside their documents:

```json
...
{
"data": {
    "scope": "user",
    "variable": "x_user_record_outbound" //name of the property defined in user's document
 },
 "children":{
	"true": { ... } //if the above variable is set to the "true"
    ,"false" { ... } //if the above variable is set to the "false"
	,"_" : { ... } //if the above variable is not defined or has a different value
 }
}
...
```
