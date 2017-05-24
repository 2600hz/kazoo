/*
Section: Callflows
Title: Branch on callee's document property value
Language: en-US
Version: 4.0
*/

The `branch_property` callflow enables you to branch on value of some field inside callee's document (device, user or account).

### Mandatory fields

**property** - name of the property we are looking for

### Optional fields

**action** - "branch" - the only action supported now
**scope** - either "device", "user" or account. Defaults to standard KAZOO way of looking up values - it starts with callee's user, if value is not defined it looks up device, and then account documents. This approach is common with KAZOO value lookups for custom values (unlike caller ids etc.).

### Children

The purpose of this callflow is to branch on value of some property so it may have children named after possible values of the property. For example you want to branch on value of some boolean property with name "x_record_callee". This way there should be three children: "true", "false" and "_", for the case when property is not defined anywhere and it's value is unknown.

### Example

The user story is to conditinally record outbound calls in no_match callflow depending on a property set for users inside their documents

```
{
"data": {
    "action":"branch", % the only one action supported currently
    "scope": "user",
    "property": "x_user_record_outbound"
 },
 "children":{
	"true": { ... }
    ,"false" { ... }
	,"_" : { ... }
 
 }
}

```
