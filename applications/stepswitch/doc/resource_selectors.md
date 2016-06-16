# Stepswitch Resource Selectors
Stepswitch Resource Selectors (SRS) - new flexible way to route outbound offnet calls to carriers (resources). SRS controled by chain of small modules. Output one module is the input of other module (similar to "pipes" in Unix-shell). Input and output of each module is list of resources. Each module make some actions with this list like adding new resources, filtering resource, reordering resources, etc...  
List of modules with addition parameters (controlling behavior of modules) called "rules". This rules saved at Account-DB in document "resource_selectors_rules". Rules in Master Account DB acting as "system" rules and applied to resources in "offnet" DB.  
You can use module several times with different parameters.

## Rules
Rules is array of JSON objects. Key of object is the name of module. Value is another object contains paramters for module.  
Example:
```JSON
{
  "rules": [
    {
      "get_resources": {}
    },
    {
      "filter_list": {
        "value_a": "request:Flags",
        "value_b": "resource:flags",
        "action": "keep",
        "mode": "ne_subset_or_exact"
      }
    },
    {
      "filter_regex": {
        "value_a": "number",
        "value_b": "resource:rules",
        "action": "keep",
        "mode": "empty_fail"
      }
    },
    {
      "filter_regex": {
        "value_a": "cid_number",
        "value_b": "resource:cid_rules",
        "action": "keep",
        "mode": "empty_ok"
      }
    },
    {
      "order": {
        "value": "resource:weight_cost",
        "direction": "ascend"
      }
    }
  ]
}
```
## Modules
### get_resources (kz_srs_get_resources)
This module ignore any input and simply load resources from DB.  
Module doesn't have any parameters. Resources loaded from "Offnet" DB or from Account DB (if Offnet-Request have Hunt-Account-Id).
### Filter modules
This modules filter resources with some criterion.  
All filter modules have similar parameters:
Name | Descritpion
-|-
value_a | Value, which will be used for filtering (phone number)
value_b | Rules for testing (list of regexes for phone number)
action | What to do when `value_a` matched rules in `value_b`, `keep` or `drop`
mode | Filter specific paramter

`value_a` and `value_b` can be:
- number - dialed phone number in e164 format
- cid_number - Caller-ID number
- resource:{RESOURCE_FIELD} - value ov the resource field like flags, weight, etc... (resource:flags, resource:weight_cost)
- request:{REQUEST_FIELD} - value of field in Offnet-Request (request:Flags)
- database:{SELECTOR_NAME} - value got from database (this selectors managed via cb_resource_selectors module)
#### filter_prefix (kz_srs_filter_prefix)
`value_a` should be a string, `value_b` showul be a list of prefixes. For example `value_a` is number (dialed number "+74951234567") and `value_b` is `database:prefix_list_1` which returns list of rows:
selector | resource
-|-
7 | Res-1
7 | Res-6
7495 | Res-1
7495 | Res-3
7499 | Res-2
749912 | Res-3
749512 | Res-5
749512345 | Res-1

Action: keep 
On module input we get resources: Res-1, Res-2, Res-3, Res-4, Res-5, Res-6
After applying filter we get result:
resource | matched prefix
-|-
Res-1 | 749512345
Res-3 | 7495
Res-5 | 749512
Res-6 | 7
Module output will be: Res-1, Res-3, Res-5 and Res-6
#### filter_regex (kz_srs_filter_regex)
This module filter resources based on list of regex rules (usualy it's `rules`/`cid_rules` from resource).  
`mode` parameter for this module:
- `empty_fail` - if list of rules is empty, then it equal to fail match (default behavior)
- `empty_ok` - if list of rules is empty then it equal to match, this behavior useful for Caller-ID check. You allow any Caller-ID on most of your resources, but several resources have strict rules. For this kined resources you write rules, all other leave with empty `cid_rules` field.
#### filter_list (kz_srs_filter_list)
This filter can be used for select resources bassed on "flags" assigned to resource and account/user/device.  
Lists in `value_a` and `value_b` sorted and deduplicated (["flag1", "flag3", "flag2", "flag3", "flag1"] become ["flag1", "flag2", flag3"]).
Values from `values_a` searched/matched with values from `value_b`.  
Default rule:
```JSON
{
  "filter_list": {
    "value_a": "request:Flags",
    "value_b": "resource:flags",
    "action": "keep",
    "mode": "ne_subset_or_exact"
  }
}
```
Here we got list of flags from "Flags" field in Offent-Request, and compare it with field "flags" in each resource.  
`mode` parameter:
- exact - exact matching of lists in `value_a` and `value_b`, for example 2 empty lists matched, ["flag1", "flag2", "flag3"] and ["flag3", "flag1", "flag2"] matched too.
- subset - `value_a` must be subset of `value_b`, ["flag2", "flag3"] will match ['flag5", "flag3", "flag2"], empty list in `value_a` match any list (empty too) in `value_b`.
- ne_subset - similar to "subset", but empty list in `value_a` always fail
- ne_subset_or_exact - similar to "subset", but empty list in `value_a` match only with empty list in `value_b`
- intersect - match if any elemnt in `value_a` exist in `value_b`, empty `vaule_a` and/or `value_b` will fail to match
- disjopint - inverse of "intersect" mode
### order (kz_srs_order)
This module sort resources by some field/criteria.  
Default rule:
```JOSN
{
  "order": {
    "value": "resource:weight_cost",
    "direction": "ascend"
  }
}
```
`Value` can be field from resource or selector from database, `direction` can be "ascend" or "descend".
