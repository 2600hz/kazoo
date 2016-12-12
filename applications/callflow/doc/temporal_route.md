
# Rule Set

To use  a Rule Set in a callflow:

1. Create your `temporal_route` module and add the `rule_set` field to the data payload. The `rule_set` should be the `ID` of the document.
2. Set your first children to be the catch all (using `_` for the key).
3. Set the second one using `rule_set`.

If one (or more) of the rule in the rule set is satified it will got to the children using the `rule_set` key. If not to the catch all.