# Kazoo JSON

Manipulating JSON objects in Erlang.


## Accessors


### Getters

The `kz_json` module provides a generic accessor, `get_value(Path, JObj[, Default])` that will return the value at `Path` (`'undefined'` or `Default` if missing).

There are also a number of accessors that will make sure the value retrieved is of a certain type; otherwise the `Default` is used. Functions like `get_ne_binary_value` will make sure the `Value` is a non-empty binary, for instance.

Another accessor is `get_first_defined(Paths, JObj[, Default])` which takes a list of paths to search, returning the first non-`'undefined'` value found iterating through `Paths`; otherwise `'undefined'` or `Default` is returned.

-   `get_keys(JObj)`: returns the top-level keys of the `JObj`.
-   `get_values(JObj)`: will return a tuple `{Values, Keys}` from the object (similar to `lists:unzip/2`).
-   `values(JObj)`: returns just the top-level `Values` of the object.
-   `new()`: returns an empty JSON object.


### Setters

-   `set_value(Path, Value, JObj)` will put `Value` at `Path` (including creating nested objects as necessary).
-   `set_values([{Path, Value}], JObj)` will put `Values` at `Paths` (including creating nested objects as necessary).
-   `insert_value(Path, Value, JObj)` will only put `Value` at `Path` if there isn't a value there already.
-   `insert_values([{Path, Value}], JObj)` will only put `Value` at `Path` if there isn't a value there already.


### Deleting keys

-   `delete_key(Path, JObj[, 'prune' | 'no_prune'])`: will delete the value at Path from the JObj. You can optionally prune the resulting empty object (`'no_prune'` is the default).
-   `delete_keys(Paths, JObj)`: will delete `Paths` using `'no_prune'`. Use `prune_keys(Paths, JObj)` if you want empty objects pruned.

Note that `set_value(Path, 'null', JObj)` is equivalent to `delete_key(Path, JObj)`.


## Predicates

Several predicates exist that will return booleans.

-   `is_true(Path, JObj[, Default])`: returns if the value at `Path` is "true" in some form (atom, list, binary). `is_false` exists as well for "false".
-   `is_defined(Path, JObj)`: Returns if the value at `Path` is not `'undefined'`.
-   `is_empty(JObj)`: returns `true` if `JObj` is the empty JSON object (`{[]}` currently but could be `#{}` if the switch to maps is made).
-   `is_json_object(JObj)`, `are_json_objects(JObjs)`: Superficial check for the internal JSON structure. Use `is_valid_json_object(JObj)` to recurse into the data structure.
-   `is_json_term(Term)`: returns if the `Term` is a `JSON-able` term. Terms like `pid()`, `reference()`, and such have no parallel in JSON and are thus considered to not be JSON terms.
-   `are_equal(JObj1, JObj2)`: compares two objects for equality, irrespective of how the underlying ordering of keys.


## Converters


### Away from JSON objects

-   `to_proplist(JObj)`: Returns the top-level object as a proplist instead (nested objects remain objects).
-   `to_map(JObj)`: Returns a map representation of the object
-   `recursive_to_proplist(JObj)`: Converts nested objects to proplists as well


### To JSON objects

-   `from_list(Proplist)`: Top-level change from proplist to object
-   `from_map(Map)`: Converts map back to JSON object data structure
-   `from_list_recursive(Proplist)`: Equivalent to `from_list_recursive/2` with default option: `#{ascii_list_enforced => 'true', invalid_as_null => 'true'}`
-   `from_list_recursive(Proplist, #{}=Options)`: Converts nested proplists to objects as well. Options are:
    -   `ascii_list_enforced`: If the value of a proplist item is list and all of the elements are ASCII characters, convert the value to binary. This also converts empty list `[]` to binary. If you are sure that you don't have any string value, but expect and empty list as value, set this option to `false` to not convert the empty list to binary
    -   `invalid_as_null`: If the value of a proplist item is not any of `list()`, `binary()`, `atom()`, `integer()`, `float()`, `kz_time:date()`, `kz_time:datetime()`, `kz_json:object()` or `kz_term:proplist()` then set the value to `null`, otherwise throw `{'error', kz_term:ne_binary()}`

## Merging Objects

You can merge two (or more) objects together and specify which objects' value will be used when there are conflicting keys.

-   `merge([JObj1, JObj2,...])`: Defaults to using the `merge_right` strategy where the object on the "right" will have its value used when both objects have a value at a give key path.
-   `merge(Strategy, JObjs)`: Choose a strategy, either `fun kz_json:merge_right/2` or `fun kz_json:merge_left/2` (or define your own), and apply it to the list of objects.

### Handling `null`

Sometimes it is beneficial to include the `null` atom as a value (for downstream processing perhaps) versus triggering a `delete_key` equivalent.

`set_value` and `merge` can take an optional map `#{'keep_null' => 'true'}` to ensure `null` is kept in the resulting data structure.

### `merge/3` vs `merge_recursive/2`

There is a second, older implementation of merging objects called `merge_recursive/2`. The main difference is that it operates differently when merging a path in the second object that has an empty object as the value:

```erlang
> J1 = {[{<<"foo">>,false}]},
> J2 = {[{<<"foo">>,false}, {<<"bar">>,{[{<<"foo">>,{[]}}]}}]}.
> kz_json:merge(fun kz_json:merge_right/2, J1, J2).
{[{<<"foo">>,false},{<<"bar">>,{[{<<"foo">>,{[]}}]}}]}
> kz_json:merge(fun kz_json:merge_left/2, J1, J2).
{[{<<"foo">>,false},{<<"bar">>,{[{<<"foo">>,{[]}}]}}]}

> kz_json:merge_recursive(J1, J2).
{[{<<"foo">>,false}]}
> kz_json:merge_recursive(J2, J1).
{[{<<"foo">>,false},{<<"bar">>,{[{<<"foo">>,{[]}}]}}]}
```

Since `bar.foo` in `J2` is an empty object, the values won't be set because folding over an empty object returns the accumulator (`J1` in this case). Nothing in `merge_recursive/4` sets `bar` or `bar.foo` onto `J1`.

More generally, the longest key path suffix with an empty object as the value on the right side of the merge will be stripped from the merged object when using `merge_recursive/2`.

## List-like operations

JSON objects are iterable and `kz_json` has equivalents to the `lists` module for `filter/{2,3}`, `filtermap/2`, `map/2`, `fold{l,r}/3`, `foreach/2`, `all/2`, and `any/2` among others.

Another of note is `find(Path, JObjs[, Default])` which searchs a list of JSON objects for a key path. There is also a `find_first_defind(Paths, JObjs[, Default])` which will `find(Path, JObjs)`, continuing to the next `Path` if `'undefined'` is returned.

There is also a `find_value(Path, Value, JObjs[, Default])` which will find the first `JObj` in the list that has `Value` at `Path`.


## Lifting common JSON properties


### Basic usage

```erlang
kz_json:lift_common_properties([JObj1, JObj2]) ->
    {CommonJObj, [UniqueJObj1, UniqueJObj2]}.
```

All key/value pairs common to each JObj in the list will be collected into `CommonJObj` and removed from each `JObj`.


### Blacklists

Sometimes you know there are keys that shouldn't be lifted (like leg-only variables on endpoints vs channel variables):

```erlang
kz_json:lift_common_properties([JObj1, JObj2,...], [Key1, Path2]) ->
    {CommonJObj, [UniqueJObj1, UniqueJObj2,...]}.
```

This will not allow `Key1` and `Path2` to be in `CommonJObj` and will remain in the objects that have it set. Note that the blacklist can be keys (`<<"key">>`) or paths (`[<<"key">>, <<"subkey">>]`).
