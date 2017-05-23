### knm_rename_carrier

The kazoo number manager `rename_carrier` module can be used to change a number's carrier module.

Only admins can use this feature, otherwise an exception is thrown.

It is a "one time" feature in the sense that once renaming is done, the feature is removed from the number's document.

The value for the `"carrier_name"` field is a carrier name without the KNM prefix.

Examples:

* `"bandwidth2"`
* `"telnyx"`
* `"local"`

Inputs that do not map to a KNM carrier module will fail the request with `invalid`.

#### Enabling the feature

Admins that want to be able to use this feature should add `"carrier_name"`
to the list of allowed features for the system, their reseller account, a sub account or even a specific number.

For example:

```json
    "features": {
        "allow": [
            "carrier_name",
            "cnam",
            "e911"
        ]
```
