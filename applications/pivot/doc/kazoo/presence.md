# Presence

Pivot allows you to set custom presence updates (known as manual presence).

## Example

```json
{
    "module": "manual_presence",
    "data": {
        "presence_id": "username",
        "status": "ringing"
    }
}
```

Do note, `presence_id` without an `@realm.com` will be suffixed with the account's realm.

`status` can be one of `idle`, `ringing`, or `busy`.
