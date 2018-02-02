# Hang Ups

Sometimes it is necessary to respond to a call with a hangup cause and code. For extra fun, some phones will display the hangup cause on the display, which can be a source of amusement. For instance, for a while Kazoo would return a "403 Insert Coin" if the account was out of money.

## Example

```json
{
    "module": "response",
    "data": {
        "code": "486",
        "message": "User Busy",
        "media":"id_or_url"
    }
}
```

If you define `media`, the media will be played to the user before hanging up the call.
