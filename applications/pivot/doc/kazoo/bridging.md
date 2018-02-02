# Bridging

Kazoo JSON offers a plethora of ways to call out to various endpoints! Below are some minimalist examples to whet your appetite. Most take more options, are nestable as children of other callflow actions, and are generally quite useful in accomplishing most peoples' needs.

## Devices

Dial a single Kazoo device

```json
{
    "module":"device",
    "data":{"id":"device_id"}
}
```

## Users

Dial a Kazoo user (any devices owned by the user)

```json
{
    "module": "user",
    "data": {
        "id": "user_id"
    }
}
```

## Ring Group

Ring groups are ultra-flexible in what types of endpoints you can combine: devices, users, or groups! You need only include the IDs you want to ring and Kazoo will build the appropriate list of endpoints.

```json
{
    "module":"ring_group",
    "data":{
        "endpoints": ["device_1_id",
                      "device_2_id",
                      "user_1_id",
                      "user_2_id",
                      "group_1_id",
                      "group_2_id"
                     ]
    }
}
```

You are free to mix/match devices, users, and groups based on the needs of this particular call.

## Dialing outside the account

It is all well and good that dialing to known Kazoo endpoints is so easy, but what about contacting the outside world?

Kazoo supports two types of resources, global and per-account (or local, as Kazoo refers to them). You can optionally route to either, depending on how you've configured your account and whether you utilize the Kazoo cluster's global resources.

The only real difference is the `use_local_resources` flag.

### Using Global resources

```json
{
    "module": "resources",
    "data": {
        "to_did": "+14155550000",
        "use_local_resources": false
    }
}
```

### Using Local resources

```json
{
    "module": "resources",
    "data": {
        "to_did": "+14155550000",
        "use_local_resources": true
    }
}
```
