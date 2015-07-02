/*
Section: Callflows
Title: Ring Group
Language: en-US
Version: 3.20
*/

Ring group callflow element allows calling multiple endpoints with given strategy and timeout.

## Example of `data` payload

```
"data": {
    "endpoints": [...],
    "strategy": "strategy",
    "timeout": seconds,
    "repeats": number_of_repeats
}
```

## Mandatory fields
**endpoints** - array of endpoints (see below)

## Optional fields

**strategy** - `single` | `simultaneous` | `weighted_random`, deafult is `simultaneous`
**timeout** - time to call the endpoint before moving further, default is `20`
**repeats** - number of repeats (rounds) this group will be called, default is `1`

### Enpoint format
#### Example

```
[...,
    {
        "id": "id-of-entpoint",
        "endpoint_type": "type of endpoint",
        "weight": weight,
        "timeout": timeout,
        "delay": delay
    },
...
]
```
#### Mandatory fields

**id**

#### Optional fields

**endpoint_type** - `device` | `user` | `group`, default is `device`
**weight** - integer from 1 to 100, used by `weighted_random` strategy
**timeout** - timeout to call the given endpoint, default is `20`
**delay** - delay before this endpoint is called, default is `0`
