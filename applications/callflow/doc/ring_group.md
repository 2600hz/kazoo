
Ring group callflow element allows calling multiple endpoints with given strategy and timeout.

## Example of `data` payload

    "data": {
        "endpoints": [...]
        ,"strategy": "{STRATEGY}"
        ,"timeout": {SECONDS}
        ,"repeats": {REPEAT}
        ,"ringback": "{RINGBACK}"
        ,"ignore_forward": boolean()
    }

## Mandatory fields
**endpoints** - array of endpoints (see below)

## Optional fields

* **{STRATEGY}**: default is `simultaneous`
    * `single` - ring one endpoint after another
    * `simultaneous` - ring all endpoints at the same time
    * `weighted_random` - randomize the list of endpoints, then use `single` strategy
* **{TIMEOUT}** - time to call the endpoint before moving further, default is `20`
* **{REPEAT}** - number of repeats (rounds) this group will be called, default is `1`
* **{RINGBACK}** - ringback to use, if any
* **{IGNORE\_FORWARD}** - Whether to ignore forwarded endpoints, defaults to `true`

### Endpoint format
#### Example

    {"id": "{ENDPOINT_ID}"
     ,"endpoint_type": "{ENDPOINT_TYPE}"
     ,"weight": {WEIGHT}
     ,"timeout": {TIMEOUT}
     ,"delay": {DELAY}
    }

#### Mandatory fields

**id**: Endpoint ID

#### Optional fields

* **{ENDPOINT\_TYPE}**: default is `device`
    * `device`
    * `user`
    * `group`
* **{WEIGHT}** - integer from 1 to 100, used by `weighted_random` strategy
* **{TIMEOUT}** - timeout to call the given endpoint, default is `20`
* **{DELAY}** - delay before this endpoint is called, default is `0`
