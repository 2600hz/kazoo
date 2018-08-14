## Resume

### About Resume

Some Konami actions put the other leg on hold (say for a transfer). The *resume* module allows the initiating party to reconnect with the on-hold party (cancelling whatever metaflow module had initiated the hold).

#### Schema

Reconnect the two legs of a call, if possible



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------



### How It Works

Alice has put Bob [on hold](./hold.md) while she is away from her desk. She returns and uses her *resume* metaflow to reconnect immediately with Bob.

### Configure the metaflow

The *resume* module should be placed under the "numbers" key in the "metaflows" object. Konami will attempt to re-connect her with the other party (and will fail if the other party has hung up already).

```json
    "metaflows":{
        "numbers":{
            "7":{
                "module":"resume"
                ,"data":{}
            }
        }
        ,"patterns":{...}
        ,"binding_key":"*"
    }
```
