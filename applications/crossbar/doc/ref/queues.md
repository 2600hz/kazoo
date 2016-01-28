### Queues

#### About Queues

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`agent_ring_timeout` | In seconds, how long to ring an agent before progressing to the next agent available | `integer` | `15` | `false`
`agent_wrapup_time` | Pre-defined wait period applied after an agent handles a customer call | `integer` | `0` | `false`
`announce` | Media ID (or appropriate media URI) of media to play when caller is about to be connected. | `string` |   | `false`
`caller_exit_key` | Key caller can press while on hold to exit the queue and continue in the callflow | `string` | `#` | `false`
`cdr_url` | An optional HTTP URL to POST the CDR | `string` |   | `false`
`connection_timeout` | In seconds, how long to try to connect the caller before progressing past the queue callflow action | `integer` | `3600` | `false`
`enter_when_empty` | Allows a caller to enter a queue and wait when no agents are available | `boolean` | `true` | `false`
`max_priority` | Maximum possible priority level queue will support. Can not be redefined for existing queue. | `integer` |   | `false`
`max_queue_size` | How many callers are allowed to wait on hold in the queue (0 for no limit) | `integer` | `0` | `false`
`moh` | Media ID (or appropriate media URI) of media to play while caller is on hold. | `string` |   | `false`
`name` | A friendly name for the queue | `string` |   | `true`
`record_caller` | When enabled, a caller's audio will be recorded | `boolean` | `false` | `false`
`recording_url` | An optional HTTP URL to PUT the call recording after the call ends (and should respond to GET for retrieving the audio data) | `string` |   | `false`
`ring_simultaneously` | The number of agents to try in parallel when connecting a caller | `integer` | `1` | `false`
`strategy` | The queue strategy for connecting agents to callers | `string` | `round_robin` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/queues

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/queues

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/queues/{QID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/queues/{QID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/queues/{QID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/queues/{QID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/queues/eavesdrop

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/eavesdrop
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/queues/stats

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/stats
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/queues/{QID}/eavesdrop

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}/eavesdrop
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/queues/{QID}/roster

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}/roster
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/queues/{QID}/roster

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}/roster
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/queues/{QID}/roster

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/queues/{QID}/roster
```

