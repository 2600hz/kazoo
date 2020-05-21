# Queues

## About Queues

#### Schema

Call Queues - FIFO call queues for serializing callers connecting to agents



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`agent_ring_timeout` | In seconds, how long to ring an agent before progressing to the next agent available | `integer()` | `15` | `false` |  
`agent_wrapup_time` | Pre-defined wait period applied after an agent handles a customer call | `integer()` | `0` | `false` |  
`announce` | Media ID (or appropriate media URI) of media to play when caller is about to be connected. | `string()` |   | `false` |  
`announcements.interval` | Time between announcements | `integer()` | `30` | `false` |  
`announcements.media.in_the_queue` | Played after the numeric position | `string()` |   | `true` |  
`announcements.media.increase_in_call_volume` | Played if the estimated wait time has increased since the previous wait time announcement | `string()` |   | `true` |  
`announcements.media.the_estimated_wait_time_is` | Played before the estimated wait time media | `string()` |   | `true` |  
`announcements.media.you_are_at_position` | Played before the numeric position | `string()` |   | `true` |  
`announcements.media` | Custom prompts to be played for the announcements | `object()` |   | `false` |  
`announcements.position_announcements_enabled` | Whether announcements of the caller's position in the queue should be played | `boolean()` |   | `false` |  
`announcements.wait_time_announcements_enabled` | Whether announcements of the estimated wait time in the queue should be played | `boolean()` |   | `false` |  
`announcements` | Configuration for periodic announcements to callers waiting in the queue | `object()` |   | `false` |  
`caller_exit_key` | Key caller can press while on hold to exit the queue and continue in the callflow | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '*' | '0' | '#')` | `#` | `false` |  
`cdr_url` | An optional HTTP URL to POST the CDR | `string()` |   | `false` |  
`connection_timeout` | In seconds, how long to try to connect the caller before progressing past the queue callflow action | `integer()` | `3600` | `false` |  
`enter_when_empty` | Allows a caller to enter a queue and wait when no agents are available | `boolean()` | `true` | `false` |  
`max_priority` | Maximum possible priority level queue will support. Can not be redefined for existing queue. | `integer()` |   | `false` |  
`max_queue_size` | How many callers are allowed to wait on hold in the queue (0 for no limit) | `integer()` | `0` | `false` |  
`moh` | Media ID (or appropriate media URI) of media to play while caller is on hold. | `string()` |   | `false` |  
`name` | A friendly name for the queue | `string(1..128)` |   | `true` |  
`record_caller` | When enabled, a caller's audio will be recorded | `boolean()` | `false` | `false` |  
`recording_url` | An optional HTTP URL to PUT the call recording after the call ends (and should respond to GET for retrieving the audio data) | `string()` |   | `false` |  
`ring_simultaneously` | The number of agents to try in parallel when connecting a caller | `integer()` | `1` | `false` |  
`strategy` | The queue strategy for connecting agents to callers | `string('round_robin' | 'most_idle')` | `round_robin` | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/queues

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/queues/eavesdrop

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/eavesdrop
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/stats_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/stats
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/eavesdrop

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/eavesdrop
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/stats_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
```

