### Callflows

#### About Callflows

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`featurecode` | When the callflow is used as a featurecode this object tracks the intended match of the pattern and name of the feature | `object` |   | `false`
`featurecode.name` |   | `string(1..128)` |   | `false`
`featurecode.number` |   | `string(1..30)` |   | `false`
`flow` | A callflow node defines a module to execute, data to provide to that module, and one or more children to branch to | `object` |   | `true`
`flow.children` | Children callflows | `object` | `{}` | `false`
`flow.data` | The data/arguments of the callflow module | `object` | `{}` | `true`
`flow.module` | The name of the callflow module to excute at this node | `string(1..64)` |   | `true`
`metaflow` | Actions applied to a call outside of the normal callflow, initiated by the caller(s) | [#/definitions/metaflows](#metaflows) |   | `false`
`numbers` | A list of static numbers that the callflow should execute for | `array(string(1..36))` | `[]` | `false`
`numbers.[]` |   | `string` |   | `false`
`patterns` | A list of regular expressions that the callflow should execute for, with optional capture groups | `array(string(1..))` | `[]` | `false`
`patterns.[]` |   | `string` |   | `false`


##### metaflow

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### metaflow.audio_level

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string` |   | `true`
`data.level` |   | `string` |   | `false`
`data.mode` |   | `string` |   | `false`
`module` |   | `string('audio_level')` |   | `true`

##### metaflow.break

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('break')` |   | `true`

##### metaflow.callflow

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.id` |   | `string` |   | `true`
`module` |   | `string('callflow')` |   | `true`

##### metaflow.hangup

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('hangup')` |   | `true`

##### metaflow.hold

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.moh_aleg` |   | `string` |   | `false`
`data.moh_bleg` |   | `string` |   | `false`
`data.unhold_key` |   | `string` | `1` | `false`
`module` |   | `string('hold')` |   | `true`

##### metaflow.hold_control

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`data.action` |   | `string('hold', 'unhold', 'toggle')` | `toggle` | `false`
`module` |   | `string('hold_control')` |   | `true`

##### metaflow.intercept

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.auto_answer` |   | `boolean` | `false` | `false`
`data.target_id` |   | `string` |   | `true`
`data.target_type` |   | `string('device', 'user', 'number')` |   | `true`
`data.unbridged_only` |   | `boolean` | `true` | `false`
`module` |   | `string('intercept')` |   | `true`

##### metaflow.move

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.auto_answer` |   | `boolean` | `false` | `false`
`data.device_id` |   | `string` |   | `false`
`data.owner_id` |   | `string` |   | `false`
`module` |   | `string('move')` |   | `true`

##### metaflow.play

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.id` |   | `string` |   | `true`
`data.leg` |   | `string('both', 'self', 'peer')` | `both` | `false`
`module` |   | `string('play')` |   | `true`

##### metaflow.record_call

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string('start', 'stop', 'toggle')` | `toggle` | `true`
`data.format` |   | `string` |   | `false`
`data.media_name` |   | `string` |   | `false`
`module` |   | `string('record_call')` |   | `true`

##### metaflow.resume

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('resume')` |   | `true`

##### metaflow.say

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.language` |   | `string` |   | `false`
`data.method` |   | `string` |   | `false`
`data.text` |   | `string` |   | `true`
`data.type` |   | `string` |   | `false`
`module` |   | `string('say')` |   | `true`

##### metaflow.sound_touch

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string('start', 'stop')` |   | `true`
`data.adjust_in_octaves` |   | `integer` |   | `false`
`data.adjust_in_semitones` |   | `integer` |   | `false`
`data.hook_dtmf` |   | `boolean` | `false` | `false`
`data.pitch` |   | `integer` |   | `false`
`data.rate` |   | `integer` |   | `false`
`data.sending_leg` |   | `boolean` | `false` | `false`
`data.tempo` |   | `integer` |   | `false`
`module` |   | `string('sound_touch')` |   | `true`

##### metaflow.transfer

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.Transfer-Type` |   | `string` | `attended` | `false`
`data.captures` |   | `string` |   | `false`
`data.target` |   | `string` |   | `false`
`module` |   | `string('transfer')` |   | `true`

##### metaflow.tts

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.engine` |   | `string` | `flite` | `false`
`data.language` |   | `string` |   | `false`
`data.leg` |   | `string` | `self` | `false`
`data.terminators` |   | `string` |   | `false`
`data.text` |   | `string` |   | `true`
`data.voice` |   | `string` | `female` | `false`
`module` |   | `string('tts')` |   | `true`

##### metaflow_children

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### metaflows

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#')` | `*` | `false`
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer` |   | `false`
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both', 'self', 'peer')` |   | `false`
`numbers` | A list of static numbers with their flows | `object` |   | `false`
`numbers./^[0-9]+$/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`patterns` | A list of patterns with their flows | `object` |   | `false`
`patterns./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/callflows

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/callflows/{CALLFLOW_ID}
```

