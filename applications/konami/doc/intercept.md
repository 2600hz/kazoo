## Intercept

### About Intercept

#### Schema

Skeleton JSON schema



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`auto_answer` | Auto-answer the leg | `boolean()` | `false` | `false`
`can_call_self` | Can intercept devices of the same targeted user | `boolean()` | `true` | `false`
`target_id` | ID of the target (device or user) | `string()` |   | `false`
`target_type` | Type of entity of the target | `string('device' | 'user' | 'number')` |   | `false`
`unbridged_only` | Only intercept if the a-leg is unbridged | `boolean()` | `true` | `false`



