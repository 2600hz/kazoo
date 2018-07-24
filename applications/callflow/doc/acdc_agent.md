## Acdc Agent

### About Acdc Agent

Toggle the ACDc agent status

#### Schema

Validator for the acdc_agent callflow data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Set which action to perform | `string('login' | 'logout' | 'paused' | 'resume')` |   | `false`
`presence_id` | Static presence ID to send BLF updates to | `string()` |   | `false`
`presence_state` | Custom presence state to send | `string('early' | 'confirmed' | 'terminated' | 'red_flash' | 'red_solid' | 'green')` |   | `false`
`timeout` | Time, in seconds, for 'paused' state | `integer()` |   | `false`



