## Acdc Queue

### About Acdc Queue

Allows an agent/manager to temporarily login to a queue

#### Schema

Validator for the acdc_queue callflow data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | What action to perform on the agent for this queue | `string('login' | 'logout')` |   | `false`
`id` | Queue ID | `string()` |   | `false`



