## Say

### About Say

#### Schema

Say the provided text



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`gender` | The gender of the voice to say the text | `string('feminine' | 'masculine' | 'neuter')` |   | `false`
`language` | Language of the text | `string()` |   | `false`
`method` | Toggle the way the text is read to the caller | `string('pronounced' | 'iterated' | 'counted')` |   | `false`
`text` | The text to say to the caller | `string()` |   | `false`
`type` | The type of text to say | `string('account_number' | 'currency' | 'current_date' | 'current_date_time' | 'current_time' | 'email_address' | 'ip_address' | 'items' | 'messages' | 'name_phonetic' | 'name_spelled' | 'number' | 'persons' | 'postal_address' | 'short_date_time' | 'telephone_extension' | 'telephone_number' | 'time_measurement' | 'url')` |   | `false`



