### Callflow Privacy

#### Overview

The `privacy` callflow enables set caller privacy on calls, restricting the presentation some or full parts of Caller ID.

##### Callflow fields

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`privacy_mode` | specifies privacy mode | `string` | `"full"` | `false`

#### Description

By calling this callflow, based on `privacy_mode` both or some part of Caller ID would be made anonymous.

#### Privacy Mode

*`full`*
:   anonymize both CIDName and CIDNumber

*`name`*
:   anonymize CIDName only

*`number`*
:   anonymize CIDNumber only

*`yes`*
:   anonymize both CIDName and CIDNumber
