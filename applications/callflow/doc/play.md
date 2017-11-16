## Play

### About Play

Play media to the caller

#### Schema

Validator for the play callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`answer` | Whether to answer an unanswered call | `boolean()` |   | `false`
`endless_playback` | Loop the media continuously | `boolean()` | `false` | `false`
`id` | Media ID or URL of the media to play | `string()` |   | `false`
`terminators.[]` |   | `string()` |   | `false`
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false`



