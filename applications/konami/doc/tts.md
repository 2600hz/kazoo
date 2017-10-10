## Tts

### About Tts

#### Schema

Use the Text-to-speech engine to say the provided text



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`engine` | What engine to use (system-dependant) | `string()` | `flite` | `false`
`language` | What language is the text in | `string()` |   | `false`
`leg` | What leg to say the text to | `string()` | `self` | `false`
`terminators` | What DTMF can terminate the TTS playback | `array()` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false`
`text` | What to say | `string()` |   | `false`
`voice` | What voice to use when speaking | `string()` | `female` | `false`



