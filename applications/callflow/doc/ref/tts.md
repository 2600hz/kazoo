## Tts

### About Tts

#### Schema

Validator for the TTS (Text-to-speech) callflow action



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`engine` | What TTS engine to use | `string('flite' | 'ispeech' | 'voicefabric')` |   | `false`
`language` | The language of the speaker | `string()` | `en` | `false`
`terminators.[]` |   | `string()` |   | `false`
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false`
`text` | The text to speak | `string(1..1000)` |   | `true`
`voice` | What voice to use when speaking the text | `string()` | `female` | `false`



