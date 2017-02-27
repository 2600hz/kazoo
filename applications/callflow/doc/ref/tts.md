## Tts

### About Tts

### Schema

Validator for the TTS (Text-to-speech) callflow action

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`engine` | What TTS engine to use | `string('flite', 'ispeech')` |   | `false`
`language` | The language of the speaker | `string` | `en` | `false`
`text` | The text to speak | `string(1..1000)` |   | `true`
`voice` | What voice to use when speaking the text | `string` | `female` | `false`
