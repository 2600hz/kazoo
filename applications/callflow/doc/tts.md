## TTS

### About TTS

#### Schema

Validator for the TTS (Text-to-speech) callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`endless_playback` | Loop the media until canceled | `boolean()` |   | `false` |  
`engine` | What TTS engine to use | `string('flite' | 'google' | 'ispeech' | 'voicefabric')` |   | `false` |  
`language` | The language of the speaker | `string()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  
`text` | The text to speak | `string(1..1000)` |   | `true` |  
`voice` | What voice to use when speaking the text | `string()` | `female` | `false` |  



