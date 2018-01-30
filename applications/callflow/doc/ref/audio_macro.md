## Audio Macro

### About Audio Macro

#### Schema

Validator for the audio_macro callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`language` | The language of the speaker | `string()` |   | `false` |  
`macros.[].macro` | the macro type to process | `string('play' | 'prompt' | 'say' | 'tts')` |   | `false` |  
`macros` | The audio macro data | `array(object())` | `[]` | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  

##### callflows.audio_macro.prompt

Validator for playing prompts


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` | Prompt ID | `string()` |   | `false` |  
`language` | The language of the speaker | `string()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  

##### callflows.audio_macro.say

Validator for executing say commands


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`gender` | The gender of the speaker | `string('masculine' | 'feminine' | 'neuter')` |   | `false` |  
`language` | The language of the speaker | `string()` |   | `false` |  
`method` | The method to use when saying the text | `string('none' | 'pronounced' | 'iterated' | 'counted')` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  
`text` | Text to say | `string()` |   | `false` |  
`type` | The type to use when saying the text | `string('number' | 'items' | 'persons' | 'messages' | 'currency' | 'time_measurement' | 'current_date' | 'current_time' | 'current_date_time' | 'telephone_number' | 'telephone_extension' | 'url' | 'ip_address' | 'e-mail_address' | 'postal_address' | 'account_number' | 'name_spelled' | 'name_phonetic' | 'short_date_time')` |   | `false` |  

##### callflows.play

Validator for the play callflow's data object


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`answer` | Whether to answer an unanswered call | `boolean()` |   | `false` |  
`endless_playback` | Loop the media continuously | `boolean()` | `false` | `false` |  
`id` | Media ID or URL of the media to play | `string()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  

##### callflows.tts

Validator for the TTS (Text-to-speech) callflow action


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`engine` | What TTS engine to use | `string('flite' | 'ispeech' | 'voicefabric')` |   | `false` |  
`language` | The language of the speaker | `string()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  
`text` | The text to speak | `string(1..1000)` |   | `true` |  
`voice` | What voice to use when speaking the text | `string()` | `female` | `false` |  



