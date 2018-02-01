## Audio Macro

### About Audio Macro

Sometimes you need to group media files together to act as a single logical audio file. Any remaining macros will be skipped if DTMF terminators are pressed.

There are four types of media that can be included:
1. `play`: same as the `play` callflow action, a media file or HTTP URL, to play to the caller
2. `prompt`: play a Kazoo system prompt (or the account's overridden version)
3. `say`: a way to say certain phrases (like pronouncing each digit in a number)
4. `tts`: play the text as speech

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
`endless_playback` | Loop the media until cancelled | `boolean()` |   | `false` |  
`engine` | What TTS engine to use | `string('flite' | 'ispeech' | 'voicefabric')` |   | `false` |  
`language` | The language of the speaker | `string()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  
`text` | The text to speak | `string(1..1000)` |   | `true` |  
`voice` | What voice to use when speaking the text | `string()` | `female` | `false` |  



