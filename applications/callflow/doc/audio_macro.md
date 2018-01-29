## Audio Macro

### About Audio Macro

Sometimes you need to group media files together to act as a single logical audio file. Any remaining macros will be skipped if DTMF terminators are pressed.

#### Schema

Validator for the audio_macro callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`language` | The language of the speaker | `string()` |   | `false` |  
`macro` | The audio macro data | `array(object())` | `[]` | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  



